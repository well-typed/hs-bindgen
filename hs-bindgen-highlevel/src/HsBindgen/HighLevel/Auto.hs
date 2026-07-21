{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TupleSections      #-}

-- | Write a spec's ordinary combinators for it, reading them off the high-level type
-- signature.
--
-- A spec has one combinator per C argument, but most of those combinators hold no
-- decision: an @int@ the signature takes as an 'Int', a @double@ it takes as a
-- 'Double', a return value converted the obvious way. 'auto' writes all of them,
-- reading each one off the high-level type signature, so the only combinators left
-- written out are the ones that needed a decision.
--
-- When no combinator needs one, the whole spec is 'auto':
--
-- > -- int sodium_library_version_major(void);
-- > major :: IO Int
-- > major = toHighLevel sodium_library_version_major auto
--
-- == The family
--
-- Few bindings are uniform throughout, so 'auto' comes with variants that hand part
-- of the decision back. They differ in two things: whether they fill the high-level
-- arguments, and how they close.
--
-- > combinator             fills the arguments   closes with                        ends the spec
-- > auto                   yes                   the obvious result                 yes
-- > autoWith f             yes                   resultPure f                       yes
-- > autoChecked check      yes                   the outputs, once check passes     yes
-- > autoMaybe accept       yes                   the outputs in Just, else Nothing  yes
-- > autoInputs rest        yes                   whatever rest is                   no
-- > autoResult             no                    the obvious result                 yes
-- > checkedResult check    no                    the outputs, once check passes     yes
-- > maybeResult accept     no                    the outputs in Just, else Nothing  yes
-- > eitherResult classify  no                    the outputs in Right, else Left e  yes
--
-- __Every combinator whose \"ends the spec\" column says yes
-- closes the spec, so it has to be written last.__ Nothing can follow it: an
-- 'HsBindgen.HighLevel.output' or 'HsBindgen.HighLevel.fixed' that C takes /after/ the
-- arguments the high-level type exposes has to go above it, or the spec needs
-- 'autoInputs' and a closer written by hand.
--
-- The four closers that fill no arguments ('autoResult', 'checkedResult', 'maybeResult'
-- and 'eitherResult') end a spec whose combinators are all written out. The rest begin
-- wherever you put them and fill the high-level type's remaining arguments from there.
--
-- There are three merged combinators rather than one per closer because 'autoInputs'
-- composes with any closer at all: @'autoInputs' '$' 'eitherResult' classify@ is a
-- perfectly good spec, and so is @'autoInputs' '$' theCloserYouWrote@. The three
-- exist only because their shapes come up often enough to deserve a word.
--
-- Reach for 'auto' first. The sections below cover how it decides, what it leaves to
-- you, when one of the variants is the one you want instead, and how to name a
-- convention of your own as a combinator.
--
-- == How it decides
--
-- 'auto' walks two types side by side: the high-level type, taken from its signature,
-- and whatever is left of the C function's type. At each step it compares the two
-- heads. Take @double hypot(double, double)@, imported as
-- @c_hypot :: CDouble -> CDouble -> IO CDouble@, and a binding
-- @hsHypot :: Double -> Double -> IO Double@ defined as @toHighLevel c_hypot auto@:
--
-- > step   high-level type left            C type left                        auto does
-- >  1     Double -> Double -> IO Double   CDouble -> CDouble -> IO CDouble   fill CDouble from Double
-- >  2     Double -> IO Double             CDouble -> IO CDouble              fill CDouble from Double
-- >  3     IO Double                       IO CDouble                         close: CDouble to Double
--
-- There are three cases, one 'Auto' instance each, and that is the whole rule:
--
--   * the high-level type still takes an argument, so fill the C arguments it covers
--     with that type's 'HsBindgen.HighLevel.Defaults.defaultIn' and carry on;
--   * both sides have reached the result, so close the spec (see 'autoResult');
--   * the high-level type has run out of arguments but C has not, which is an error:
--     nothing in the signature accounts for that leftover C argument.
--
-- A Haskell argument does not have to line up one-for-one with a C argument. A
-- 'Data.ByteString.ByteString' argument fills a @(pointer, length)@ pair, so a single
-- walk step can consume two C arguments at once. The walk is driven by the high-level
-- signature, and C arguments are consumed to match.
--
-- == What it leaves to you
--
-- 'auto' never writes an 'HsBindgen.HighLevel.output' or a
-- 'HsBindgen.HighLevel.scratch', because it has nothing to go on. A C
-- @int *@ argument might be a pointer C reads, an out-parameter C writes,
-- or an array, and nothing in the type says which; a scratch buffer's length is not
-- in the type at all. You write those combinators and 'auto' takes the rest:
--
-- > encodeText
-- >   :: String -> Ecc -> Int -> Int -> Mask -> Bool -> IO (IncompleteArray Word8, Bool)
-- > encodeText = toHighLevel qrcodegen_encodeText
-- >            $ input defaultIn      -- text
-- >            $ scratchArray maxLen  -- tempBuffer: C writes it, we never read it
-- >            $ output qrCodeOut     -- qrcode: the out-parameter we keep
-- >            $ auto                 -- ecl, minVersion, maxVersion, mask, boostEcl,
-- >                                   --   then the (qrcode, ok) result
--
-- 'auto' runs to the end of the spec once it starts, so it always comes last. An
-- ordinary input that sits /before/ an explicit combinator, like @text@ here, is
-- written out as @input defaultIn@: one line, saying the same thing.
--
-- == Write the signature first
--
-- Everything 'auto' fills is read from the high-level type signature, so the
-- signature has to be there. Without one there is nothing to read, and the error is
-- an ambiguous type variable rather than anything about marshalling.
--
-- That same property makes 'auto' the right __placeholder while you are still
-- writing a spec__. Unlike @undefined@ it constrains the combinators above it, so the
-- holes you are working on come back with concrete types. See \"Building against
-- typed holes\" in "HsBindgen.HighLevel".
--
-- == When something does not line up
--
-- @No default input marshaller for type ...@: 'auto' reached an argument whose type
-- has no default. Write that argument out with an explicit @input@, or give the type
-- a 'HsBindgen.HighLevel.Defaults.DefaultIn' instance.
--
-- @auto cannot line the high-level type up with the C function@: the third case
-- above. A C argument is left over that the high-level type does not expose, so give
-- it an explicit 'HsBindgen.HighLevel.output' or 'HsBindgen.HighLevel.scratch', or add
-- the missing argument to the signature.
--
-- @auto cannot assemble this result@: the outputs and the C return do not add up to
-- the result type in the signature. See 'autoResult' for the shape it builds.
--
-- @auto cannot assemble this result from the out-parameters alone@: the same, where
-- the C return contributed nothing because the call returns @void@ or a closer
-- consumed its status. See 'AutoOutputs'.
--
module HsBindgen.HighLevel.Auto (
    -- * Filling a spec automatically
    auto
  , autoInputs
    -- ** Filling the inputs and closing in one word
  , autoWith
  , autoChecked
  , autoMaybe
    -- * Closing a spec automatically
  , autoResult
    -- ** Closing on a status
    -- $status
  , checkedResult
  , maybeResult
  , eitherResult
    -- * Naming a convention of your own
    -- $vocabulary
    -- * Machinery
    --
    -- | The classes and type families the combinators above are built from. Name
    -- them when writing a combinator of your own that is polymorphic in the spec it
    -- completes; a spec that only uses the combinators never does.
    --
    -- 'Auto' and 'AutoInputs' appear here without their methods because those methods
    -- are 'auto' and 'autoInputs', already exported above. The other two carry theirs,
    -- since a closer of your own may want 'autoOutputs'.
  , Auto
  , AutoInputs
  , AutoOutputs (..)
  , AutoResult
  , AutoResultAt (..)
  , IsUnit
  ) where

import Data.Kind (Constraint, Type)
import GHC.TypeLits (TypeError)

import HsBindgen.HighLevel (ApplyOutputs, AssembleOutputs, ToHighLevel, inputN,
                            resultIO, resultPure)
import HsBindgen.HighLevel.Defaults (DefaultIn (..), DefaultRes (..))
import HsBindgen.HighLevel.Internal.Errors (AutoMismatch, AutoOutputsMismatch,
                                            AutoResultMismatch, unreachable)
import HsBindgen.HighLevel.Internal.Spec (Outputs (..), ToHighLevel (..))
import HsBindgen.HighLevel.Internal.Threading (ThreadIn)

{-------------------------------------------------------------------------------
  Assembling the outputs alone
-------------------------------------------------------------------------------}

-- | @AutoOutputs os hs@ says: __the outputs @os@ assemble into the result @hs@ on
-- their own__, with the C return value contributing nothing.
--
-- > outputs   high-level result
-- > none      ()
-- > a         a
-- > a, b      (a, b)
-- > a, b, c   (a, b, c)
--
-- A single output is not wrapped in a tuple. Up to five are covered.
--
-- Two situations produce it. A @void@-returning call has no return value to
-- contribute, and a call whose return value is a /status/ has none left once a
-- closer has consumed it, so 'checkedResult', 'maybeResult' and 'eitherResult' all
-- assemble their results this way. A call that returns @int@ and one that returns
-- @void@ therefore land on the same result:
--
-- > void f(int *a, int *b);   -- IO (Int, Int)
-- > int  g(int *a, int *b);   -- IO (Int, Int) too, once the int is checked away
--
class AutoOutputs (os :: [Type]) hs where
  -- | The collected values, assembled. A closer of your own runs the deferred
  -- read-backs and then applies this: @'autoOutputs' '<$>' pending@ is what
  -- 'checkedResult' and its siblings do once their verdict has come in.
  autoOutputs :: Outputs os -> hs

{-
  The result's *components* are matched in the head, so a wrong number of them falls
  through to the AutoOutputsMismatch instance rather than producing a raw mismatch,
  while their *identities* sit in the context, so the high-level signature fixes the
  outputs' Haskell types rather than the other way round. Moving an equality from a
  context into a head breaks inference for a polymorphic unmarshaller; moving one the
  other way stops AutoOutputsMismatch from ever firing.
-}

instance AutoOutputs '[] () where
  autoOutputs NoOutputs = ()
  {-# INLINE autoOutputs #-}
instance h1 ~ o1 => AutoOutputs '[o1] h1 where
  autoOutputs (o1 :* NoOutputs) = o1
  {-# INLINE autoOutputs #-}
instance (h1 ~ o1, h2 ~ o2) => AutoOutputs '[o2, o1] (h1, h2) where
  autoOutputs (o2 :* o1 :* NoOutputs) = (o1, o2)
  {-# INLINE autoOutputs #-}
instance (h1 ~ o1, h2 ~ o2, h3 ~ o3)
      => AutoOutputs '[o3, o2, o1] (h1, h2, h3) where
  autoOutputs (o3 :* o2 :* o1 :* NoOutputs) = (o1, o2, o3)
  {-# INLINE autoOutputs #-}
instance (h1 ~ o1, h2 ~ o2, h3 ~ o3, h4 ~ o4)
      => AutoOutputs '[o4, o3, o2, o1] (h1, h2, h3, h4) where
  autoOutputs (o4 :* o3 :* o2 :* o1 :* NoOutputs) = (o1, o2, o3, o4)
  {-# INLINE autoOutputs #-}
instance (h1 ~ o1, h2 ~ o2, h3 ~ o3, h4 ~ o4, h5 ~ o5)
      => AutoOutputs '[o5, o4, o3, o2, o1] (h1, h2, h3, h4, h5) where
  autoOutputs (o5 :* o4 :* o3 :* o2 :* o1 :* NoOutputs) = (o1, o2, o3, o4, o5)
  {-# INLINE autoOutputs #-}

-- Fallback: the outputs and the high-level result type do not line up. Reached only
-- once hs is known, since every instance above is general in it.
instance {-# OVERLAPPABLE #-} TypeError (AutoOutputsMismatch os hs)
      => AutoOutputs os hs where
  autoOutputs = unreachable

{-------------------------------------------------------------------------------
  Closing a spec automatically
-------------------------------------------------------------------------------}

-- | Does the C function return @void@? A @void@ return is imported as @IO ()@, so
-- this asks whether the C return type is @()@. It is what 'AutoResult' dispatches on,
-- and the only reason 'AutoResultAt' is indexed by a 'Bool'.
type IsUnit :: Type -> Bool
type family IsUnit c where
  IsUnit () = 'True
  IsUnit c  = 'False

-- | @AutoResult os c hs@ says: __the outputs @os@ and the C return value @c@ assemble
-- into the result @hs@__, by the rule 'autoResult' describes.
--
-- Written as the constraint the binding's own types produce, so the @void@ / non-@void@
-- split that 'AutoResultAt' dispatches on is computed rather than spelled out. Where
-- the C return contributes nothing, 'AutoOutputs' is the constraint to reach for
-- instead; this one is for a closer that must work at any C return type.
type AutoResult :: [Type] -> Type -> Type -> Constraint
type AutoResult os c hs = AutoResultAt (IsUnit c) os c hs

-- | Close a spec by building its result the obvious way: __every output in spec
-- order, then the converted C return value, unless the C function returns @void@__.
--
-- > C returns   outputs   high-level result
-- > void        none      IO ()
-- > void        a         IO a
-- > void        a, b      IO (a, b)
-- > int         none      IO Int
-- > int         a         IO (a, Int)
-- > int         a, b      IO (a, b, Int)
--
-- A single component is not wrapped in a tuple, and the C return is converted by
-- 'HsBindgen.HighLevel.Defaults.DefaultRes', so an @int@ closes to 'Int' if the
-- signature says 'Int' and stays a @CInt@ if it says @CInt@. Up to five outputs are
-- covered. The @void@ half of that table is 'AutoOutputs'.
--
-- 'auto' ends with this. Reach for 'autoResult' on its own when the inputs need
-- explicit marshallers but the result is that plain shape:
--
-- > hsParseInt :: String -> IO (Int, Int)
-- > hsParseInt = toHighLevel c_parse_int
-- >            $ input withCStringIn
-- >            $ output peekIntOut
-- >            $ autoResult -- (the output, the converted return)
--
-- For any other result shape, a record, a 'Maybe', a status the binding checks
-- rather than returns, write the closer yourself:
-- 'HsBindgen.HighLevel.resultPure' and 'HsBindgen.HighLevel.resultIO' take a function
-- of exactly this arity and may return anything.
--
-- __The rule is positional, so it cannot tell two components of the same type
-- apart.__ For @void f(int *value, int *status)@ and @hsF :: IO (Int, Int)@ this
-- builds @(value, status)@. An author who meant @(status, value)@ gets a wrapper that
-- compiles, runs, and is wrong, because nothing in the types distinguishes the two.
-- Where result components share a Haskell type and their order is not obvious from
-- the C signature, write the closer out, which names each one:
--
-- > output valueOut $ output statusOut $ resultPure (\value status _ -> (status, value))
--
autoResult
  :: forall os c hs.
    AutoResult os c hs
  => ToHighLevel os (IO c) (IO hs)
autoResult = autoResultAt @(IsUnit c)
{-# INLINE autoResult #-}

-- | The table behind 'autoResult', indexed by whether the C return is @void@. Name
-- 'AutoResult' instead, which computes that index; this is only where the two halves
-- of the table are written down.
class AutoResultAt (unit :: Bool) (os :: [Type]) c hs where
  autoResultAt :: ToHighLevel os (IO c) (IO hs)

-- A void return contributes nothing, so the result is the outputs alone. The C type
-- is written () rather than a variable: IsUnit c ~ 'True already forces it, and
-- spelling it out stops a hand-written @AutoResultAt 'True os c hs@ constraint from
-- matching at a non-void c and silently dropping its return value.
instance AutoOutputs os hs => AutoResultAt 'True os () hs where
  autoResultAt = onReturn (\_ pending -> autoOutputs <$> pending)
  {-# INLINE autoResultAt #-}

-- A non-void return is the last component, converted by 'DefaultRes'.
--
-- This table and 'AutoOutputs' are paired: each row here is the 'AutoOutputs' row for
-- the same outputs with one more component on the end. Raising the maximum arity means
-- adding a row to both. They cannot be derived from one another, because "a k-tuple
-- with one more component" is itself a relation that would have to enumerate k.
instance DefaultRes c hs => AutoResultAt 'False '[] c hs where
  autoResultAt = resultIO defaultResConv
  {-# INLINE autoResultAt #-}
instance (h1 ~ o1, DefaultRes c h2)
      => AutoResultAt 'False '[o1] c (h1, h2) where
  autoResultAt = resultIO $ \o1 c -> (o1,) <$> defaultResConv c
  {-# INLINE autoResultAt #-}
instance (h1 ~ o1, h2 ~ o2, DefaultRes c h3)
      => AutoResultAt 'False '[o2, o1] c (h1, h2, h3) where
  autoResultAt = resultIO $ \o1 o2 c -> (o1, o2,) <$> defaultResConv c
  {-# INLINE autoResultAt #-}
instance (h1 ~ o1, h2 ~ o2, h3 ~ o3, DefaultRes c h4)
      => AutoResultAt 'False '[o3, o2, o1] c (h1, h2, h3, h4) where
  autoResultAt = resultIO $ \o1 o2 o3 c -> (o1, o2, o3,) <$> defaultResConv c
  {-# INLINE autoResultAt #-}
instance (h1 ~ o1, h2 ~ o2, h3 ~ o3, h4 ~ o4, DefaultRes c h5)
      => AutoResultAt 'False '[o4, o3, o2, o1] c (h1, h2, h3, h4, h5) where
  autoResultAt = resultIO $ \o1 o2 o3 o4 c -> (o1, o2, o3, o4,) <$> defaultResConv c
  {-# INLINE autoResultAt #-}
instance (h1 ~ o1, h2 ~ o2, h3 ~ o3, h4 ~ o4, h5 ~ o5, DefaultRes c h6)
      => AutoResultAt 'False '[o5, o4, o3, o2, o1] c (h1, h2, h3, h4, h5, h6) where
  autoResultAt = resultIO $ \o1 o2 o3 o4 o5 c ->
                   (o1, o2, o3, o4, o5,) <$> defaultResConv c
  {-# INLINE autoResultAt #-}

-- Fallback: the outputs, the C return and the high-level result type do not line up.
-- Reached only once hs is known, since every instance above is general in it.
instance {-# OVERLAPPABLE #-} TypeError (AutoResultMismatch os c hs)
      => AutoResultAt 'False os c hs where
  autoResultAt = unreachable

{-------------------------------------------------------------------------------
  Closing on a status
-------------------------------------------------------------------------------}

-- | Run the C call, then decide what to do with the out-parameters, which have not
-- been read yet.
--
-- The one place the closers below get their ordering from, and the reason they can
-- promise something 'HsBindgen.HighLevel.resultIO' cannot: its assembler receives
-- outputs that have already been read, whereas the continuation here receives
-- @pending@ and may decline to run it. Every closer that rejects a status without
-- peeking a slot C never wrote is this function plus a verdict.
onReturn ::
     (c -> IO (Outputs os) -> IO r) -- ^ the C return value, and the deferred read-backs
  -> ToHighLevel os (IO c) (IO r)
onReturn decide = ToHighLevel $ \pending cFn -> cFn >>= \c -> decide c pending
{-# INLINE onReturn #-}

-- | Close a spec whose C return value is a __status guarding the out-parameters__
-- rather than part of the result: run @check@ on it, then assemble the outputs
-- exactly as 'autoResult' would for a @void@-returning call.
--
-- This is the shape most of a C library has. A call reports success or failure in
-- its return value and delivers the answer through @T *out@ parameters, so the
-- status belongs in the check and never in the result:
--
-- > keypair :: IO (PublicKey, SecretKey)
-- > keypair = toHighLevel crypto_sign_keypair
-- >         $ output publicKeyOut
-- >         $ output secretKeyOut
-- >         $ checkedResult (throwUnlessZero (SodiumError "crypto_sign_keypair"))
--
-- __The check runs before the read-backs.__ That ordering is the reason this is a
-- combinator rather than something you can write with 'HsBindgen.HighLevel.resultIO',
-- whose assembler by construction receives outputs that have already been read. When
-- a call fails it typically leaves its out-parameters untouched, so reading them back
-- is at best meaningless and at worst unsafe: an out-parameter unmarshaller that
-- attaches a finaliser (see @outForeignPtr@) would register a free for a pointer C
-- never wrote. Throwing from @check@ skips every read-back and unwinds the
-- brackets, so nothing is peeked.
--
-- The same ordering matters when a read-back /releases/ something rather than only
-- reading it, as one that copies a string out and then frees it does: on a rejected
-- status that release is skipped too. That is correct whenever a failed call
-- allocated nothing, which is the usual arrangement, but it is a property of the C
-- function rather than one the types check. Where a call can fail /and/ still leave
-- something to free, close it with 'HsBindgen.HighLevel.resultIO' and do the check
-- inside the assembler, which runs after the read-backs.
--
-- @check@ is in 'IO' because deciding whether a status is a failure often needs one.
-- A C library commonly reports only \"failed\" in the return value and keeps the
-- detail somewhere the check has to go and fetch, such as thread-local state or a
-- @get_last_error@ call. Where the verdict is a pure one and failure is expected
-- input rather than an error, 'maybeResult' and 'eitherResult' return it instead of
-- throwing. For a check that only needs the value and has no outputs,
-- 'HsBindgen.HighLevel.throwOnNonZero' is this with a pure classifier. To ignore the
-- return value entirely, pass @\\_ -> pure ()@, which generalises
-- 'HsBindgen.HighLevel.discardResult' to a spec that has outputs.
--
checkedResult
  :: forall os c hs.
    AutoOutputs os hs
  => (c -> IO ()) -- ^ inspect the C return; throw to reject it
  -> ToHighLevel os (IO c) (IO hs)
checkedResult check = onReturn $ \c pending -> do
    check c
    -- Only now, past the check, are the deferred read-backs run.
    autoOutputs <$> pending
{-# INLINE checkedResult #-}

-- | 'checkedResult' for a status that means the answer is __absent__ rather than that
-- the call failed: assemble the outputs into @'Just' hs@ when @accept@ passes, and
-- give back 'Nothing' otherwise.
--
-- A C library uses this shape wherever failure is expected input: an authenticated
-- decryption handed a forged ciphertext, a lookup that finds nothing, a parse that
-- rejects its input. Throwing would be wrong there, and so would reading the
-- out-parameters back, which is what makes this a closer rather than something you
-- can write with 'HsBindgen.HighLevel.resultPure':
--
-- > open :: Key -> Nonce -> ByteString -> Maybe ByteString
-- > open key nonce ciphertext = toHighLevelPure crypto_secretbox_open_easy
-- >   ( output (byteStringOut (BS.length ciphertext - macBytes)) -- unsigned char *m
-- >   $ input2 unsafeByteStringLenIn                             -- c, clen
-- >   $ autoInputs                                               -- n, k
-- >   $ maybeResult (== 0)
-- >   ) ciphertext nonce key
--
-- __A rejected status skips the read-backs__, exactly as at 'checkedResult' and for
-- the same reasons: on a call that failed to authenticate, the plaintext buffer holds
-- nothing worth copying out. Closing with @'HsBindgen.HighLevel.resultPure' (\\m s ->
-- if s == 0 then Just m else Nothing)@ instead would read it every time.
--
-- Compare 'HsBindgen.HighLevel.throwOn' and 'checkedResult', which throw, and
-- 'eitherResult', which keeps a reason for the rejection.
--
maybeResult
  :: forall os c hs.
    AutoOutputs os hs
  => (c -> Bool) -- ^ does the C return say the out-parameters are worth reading?
  -> ToHighLevel os (IO c) (IO (Maybe hs))
maybeResult accept = onReturn $ \c pending ->
    if accept c
      then Just . autoOutputs <$> pending
      else pure Nothing
{-# INLINE maybeResult #-}

-- | 'maybeResult' keeping a reason: assemble the outputs into @'Right' hs@ when
-- @classify@ returns 'Nothing', and give back @'Left' e@ when it returns @'Just' e@.
--
-- The shape for a C library whose failures are expected /and/ worth telling apart, an
-- error @enum@ say, where 'Nothing' would throw the reason away and an exception would
-- overstate what happened:
--
-- > lookupKey :: Db -> ByteString -> IO (Either DbError ByteString)
-- > lookupKey = toHighLevel db_lookup
-- >           $ output (byteStringOut valueSize)
-- >           $ autoInputs
-- >           $ eitherResult (\c -> if c == 0 then Nothing else Just (toDbError c))
--
-- A rejected status skips the read-backs, as at 'maybeResult'.
--
eitherResult
  :: forall os c hs e.
    AutoOutputs os hs
  => (c -> Maybe e) -- ^ the reason to reject the C return, if there is one
  -> ToHighLevel os (IO c) (IO (Either e hs))
eitherResult classify = onReturn $ \c pending ->
    case classify c of
      Just e  -> pure (Left e)
      Nothing -> Right . autoOutputs <$> pending
{-# INLINE eitherResult #-}

{-------------------------------------------------------------------------------
  Filling a spec

  A structural recursion over the two types @auto@ walks. The three instances match
  on different type constructors and so are pairwise apart, which is what makes a
  stuck decision stay stuck: on a binding with no type signature @hi@ is still a
  metavariable, no instance matches, the constraint is deferred, and GHC reports the
  ambiguity it actually found. A catch-all instance would instead commit to the error
  branch and bury that ambiguity under "Overlapping instances".
-------------------------------------------------------------------------------}

-- | @Auto os lo hi@ says: __the whole of the rest of the spec writes itself__, from
-- @hi@'s remaining arguments and @lo@'s remaining C arguments. See the module header
-- for what that means and how it decides.
--
-- Write 'auto' in a spec; name the class only to demand that some other spec can be
-- completed automatically.
class Auto (os :: [Type]) lo hi where
  -- | Write every remaining combinator in a spec from the high-level type signature,
  -- then close it.
  auto :: ToHighLevel os lo hi

-- The high-level type still takes an argument: fill the C arguments it covers with its
-- default and recurse.
instance ( DefaultIn a lo lo'
         , ThreadIn hi'
         , Auto os lo' hi'
         )
      => Auto os lo (a -> hi') where
  auto = inputN (defaultIn @a @lo @lo') (auto @os @lo' @hi')
  {-# INLINE auto #-}

-- Both sides have reached the result: close the spec.
instance AutoResult os c hs => Auto os (IO c) (IO hs) where
  auto = autoResult @os @c @hs
  {-# INLINE auto #-}

-- hi bottomed out at a result while lo still takes arguments.
instance TypeError (AutoMismatch (IO hs) (c -> lo'))
      => Auto os (c -> lo') (IO hs) where
  auto = unreachable

{-------------------------------------------------------------------------------
  Filling only the inputs

  'auto' runs to the end of a spec once it starts, closer included, which suits a
  result of the obvious shape and nothing else. Writing one closer by hand should not
  cost the inputs as well, so 'autoInputs' writes the same input combinators and then
  stops, handing what is left to a spec you wrote.
-------------------------------------------------------------------------------}

-- | @AutoInputs lo hi lo' hi'@ says: __filling every argument the high-level type @hi@
-- takes, from @lo@ and the default marshallers, leaves @lo'@ of the C function and
-- @hi'@ of the high-level type__. @hi'@ is always the high-level result, since that is
-- where the walk stops; @lo'@ is whatever C arguments the high-level type did not
-- account for, and is what the tail spec has to consume.
--
-- Read the four in pairs: @lo@ and @hi@ are the two types going in, @lo'@ and @hi'@
-- the two coming out. They are determined by the first two, so a combinator of your
-- own can pin the far end and leave the near end open:
--
-- > -- every fallible call in this library returns int, and zero means it worked
-- > verifyLike :: AutoInputs lo hi (IO CInt) (IO Bool) => ToHighLevel '[] lo hi
-- > verifyLike = autoWith (== 0)
--
-- which reads: whatever the C function and the high-level type are, filling the
-- high-level arguments has to leave a C call returning @int@ and a high-level result
-- of 'Bool'.
--
-- Both instances match on @hi@'s head, one on an arrow and one on 'IO', so a binding
-- with no type signature leaves the constraint deferred and GHC reports the ambiguity
-- rather than committing to either.
--
class AutoInputs lo hi lo' hi' | lo hi -> lo' hi' where
  -- | Fill the high-level type's remaining arguments from its signature, then continue
  -- with a spec you wrote yourself.
  --
  -- 'auto' writes every combinator including the closer, so a binding that needs any
  -- other result shape loses the inputs too. The usual reason is a status C returns as
  -- @int@ that Haskell wants as 'Bool', a conversion no default makes. Writing the
  -- ordinary arguments out only to reach that closer is what 'autoInputs' avoids:
  --
  -- > toHighLevel c_verify (input defaultIn $ input defaultIn $ resultPure (== 0))
  -- > toHighLevel c_verify (autoInputs                        $ resultPure (== 0))
  --
  -- Reach for it only where the defaults are the marshallers you want. The
  -- 'Data.ByteString.ByteString' default fills a @(const T *, len)@ pair by copying,
  -- so that it can NUL-terminate; a binding that would rather hand C the buffer it
  -- already has needs the zero-copy @unsafeByteStringLenIn@, and has to write that
  -- combinator out. (@crypto_sign_detached@ in @examples\/libsodium@ spells its message
  -- out for exactly this reason; @crypto_sign_verify_detached@ next to it takes the copy
  -- and stays a one-liner.)
  --
  -- The tail is an ordinary spec, so it doesn't need to be only a closer. Anything the
  -- high-level type does not take an argument for can go there, an
  -- 'HsBindgen.HighLevel.output' in particular.
  --
  -- > signMultipart :: SecretKey -> [ByteString] -> Signature
  -- > signMultipart secretKey chunks = toHighLevelPure crypto_sign_final_create
  -- >   ( scratch (withSignState chunks)         -- crypto_sign_state *state
  -- >   $ output (byteStringOut signatureBytes)  -- unsigned char *sig
  -- >   $ output (unmarshalOutPure fromIntegral) -- unsigned long long *siglen_p
  -- >   $ autoInputs                             -- const unsigned char *sk
  -- >   $ resultIO (takeSignature "crypto_sign_final_create")
  -- >   ) secretKey
  --
  -- __It takes every high-level argument that is left, not a chosen number of them.__
  -- Nothing in its type says \"the next two\", so it runs until the high-level type's
  -- arrows are gone and stops for good there. It therefore has to be the last
  -- combinator in a spec that consumes a high-level argument: anything written below it
  -- can fill C arguments the high-level type does not expose, but not ones it does.
  --
  -- > toHighLevel cAdd (autoInputs $ input defaultIn $ defaultRes)
  -- >   -- Couldn't match type: IO CInt with: c0 -> IO c1
  --
  -- To fill one argument from the defaults, write that combinator:
  -- @'HsBindgen.HighLevel.input' 'defaultIn'@ /is/ the step 'autoInputs' takes, once,
  -- and mixes with hand-written combinators anywhere in a spec. 'autoInputs' is the
  -- short way to say it several times in a row.
  --
  -- The same reasoning covers a C argument sitting /between/ two high-level arguments,
  -- an out-parameter in the middle of the prototype say: 'autoInputs' would run
  -- straight past it, so write those combinators out.
  --
  -- Like 'auto' it reads the argument types from the high-level signature, so the
  -- signature has to be there. @'auto' = 'autoInputs' 'autoResult'@ in every respect
  -- but the error it reports when the two types stop lining up, which 'auto' can be
  -- more specific about because it knows how the spec ends.
  --
  autoInputs
    :: forall os.
       ToHighLevel os lo' hi' -- ^ the rest of the spec, from the first combinator
                              --   the high-level type takes no argument for
    -> ToHighLevel os lo  hi

-- One more high-level argument: fill the C arguments it covers, and carry on. The same
-- step as Auto's first instance, differing only in where it stops.
--
-- The collected outputs are quantified in the method rather than indexing the class,
-- because neither instance looks at them: the walk is over hi and lo alone and carries
-- os through untouched. ThreadIn is indexed the same way, for the same reason.
instance ( DefaultIn a lo lo''
         , ThreadIn hi2
         , AutoInputs lo'' hi2 lo' hi'
         )
      => AutoInputs lo (a -> hi2) lo' hi' where
  autoInputs rest = inputN (defaultIn @a @lo @lo'') (autoInputs @lo'' @hi2 rest)
  {-# INLINE autoInputs #-}

-- The high-level type takes no more arguments: hand over whatever is left of the C
-- function to the tail, untouched.
instance (lo ~ lo', hi' ~ IO r) => AutoInputs lo (IO r) lo' hi' where
  autoInputs = id
  {-# INLINE autoInputs #-}

{-------------------------------------------------------------------------------
  Filling the inputs and closing in one word

  'autoInputs' composes with any closer, and for a spec with combinators of its own
  that is how to write it. The three below are the common whole-spec cases, where
  every input is ordinary and the only decision left is the result.

  All three close the spec, so a combinator that is not an input has to be written
  *above* them. In practice that means an 'HsBindgen.HighLevel.output' is fine (C puts
  out-parameters first by convention, so it is written first anyway) but a 'fixed' or
  'scratch' argument that C takes *after* the ones the high-level type exposes is not:
  use
  'autoInputs' with an explicit closer there.
-------------------------------------------------------------------------------}

-- | Fill the high-level arguments from the signature, then close with a result
-- function of your own: @'autoInputs' '.' 'HsBindgen.HighLevel.resultPure'@.
--
-- This fits the binding whose arguments are all ordinary and whose only decision is
-- the result, which is the case whenever C reports an outcome the Haskell type does
-- not want verbatim.
--
-- The function is an 'AssembleOutputs', so on a spec with outputs it takes one
-- argument per output and then the C return, exactly as
-- 'HsBindgen.HighLevel.resultPure' does:
--
-- > verifyDetached :: PublicKey -> Signature -> ByteString -> Bool
-- > verifyDetached pk sig msg =
-- >   toHighLevelPure crypto_sign_verify_detached (autoWith (== 0)) sig msg pk
--
-- Compare 'autoChecked' and 'autoMaybe', which close on a verdict about the status
-- rather than on an assembler, and which do not read the out-parameters back when the
-- verdict goes against them.
--
-- There is no @IO@ counterpart on purpose: reach for
-- @'autoInputs' '$' 'HsBindgen.HighLevel.resultIO' f@, which reads the same and keeps
-- the closer vocabulary in one place.
--
autoWith
  :: forall os lo hi c hs.
    ( AutoInputs lo hi (IO c) (IO hs)
    , ApplyOutputs os
    )
  => AssembleOutputs os (c -> hs)
  -> ToHighLevel os lo hi
autoWith = autoInputs . resultPure
{-# INLINE autoWith #-}

-- | Fill the high-level arguments from the signature, then close on a status:
-- @'autoInputs' '.' 'checkedResult'@.
--
-- This fits a call of the shape 'checkedResult' describes whose remaining arguments
-- are also ordinary, which between them covers a good deal of a typical C library.
-- An authenticated-encryption call writes the ciphertext into an out-parameter and
-- reports success in its return value (from @examples\/libsodium@):
--
-- > encrypt :: Key -> Nonce -> ByteString -> ByteString
-- > encrypt key nonce message = toHighLevelPure crypto_secretbox_easy
-- >   ( output (byteStringOut (macBytes + BS.length message)) -- unsigned char *c
-- >   $ input2 unsafeByteStringLenIn                          -- m, mlen
-- >   $ autoChecked (checkStatus "crypto_secretbox_easy")     -- n, k
-- >   ) message nonce key
--
-- Everything 'checkedResult' says applies, the check running before the read-backs
-- included. A binding that names its own check once may find
-- @'autoInputs' '$' theCheck@ reads better; both spell the same spec.
--
-- See 'AutoOutputs' for stating this constraint in a closer of your own.
--
autoChecked
  :: forall os lo hi c hs.
    ( AutoInputs lo hi (IO c) (IO hs)
    , AutoOutputs os hs
    )
  => (c -> IO ()) -- ^ inspect the C return; throw to reject it
  -> ToHighLevel os lo hi
autoChecked = autoInputs . checkedResult
{-# INLINE autoChecked #-}

-- | Fill the high-level arguments from the signature, then close on a verdict:
-- @'autoInputs' '.' 'maybeResult'@.
--
-- 'autoChecked' with the failure returned rather than thrown, for a call whose status
-- means the answer is absent. Decryption and encryption make the pair, since a
-- rejected ciphertext is expected input rather than a failure (again from
-- @examples\/libsodium@):
--
-- > open :: Key -> Nonce -> ByteString -> Maybe ByteString
-- > open key nonce ciphertext = toHighLevelPure crypto_secretbox_open_easy
-- >   ( output (byteStringOut (BS.length ciphertext - macBytes)) -- unsigned char *m
-- >   $ input2 unsafeByteStringLenIn                             -- c, clen
-- >   $ autoMaybe (== 0)                                         -- n, k
-- >   ) ciphertext nonce key
--
-- Note where the 'Maybe' lands: the high-level result is @'Maybe' hs@, and the
-- @'AutoOutputs' os hs@ constraint is on the @hs@ inside it, because the outputs
-- assemble into the value the 'Just' carries.
--
autoMaybe
  :: forall os lo hi c hs.
    ( AutoInputs lo hi (IO c) (IO (Maybe hs))
    , AutoOutputs os hs
    )
  => (c -> Bool) -- ^ does the C return say the out-parameters are worth reading?
  -> ToHighLevel os lo hi
autoMaybe = autoInputs . maybeResult
{-# INLINE autoMaybe #-}

{-------------------------------------------------------------------------------
  Haddock named sections
-------------------------------------------------------------------------------}

-- $status
-- Three closers for the same C shape, differing only in what a rejected status
-- becomes. All three consume the status, so the high-level result comes from the
-- out-parameters alone ('AutoOutputs'), and all three __skip the read-backs when the
-- status is rejected__, which a closer written with
-- 'HsBindgen.HighLevel.resultPure' cannot do.
--
-- > closer          verdict          rejected becomes    reach for it when
-- > checkedResult   c -> IO ()       the check's throw   failure is an error
-- > maybeResult     c -> Bool        Nothing             failure is expected input
-- > eitherResult    c -> Maybe e     Left e              failure is expected and worth naming
--
-- 'checkedResult' is the one whose verdict is in 'IO', because a C library that
-- reports only \"failed\" keeps the detail somewhere the check has to go and fetch.
-- The other two decide from the status alone.

-- $vocabulary
-- A C library usually repeates a certain convention/pattern a
-- few times. That's what these classes are for: a combinator of your own,
-- polymorphic in the spec it completes, that a binding then uses at every call.
--
-- Reach for them second. A combinator whose specs all have the same /shape/ (one
-- out-parameter, one closer, only the inputs varying) is an ordinary Haskell function
-- taking the varying part as an argument, and needs none of this; the README works
-- that case through on libgit2's handle constructors. These four are for a combinator
-- that has to work at any number of arguments, any number of out-parameters, or any C
-- return type, which cannot be passed as a value and so has to be said in a context.
--
-- [@'Auto' os lo hi@]: the rest of the spec writes itself.
--
-- [@'AutoInputs' lo hi lo' hi'@]: filling @hi@'s arguments from @lo@ leaves @lo'@ and
--   @hi'@. The one to reach for when your combinator fills arguments.
--
-- [@'AutoOutputs' os hs@]: the outputs alone assemble into @hs@. The one to reach for
--   when your combinator closes on a status.
--
-- [@'AutoResult' os c hs@]: the outputs and the converted C return assemble into
--   @hs@. Only for a combinator that must close at any C return type; where a status
--   is consumed, 'AutoOutputs' is what you want.
--
-- == Which one a combinator needs
--
-- A combinator in the body asks for the constraint of the same name in the context:
--
-- > the body uses                                     the context needs
-- > autoInputs                                        AutoInputs lo hi lo' hi'
-- > auto                                              Auto os lo hi
-- > autoResult                                        AutoResult os c hs
-- > autoOutputs, or a closer built on it              AutoOutputs os hs
-- >   (checkedResult / maybeResult / eitherResult)
-- > resultPure / resultIO over a caller's assembler   ApplyOutputs os
--
-- Two things sit outside that rule. Anything GHC defaulted is yours to replace, a
-- numeric literal above all. And a type occurring only under 'AssembleOutputs' is not
-- /determined/ by it: GHC cannot infer a type family's inputs from its result, so such
-- a type has to be named somewhere else; the 'AutoInputs' constraint normally already
-- does.
--
-- == Letting the compiler say it instead
--
-- None of that has to be worked out up front. Write the body and the /result/ type,
-- leave the context off, and read the error:
--
-- > foo :: ToHighLevel '[] lo hi
-- > foo = autoInputs $ resultPure (== 0)
-- >
-- >   -- No instance for 'AutoInputs lo hi (IO Integer) (IO Bool)'
-- >   --   arising from a use of 'autoInputs'
--
-- Paste that back and it compiles. The one thing to fix by hand is @Integer@, which
-- is where GHC defaulted the @0@ rather than anything the library chose; replace it
-- with the C type you meant, or leave it open:
--
-- > foo :: (AutoInputs lo hi (IO c) (IO Bool), Eq c, Num c) => ToHighLevel '[] lo hi
--
-- The same loop covers the rest. Generalising @\'[]@ to @os@ makes GHC point out that
-- @(== 0)@ cannot be an assembler over outputs; taking the assembler as an argument
-- instead makes it point out that @c@ and @hs@ sit under a non-injective family;
-- adding the 'AutoInputs' constraint names them and leaves only the
-- 'HsBindgen.HighLevel.ApplyOutputs' to add. Four errors later you have rebuilt
-- 'autoWith'. Going the same way from @'autoInputs' '$' 'checkedResult' check@
-- rebuilds 'autoChecked'.
--
-- == A closer for a library-wide status convention
--
-- libgit2 reports failure as a negative @int@ and keeps the message in thread-local
-- state. Stating that once gives every fallible call in the binding its closer:
--
-- > checkedStatus :: AutoOutputs os hs => ToHighLevel os (IO CInt) (IO hs)
-- > checkedStatus = checkedResult checkStatus
--
-- @os@ and @hs@ stay open, so the one definition closes a call with no
-- out-parameters to @IO ()@, one to @IO a@, and two to @IO (a, b)@.
--
-- == The same, filling the arguments too
--
-- Compose it with 'autoInputs' and the calls whose arguments are all ordinary need
-- nothing else:
--
-- > gitCall :: (AutoInputs lo hi (IO CInt) (IO hs), AutoOutputs os hs)
-- >         => ToHighLevel os lo hi
-- > gitCall = autoInputs checkedStatus
--
-- Two constraints, one per half of what it does: the first says the arguments fill
-- themselves down to a C call returning @int@, the second that the outputs assemble
-- into the result.
--
-- == A whole-spec generator
--
-- Where the closer needs no configuration at all, the combinator is a value:
--
-- > -- int crypto_sign_verify_detached(...);  0 means the signature checks out
-- > verifyLike :: AutoInputs lo hi (IO CInt) (IO Bool) => ToHighLevel '[] lo hi
-- > verifyLike = autoWith (== 0)
-- >
-- > verifyDetached :: PublicKey -> Signature -> ByteString -> Bool
-- > verifyDetached pk sig msg =
-- >   toHighLevelPure crypto_sign_verify_detached verifyLike sig msg pk
--
-- The @'[]@ says this one collects no outputs, which is what lets the constraint be a
-- single line: with outputs it would need an 'AutoOutputs' or 'ApplyOutputs' beside
-- it, as @gitCall@ does.
--
-- == Parameterising one
--
-- A combinator is an ordinary function, so anything the convention leaves open
-- becomes an argument:
--
-- > statusVia :: AutoInputs lo hi (IO CInt) (IO hs) => (CInt -> hs) -> ToHighLevel '[] lo hi
-- > statusVia = autoWith
--
-- == What the constraints will not do
--
-- They say the two types line up; they do not say how many arguments there are. A
-- combinator cannot fill \"the next two\" arguments and stop, because 'autoInputs'
-- runs until the high-level type's arrows are gone. Where a spec needs to interleave,
-- write @input defaultIn@ per argument: it is the step 'autoInputs' takes, once.
