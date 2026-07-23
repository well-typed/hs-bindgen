{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Fill a wrapper's ordinary positions from its type signature.
--
-- A spec has one position per C argument, but most of those positions hold no
-- decision: an @int@ the wrapper takes as an 'Int', a @double@ it takes as a
-- 'Double', a return value converted the obvious way. 'auto' writes all of them,
-- reading each one off the wrapper's type signature, so the only positions left
-- written out are the ones that needed a decision.
--
-- When no position needs one, the whole spec is 'auto':
--
-- > -- int sodium_library_version_major(void);
-- > major :: IO Int
-- > major = toHighLevel auto sodium_library_version_major
--
-- == The family
--
-- Few bindings are uniform throughout, so 'auto' comes with variants that hand part
-- of the decision back. They differ in two things: whether they fill the wrapper's
-- arguments, and how they close.
--
-- > combinator            fills the arguments   closes with                      ends the spec
-- > auto                  yes                   the obvious result               yes
-- > autoWith f            yes                   resultPure f                     yes
-- > autoChecked check     yes                   the outputs, once check passes   yes
-- > autoInputs rest       yes                   whatever rest is                 no
-- > autoResult            no                    the obvious result               yes
-- > checkedResult check   no                    the outputs, once check passes   yes
--
-- __Everything in that last column but 'autoInputs' closes the spec, so it has to be
-- written last.__ Nothing can follow it: an 'HsBindgen.HighLevel.output' or
-- 'HsBindgen.HighLevel.fixed' that C takes /after/ the arguments the wrapper exposes
-- has to go above it, or the spec needs 'autoInputs' and a closer written by hand.
--
-- 'autoResult' and 'checkedResult' are closers that fill no arguments: they end a spec
-- whose positions are all written out. The rest begin wherever you put them and fill
-- the wrapper's remaining arguments from there.
--
-- Reach for 'auto' first. The sections below cover how it decides, what it leaves to
-- you, and when one of the variants is the one you want instead.
--
-- == How it decides
--
-- 'auto' walks two types side by side: the wrapper's type, taken from its signature,
-- and whatever is left of the C function's type. At each step it compares the two
-- heads. Take @double hypot(double, double)@, imported as
-- @c_hypot :: CDouble -> CDouble -> IO CDouble@, and a wrapper
-- @hsHypot :: Double -> Double -> IO Double@ defined as @toHighLevel auto c_hypot@:
--
-- > step   wrapper type left               C type left                        auto does
-- >  1     Double -> Double -> IO Double   CDouble -> CDouble -> IO CDouble   fill CDouble from Double
-- >  2     Double -> IO Double             CDouble -> IO CDouble              fill CDouble from Double
-- >  3     IO Double                       IO CDouble                         close: CDouble to Double
--
-- There are three cases, and that is the whole rule:
--
--   * the wrapper still takes an argument, so fill C positions with that type's
--     'HsBindgen.HighLevel.Defaults.defaultIn' and carry on;
--   * both sides have reached the result, so close the spec (see 'autoResult');
--   * the wrapper has run out of arguments but C has not, which is an error: nothing
--     in the signature accounts for that leftover C argument.
--
-- A Haskell argument does not have to line up one-for-one with a C argument. A
-- 'Data.ByteString.ByteString' argument fills a @(pointer, length)@ pair, so step 1
-- above can consume two C positions at once. The walk is driven by the wrapper's
-- signature, and C positions are consumed to match.
--
-- == What it leaves to you
--
-- 'auto' never fills an 'HsBindgen.HighLevel.output' or a
-- 'HsBindgen.HighLevel.scratch' position, because it has nothing to go on. A C
-- @int *@ argument might be a pointer C reads, an out-parameter C writes,
-- or an array, and nothing in the type says which; a scratch buffer's length is not
-- in the type at all. You write those positions and 'auto' takes the rest:
--
-- > encodeText
-- >   :: String -> Ecc -> Int -> Int -> Mask -> Bool -> IO (IncompleteArray Word8, Bool)
-- > encodeText = toHighLevel
-- >   ( input defaultIn      -- text
-- >   $ scratchArray maxLen  -- tempBuffer: C writes it, we never read it
-- >   $ output qrCodeOut     -- qrcode: the out-parameter we keep
-- >   $ auto                 -- ecl, minVersion, maxVersion, mask, boostEcl,
-- >   ) qrcodegen_encodeText --   then the (qrcode, ok) result
--
-- 'auto' runs to the end of the spec once it starts, so it always comes last. An
-- ordinary input that sits /before/ an explicit position, like @text@ here, is
-- written out as @input defaultIn@: one line, saying the same thing.
--
-- == Write the signature first
--
-- Everything 'auto' fills is read from the wrapper's type signature, so the
-- signature has to be there. Without one there is nothing to read, and the error is
-- an ambiguous type variable rather than anything about marshalling.
--
-- That same property makes 'auto' the right __placeholder while you are still
-- writing a spec__. Unlike @undefined@ it constrains the positions above it, so the
-- holes you are working on come back with concrete types. See \"Building against
-- typed holes\" in "HsBindgen.HighLevel".
--
-- == When something does not line up
--
-- @No default input marshaller for type ...@: 'auto' reached an argument whose type
-- has no default. Give that position an explicit @input@, or give the type a
-- 'HsBindgen.HighLevel.Defaults.DefaultIn' instance.
--
-- @auto cannot line the high-level type up with the C function@: the third case
-- above. A C argument is left over that the wrapper does not expose, so give it an
-- explicit 'HsBindgen.HighLevel.output' or 'HsBindgen.HighLevel.scratch', or add the
-- missing argument to the signature.
--
-- @auto cannot assemble this result@: the outputs and the C return do not add up to
-- the result type in the signature. See 'autoResult' for the shape it builds.
--
module HsBindgen.HighLevel.Auto (
    -- * Filling a spec automatically
    auto
  , autoInputs
    -- ** Filling the inputs and closing in one word
  , autoWith
  , autoChecked
    -- * Closing a spec automatically
  , autoResult
  , checkedResult
  , Checked
    -- * Machinery
    --
    -- | The classes and type families the combinators above are built from. Name
    -- them when writing a combinator of your own that is polymorphic in the spec it
    -- completes, as 'autoChecked' is; a spec that only uses them never does.
  , AutoAt
  , AutoStep
  , AutoStepFor
  , AutoInputs
  , TakesArgument
  , AutoResult
  , IsVoid
  ) where

import Data.Kind (Type)
import GHC.TypeLits (TypeError)

import HsBindgen.HighLevel (ApplyOutputs, Assembler, ToHighLevel, inputN,
                            resultPure)
import HsBindgen.HighLevel.Defaults (DefaultIn (..), DefaultRes (..),
                                     defaultRes)
import HsBindgen.HighLevel.Internal.Errors (AutoMismatch, AutoResultMismatch,
                                            unreachable)
import HsBindgen.HighLevel.Internal.Spec (ToHighLevel (..))
import HsBindgen.HighLevel.Internal.Threading (ThreadIn)

-- | Does the C function return @void@? A @void@ return is imported as @IO ()@, so
-- this asks whether the C return type is @()@.
type IsVoid :: Type -> Bool
type family IsVoid c where
  IsVoid () = 'True
  IsVoid c  = 'False

-- | Close a spec by building its result the obvious way: __every output in spec
-- order, then the converted C return value, unless the C function returns @void@__.
--
-- > C returns   outputs   wrapper result
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
-- covered.
--
-- 'auto' ends with this. Reach for 'autoResult' on its own when the inputs need
-- explicit marshallers but the result is that plain shape:
--
-- > hsParseInt :: String -> IO (Int, Int)
-- > hsParseInt = toHighLevel ( input withCStringIn
-- >                          $ output peekIntOut
-- >                          $ autoResult -- (the output, the converted return)
-- >                          ) c_parse_int
--
-- For any other result shape, a record, a 'Maybe', a status the wrapper checks
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
    AutoResult (IsVoid c) os c hs
  => ToHighLevel os (IO c) (IO hs)
autoResult = autoResultFor @(IsVoid c)
{-# INLINE autoResult #-}

-- | @Checked os hs@ says: __the outputs @os@ assemble into the result @hs@ on their
-- own__, with the C return value contributing nothing.
--
-- That is the situation a check creates. Once 'checkedResult' has handed the status to
-- @check@, the status has done its job and no part of it survives into the result, so
-- the wrapper's result has to come from the out-parameters alone. A call that returns
-- @int@ and one that returns @void@ therefore assemble their results identically here,
-- which is why this is defined as 'AutoResult' at a @void@ return:
--
-- > void f(int *a, int *b);   -- IO (Int, Int)
-- > int  g(int *a, int *b);   -- IO (Int, Int) too, once the int is checked away
--
-- __You rarely write it.__ Using 'checkedResult' or 'autoChecked' in a spec never
-- requires naming it, because GHC solves the constraint from the spec's own text. It
-- has a name for one case: a binding whose whole library shares a status convention,
-- which is worth stating once rather than at every call. Give that closer a signature
-- and @Checked@ is the constraint it needs:
--
-- > -- every fallible call in this binding reports failure as a negative int
-- > checkedStatus :: Checked os hs => ToHighLevel os (IO CInt) (IO hs)
-- > checkedStatus = checkedResult (\\n -> when (n < 0) (throwIO =<< lastError n))
--
-- The @os@ and @hs@ stay polymorphic, so that one definition closes a spec with no
-- outputs (@IO ()@), one output (@IO a@), or several (@IO (a, b)@), and each call site
-- fixes them.
--
-- Spelled as a synonym rather than written out because @'AutoResult' \'True os () hs@
-- needs @DataKinds@ at every use site to write that promoted @\'True@.
type Checked os hs = AutoResult 'True os () hs

-- | Close a spec whose C return value is a __status guarding the out-parameters__
-- rather than part of the result: run @check@ on it, then assemble the outputs
-- exactly as 'autoResult' would for a @void@-returning call.
--
-- This is the shape most of a C library has. A call reports success or failure in
-- its return value and delivers the answer through @T *out@ parameters, so the
-- status belongs in the check and never in the result:
--
-- > keypair :: IO (PublicKey, SecretKey)
-- > keypair = toHighLevel ( output publicKeyOut
-- >                       $ output secretKeyOut
-- >                       $ checkedResult (throwUnlessZero (SodiumError "crypto_sign_keypair"))
-- >                       ) crypto_sign_keypair
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
-- @get_last_error@ call. For a check that only needs the value,
-- 'HsBindgen.HighLevel.throwOnNonZero' is this with
-- a pure classifier and no outputs. To ignore the return value entirely, pass
-- @\\_ -> pure ()@, which generalises 'HsBindgen.HighLevel.discardResult' to a spec
-- that has outputs.
--
checkedResult
  :: forall os c hs.
    Checked os hs
  => (c -> IO ()) -- ^ inspect the C return; throw to reject it
  -> ToHighLevel os (IO c) (IO hs)
checkedResult check =
    case autoResult @os @() @hs of
      ToHighLevel close ->
        ToHighLevel $ \pending cFn -> do
          c <- cFn
          check c
          -- Only now, past the check, does 'close' run 'pending' and read the
          -- out-parameters back. The unit stands in for the return value the
          -- assembled result does not use.
          close pending (pure ())
{-# INLINE checkedResult #-}

-- | The table behind 'autoResult', indexed by whether the C return is @void@ and by
-- the outputs the spec collected. Write 'autoResult' in a spec; name this class only
-- to demand that some other spec can be closed automatically.
class AutoResult (void :: Bool) (os :: [Type]) c hs where
  autoResultFor :: ToHighLevel os (IO c) (IO hs)

{-
  Dispatch is on the triple (is the return void, how many outputs, what shape the
  result is), all three known from the spec's text and the wrapper's signature, so
  the instances do not overlap.

  The result's *components* are matched in the head, so a wrong number of them falls
  through to the AutoResultMismatch instance rather than producing a raw mismatch,
  while their *identities* sit in the context, so the wrapper's signature fixes the
  outputs' Haskell types rather than the other way round. Moving an equality from a
  context into a head breaks inference for a polymorphic unmarshaller; moving one the
  other way stops AutoResultMismatch from ever firing.
-}

-- A void return contributes nothing, so the result is the outputs alone. The C type
-- is written () rather than a variable: IsVoid c ~ 'True already forces it, and
-- spelling it out stops a hand-written @AutoResult 'True os c hs@ constraint from
-- matching at a non-void c and silently dropping its return value.
instance AutoResult 'True '[] () () where
  autoResultFor = resultPure (const ())
  {-# INLINE autoResultFor #-}
instance h1 ~ o1 => AutoResult 'True '[o1] () h1 where
  autoResultFor = resultPure (\o1 _ -> o1)
  {-# INLINE autoResultFor #-}
instance (h1 ~ o1, h2 ~ o2) => AutoResult 'True '[o2, o1] () (h1, h2) where
  autoResultFor = resultPure (\o1 o2 _ -> (o1, o2))
  {-# INLINE autoResultFor #-}
instance (h1 ~ o1, h2 ~ o2, h3 ~ o3)
      => AutoResult 'True '[o3, o2, o1] () (h1, h2, h3) where
  autoResultFor = resultPure (\o1 o2 o3 _ -> (o1, o2, o3))
  {-# INLINE autoResultFor #-}
instance (h1 ~ o1, h2 ~ o2, h3 ~ o3, h4 ~ o4)
      => AutoResult 'True '[o4, o3, o2, o1] () (h1, h2, h3, h4) where
  autoResultFor = resultPure (\o1 o2 o3 o4 _ -> (o1, o2, o3, o4))
  {-# INLINE autoResultFor #-}
instance (h1 ~ o1, h2 ~ o2, h3 ~ o3, h4 ~ o4, h5 ~ o5)
      => AutoResult 'True '[o5, o4, o3, o2, o1] () (h1, h2, h3, h4, h5) where
  autoResultFor = resultPure (\o1 o2 o3 o4 o5 _ -> (o1, o2, o3, o4, o5))
  {-# INLINE autoResultFor #-}

-- A non-void return is the last component, converted by 'DefaultRes'.
instance DefaultRes c hs => AutoResult 'False '[] c hs where
  autoResultFor = defaultRes
  {-# INLINE autoResultFor #-}
instance (h1 ~ o1, DefaultRes c h2) => AutoResult 'False '[o1] c (h1, h2) where
  autoResultFor = resultPure (\o1 c -> (o1, defaultResConv c))
  {-# INLINE autoResultFor #-}
instance (h1 ~ o1, h2 ~ o2, DefaultRes c h3)
      => AutoResult 'False '[o2, o1] c (h1, h2, h3) where
  autoResultFor = resultPure (\o1 o2 c -> (o1, o2, defaultResConv c))
  {-# INLINE autoResultFor #-}
instance (h1 ~ o1, h2 ~ o2, h3 ~ o3, DefaultRes c h4)
      => AutoResult 'False '[o3, o2, o1] c (h1, h2, h3, h4) where
  autoResultFor = resultPure (\o1 o2 o3 c -> (o1, o2, o3, defaultResConv c))
  {-# INLINE autoResultFor #-}
instance (h1 ~ o1, h2 ~ o2, h3 ~ o3, h4 ~ o4, DefaultRes c h5)
      => AutoResult 'False '[o4, o3, o2, o1] c (h1, h2, h3, h4, h5) where
  autoResultFor = resultPure (\o1 o2 o3 o4 c -> (o1, o2, o3, o4, defaultResConv c))
  {-# INLINE autoResultFor #-}
instance (h1 ~ o1, h2 ~ o2, h3 ~ o3, h4 ~ o4, h5 ~ o5, DefaultRes c h6)
      => AutoResult 'False '[o5, o4, o3, o2, o1] c (h1, h2, h3, h4, h5, h6) where
  autoResultFor =
    resultPure (\o1 o2 o3 o4 o5 c -> (o1, o2, o3, o4, o5, defaultResConv c))
  {-# INLINE autoResultFor #-}

-- Fallback: the outputs, the C return and the wrapper's result type do not line up.
-- Reached only once hs is known (see 'AutoStepFor'), so it cannot fire early.
instance {-# OVERLAPPABLE #-} TypeError (AutoResultMismatch os c hs)
      => AutoResult void os c hs where
  autoResultFor = unreachable

{-------------------------------------------------------------------------------
  Filling a spec

  A structural recursion over the two types @auto@ walks, gated by a closed type
  family so that a stuck decision stays stuck (see 'AutoStepFor').
-------------------------------------------------------------------------------}

-- | The three things 'auto' can see at a position: a wrapper argument to fill, a
-- result to close, or a C argument the wrapper never takes.
data AutoStep = FillInput
              | Close
              | Leftover

-- | Which of the three 'auto' is looking at, decided from the wrapper type @hi@ and
-- what is left of the C type @lo@.
{-
  The point of routing auto through a closed type family is *when it does not reduce*.
  On a wrapper with no type signature hi is still a metavariable, no equation matches
  and none is apart, so the family is stuck, the AutoAt constraint is deferred,
  and GHC reports the ambiguity it actually found. An OVERLAPPABLE catch-all in AutoAt
  would instead commit to the error branch and bury that ambiguity under "Overlapping
  instances ... use IncoherentInstances".
-}
type AutoStepFor :: Type -> Type -> AutoStep
type family AutoStepFor hi lo where
  AutoStepFor (a -> hi') lo     = 'FillInput
  AutoStepFor (IO r)     (IO c) = 'Close
  AutoStepFor (IO r)     lo     = 'Leftover

-- | Fill every remaining position in a spec from the wrapper's type signature, then
-- close it. See the module header for what it fills, what it leaves to you, and how
-- it decides.
auto :: forall os lo hi. AutoAt (AutoStepFor hi lo) os lo hi => ToHighLevel os lo hi
auto = autoAt @(AutoStepFor hi lo)
{-# INLINE auto #-}

{-------------------------------------------------------------------------------
  Filling only the inputs

  'auto' runs to the end of a spec once it starts, closer included, which suits a
  result of the obvious shape and nothing else. Writing one closer by hand should not
  cost the inputs as well, so 'autoInputs' fills the same input positions and then
  stops, handing what is left to a spec you wrote.
-------------------------------------------------------------------------------}

-- | Does the wrapper still take an argument here?
--
-- This is 'AutoStepFor' looking at the wrapper's type alone. 'autoInputs' stops as
-- soon as the wrapper runs out of arguments and says nothing about what is left of
-- the C function, because the tail it was given is what accounts for that.
--
-- Stuck rather than 'False on an unsolved @hi@, for the reason given at
-- 'AutoStepFor': a wrapper with no type signature should report its ambiguity, not
-- an overlap.
type TakesArgument :: Type -> Bool
type family TakesArgument hi where
  TakesArgument (a -> hi') = 'True
  TakesArgument (IO r)     = 'False

-- | Fill the wrapper's remaining arguments from its type signature, then continue
-- with a spec you wrote yourself.
--
-- 'auto' fills every position including the closer, so a wrapper that needs any
-- other result shape loses the inputs too. The usual reason is a status C returns as
-- @int@ that Haskell wants as 'Bool', a conversion no default makes. Writing the
-- ordinary arguments out only to reach that closer is what 'autoInputs' avoids:
--
-- > toHighLevel (input defaultIn $ input defaultIn $ resultPure (== 0)) c_verify
-- > toHighLevel (autoInputs                        $ resultPure (== 0)) c_verify
--
-- Reach for it only where the defaults are the marshallers you want. A signature
-- verification taking @(const unsigned char *m, unsigned long long mlen)@, say, is
-- better off writing its inputs out, because the message wants the zero-copy
-- @unsafeByteStringLenIn@ rather than the copying 'ByteString' default. (That one is
-- real: see @crypto_sign_verify_detached@ in the @libsodium@ example, one of the
-- worked bindings the README links to.)
--
-- The tail is an ordinary spec, so it doesn't need to be only a closer. Anything the
-- wrapper does not take an argument for can go there, an 'HsBindgen.HighLevel.output'
-- in particular.
--
-- > signMultipart :: SecretKey -> [ByteString] -> IO Signature
-- > signMultipart secretKey chunks = withSignState chunks $ \\st -> toHighLevel
-- >   ( fixed  st                              -- crypto_sign_state *state
-- >   $ output (byteStringOut signatureBytes)  -- unsigned char *sig
-- >   $ output (unmarshalOutPure fromIntegral) -- unsigned long long *siglen_p
-- >   $ autoInputs                             -- const unsigned char *sk
-- >   $ resultIO (takeSignature "crypto_sign_final_create")
-- >   ) crypto_sign_final_create secretKey
--
-- __It takes every wrapper argument that is left, not a chosen number of them.__
-- Nothing in its type says \"the next two\", so it runs until the wrapper's arrows are
-- gone and stops for good there. It therefore has to be the last position in a spec
-- that consumes a wrapper argument: anything written below it can fill C arguments the
-- wrapper does not expose, but not ones it does.
--
-- > toHighLevel (autoInputs $ input defaultIn $ defaultRes) cAdd
-- >   -- Couldn't match type: IO CInt with: c0 -> IO c1
--
-- To fill one position from the defaults, write that position:
-- @'HsBindgen.HighLevel.input' 'defaultIn'@ /is/ the step 'autoInputs' takes, once,
-- and mixes with hand-written positions anywhere in a spec. 'autoInputs' is the short
-- way to say it several times in a row.
--
-- The same reasoning covers a C argument sitting /between/ two wrapper arguments, an
-- out-parameter in the middle of the prototype say: 'autoInputs' would run straight
-- past it, so write those positions out.
--
-- Like 'auto' it reads the argument types from the wrapper's signature, so the
-- signature has to be there. @'auto' = 'autoInputs' 'autoResult'@ in every respect
-- but the error it reports when the two types stop lining up, which 'auto' can be
-- more specific about because it knows how the spec ends.
--
autoInputs
  :: forall os lo lo' hi hiEnd.
    AutoInputs (TakesArgument hi) lo lo' hi hiEnd
  => ToHighLevel os lo' hiEnd -- ^ the rest of the spec, from the first position
                              --   the wrapper takes no argument for
  -> ToHighLevel os lo  hi
autoInputs = autoInputsAt @(TakesArgument hi)
{-# INLINE autoInputs #-}

-- | The recursion behind 'autoInputs', indexed by whether the wrapper still takes an
-- argument. @lo'@ and @hiEnd@ are where the walk stopped, and are what the tail spec
-- has to match; both are determined by @hi@ and @lo@, so the tail's type is forced
-- rather than guessed.
--
-- The collected outputs are quantified in the method rather than indexing the class,
-- because neither instance looks at them: the walk is over @hi@ and @lo@ alone and
-- carries @os@ through untouched. 'HsBindgen.HighLevel.Internal.Threading.ThreadIn'
-- is indexed the same way, for the same reason.
class AutoInputs (more :: Bool) lo lo' hi hiEnd where
  autoInputsAt :: forall os. ToHighLevel os lo' hiEnd -> ToHighLevel os lo hi

-- One more wrapper argument: fill the C positions it covers, and carry on. The same
-- step as AutoAt's 'FillInput, differing only in where it stops.
instance ( hi ~ (a -> hi')
         , DefaultIn a lo lo''
         , ThreadIn hi'
         , AutoInputs (TakesArgument hi') lo'' lo' hi' hiEnd
         )
      => AutoInputs 'True lo lo' hi hiEnd where
  autoInputsAt rest =
    inputN (defaultIn @a @lo @lo'') (autoInputs @_ @lo'' @lo' @hi' @hiEnd rest)
  {-# INLINE autoInputsAt #-}

-- The wrapper takes no more arguments: hand over whatever is left of the C function
-- to the tail, untouched.
instance (lo ~ lo', hi ~ hiEnd) => AutoInputs 'False lo lo' hi hiEnd where
  autoInputsAt = id
  {-# INLINE autoInputsAt #-}

{-------------------------------------------------------------------------------
  Filling the inputs and closing in one word

  'autoInputs' composes with any closer, and for a spec with positions of its own
  that is how to write it. The two below are the common whole-spec case, where every
  input is ordinary and the only decision left is the result.

  Both close the spec, so a position that is not an input has to be written *above*
  them. In practice that means an 'HsBindgen.HighLevel.output' is fine (C puts
  out-parameters first by convention, so it is written first anyway) but a 'fixed' or
  'scratch' argument that C takes *after* the ones the wrapper exposes is not: use
  'autoInputs' with an explicit closer there.
-------------------------------------------------------------------------------}

-- | Fill the wrapper's arguments from its signature, then close with a result
-- function of your own: @'autoInputs' ('HsBindgen.HighLevel.resultPure' f)@.
--
-- This fits the wrapper whose arguments are all ordinary and whose only decision is
-- the result, which is the case whenever C reports an outcome the Haskell type does
-- not want verbatim.
--
-- The function is an 'Assembler', so on a spec with outputs it takes one argument
-- per output and then the C return, exactly as 'HsBindgen.HighLevel.resultPure' does.
-- An authenticated-decryption call is that shape: the plaintext arrives in an
-- out-parameter, and the status decides whether it is worth anything. This one is
-- taken from the @libsodium@ example:
--
-- > open :: Key -> Nonce -> ByteString -> IO (Maybe ByteString)
-- > open key nonce ciphertext = toHighLevel
-- >   ( output (byteStringOut (BS.length ciphertext - macBytes)) -- unsigned char *m
-- >   $ input2 unsafeByteStringLenIn                             -- c, clen
-- >   $ autoWith (\\plaintext status ->                          -- n, k
-- >       if status == 0 then Just plaintext else Nothing)
-- >   ) crypto_secretbox_open_easy ciphertext nonce key
--
-- Compare 'autoChecked', which is the same position filled by a check rather than an
-- assembler, for a status that means the call /failed/ rather than one that is part
-- of the answer.
--
-- There is no @IO@ counterpart on purpose: reach for
-- @'autoInputs' '$' 'HsBindgen.HighLevel.resultIO' f@, which reads the same and keeps
-- the closer vocabulary in one place.
--
autoWith
  :: forall os lo hi c hs.
    ( AutoInputs (TakesArgument hi) lo (IO c) hi (IO hs)
    , ApplyOutputs os
    )
  => Assembler os (c -> hs)
  -> ToHighLevel os lo hi
-- Written applied rather than as @autoInputs . resultPure@: composing them leaves
-- 'autoInputs' free to stop anywhere, so nothing ties its @lo'@ and @hiEnd@ to the
-- closer's @IO c@ and @IO hs@ and both come out ambiguous. Naming them here is what
-- makes the two halves line up.
autoWith f = autoInputs @os @lo @(IO c) @hi @(IO hs) (resultPure f)
{-# INLINE autoWith #-}

-- | Fill the wrapper's arguments from its signature, then close on a status:
-- @'autoInputs' ('checkedResult' check)@.
--
-- This fits a call of the shape 'checkedResult' describes whose remaining arguments
-- are also ordinary, which between them covers a good deal of a typical C library.
-- An authenticated-encryption call writes the ciphertext into an out-parameter and
-- reports success in its return value (again from @examples\/libsodium@):
--
-- > encrypt :: Key -> Nonce -> ByteString -> IO ByteString
-- > encrypt key nonce message = toHighLevel
-- >   ( output (byteStringOut (macBytes + BS.length message)) -- unsigned char *c
-- >   $ input2 unsafeByteStringLenIn                          -- m, mlen
-- >   $ autoChecked (checkStatus "crypto_secretbox_easy")     -- n, k
-- >   ) crypto_secretbox_easy message nonce key
--
-- Everything 'checkedResult' says applies, the check running before the read-backs
-- included. A binding that names its own check once may find
-- @'autoInputs' '$' theCheck@ reads better; both spell the same spec.
--
-- Compare 'autoWith', which fills the same position with an assembler instead of a
-- check. The two differ in what the status means: here a non-zero one means the call
-- failed and nothing is worth reading back, whereas 'autoWith' suits a status that is
-- part of the answer. Encryption and decryption make the pair, since a rejected
-- ciphertext is expected input rather than a failure, so the example above uses this
-- and the one at 'autoWith' uses that.
--
-- See 'Checked' for stating this constraint in a closer of your own.
--
autoChecked
  :: forall os lo hi c hs.
    ( AutoInputs (TakesArgument hi) lo (IO c) hi (IO hs)
    , Checked os hs
    )
  => (c -> IO ())               -- ^ inspect the C return; throw to reject it
  -> ToHighLevel os lo hi
-- Applied, not composed, for the reason given at 'autoWith'.
autoChecked check = autoInputs @os @lo @(IO c) @hi @(IO hs) (checkedResult check)
{-# INLINE autoChecked #-}

-- | The recursion behind 'auto', indexed by the step 'AutoStepFor' chose. Write
-- 'auto' in a spec; name this class only to demand that some other spec can be
-- completed automatically.
class AutoAt (step :: AutoStep) os lo hi where
  autoAt :: ToHighLevel os lo hi

-- The wrapper still takes an argument: fill C positions with its default and recurse.
instance ( DefaultIn a lo lo'
         , ThreadIn hi'
         , hi ~ (a -> hi')
         , AutoAt (AutoStepFor hi' lo') os lo' hi'
         )
      => AutoAt 'FillInput os lo hi where
  autoAt = inputN (defaultIn @a @lo @lo') (auto @os @lo' @hi')
  {-# INLINE autoAt #-}

-- Both sides have reached the result: close the spec.
instance ( lo ~ IO c
         , hi ~ IO hs
         , AutoResult (IsVoid c) os c hs
         )
      => AutoAt 'Close os lo hi where
  autoAt = autoResult @os @c @hs
  {-# INLINE autoAt #-}

-- hi bottomed out at a result while lo still takes arguments.
instance TypeError (AutoMismatch hi lo) => AutoAt 'Leftover os lo hi where
  autoAt = unreachable
