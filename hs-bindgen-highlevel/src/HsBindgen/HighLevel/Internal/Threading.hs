-- | Holding a bracket open across a binding's remaining arguments.
--
-- This is the one piece of machinery every combinator in "HsBindgen.HighLevel" is
-- built on. You need it to write a combinator of your own; you never mention it to
-- use one, beyond the @ThreadIn hi@ constraint that shows up in their signatures.
module HsBindgen.HighLevel.Internal.Threading (
    ThreadIn (..)
  ) where

-- | Keep a bracket open until the C call runs.
--
-- Every combinator in a spec opens one: 'Foreign.C.String.withCString' for a string
-- argument, 'Foreign.Marshal.Alloc.alloca' for an out-parameter slot,
-- 'Foreign.Marshal.Array.allocaArray' for a scratch buffer. Each has to stay open
-- until the C call happens, and the C call happens only once the binding has been
-- given /all/ of its arguments. But the binding is curried, so the first argument
-- arrives long before the last, and a bracket opened where the value is marshalled
-- would close long before the call.
--
-- 'threadIn' pushes the bracket past the arguments instead. Read its type as: /given
-- a bracket supplying an @a@, and a way to turn that @a@ into the high-level type @hi@,
-- build @hi@ with the bracket open around the point where @hi@ finally reaches 'IO'/.
--
-- The two instances walk @hi@\'s arrows. At @hi ~ Int -> Bool -> IO r@:
--
-- > threadIn br f                            -- hi = Int -> Bool -> IO r
-- >   = \i   -> threadIn br (\a -> f a i)    -- hi = Bool -> IO r   (function instance)
-- >   = \i j -> threadIn br (\a -> f a i j)  -- hi = IO r           (function instance)
-- >   = \i j -> br          (\a -> f a i j)  -- open the bracket    (IO instance)
--
-- Each function instance peels one argument off @hi@ and tucks it into the
-- continuation; the 'IO' instance is where the bracket actually opens. So the
-- bracket opens once the last argument has arrived and closes when the resulting 'IO'
-- action completes, which is exactly the window the C call needs. Nesting several
-- 'threadIn's, one per combinator, opens them outermost-first and closes them in the
-- reverse order, so a throw anywhere unwinds all of them.
--
-- The class is indexed on the high-level type alone. What the bracket supplies is
-- quantified in the method, because neither instance looks at it: the recursion walks
-- @hi@ and hands the bracketed value straight through. That is what lets @output@ ask
-- for @ThreadIn hi@ without naming the pair of C residual and pending read-backs it
-- happens to thread (see 'HsBindgen.HighLevel.output').
class ThreadIn hi where
  threadIn :: forall a. (forall r. (a -> IO r) -> IO r) -> (a -> hi) -> hi

instance ThreadIn (IO r) where
  threadIn br f = br f
  {-# INLINE threadIn #-}

instance ThreadIn rest => ThreadIn (arg -> rest) where
  threadIn br f = \arg -> threadIn br (\a -> f a arg)
  {-# INLINE threadIn #-}
