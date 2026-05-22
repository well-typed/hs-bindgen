-- | Convenient re-exports for users of the high-level marshalling API.
--
-- > import HsBindgen.Runtime.HighLevel.Prelude qualified as HL
-- >
-- > hsStrlen :: String -> IO Int
-- > hsStrlen = HL.hl strlen
--
-- For the elaboration helpers ('Spread' and 'Thread'), import
-- "HsBindgen.Runtime.HighLevel.Call" directly. Most users never need
-- to mention either by name.
module HsBindgen.Runtime.HighLevel.Prelude (
    -- * Marshalling classes
    ToC (..)
  , FromC (..)
    -- * The combinator
  , hl
    -- * Const-pointer view of an array
  , ConstIncompleteArray (..)
    -- * Nullability
  , Nullable (..)
    -- * Result-side helpers
  , withOut
  , withBuf
  ) where

import HsBindgen.Runtime.HighLevel.Call (hl)
import HsBindgen.Runtime.HighLevel.FromC (FromC (..))
import HsBindgen.Runtime.HighLevel.Result (withBuf, withOut)
import HsBindgen.Runtime.HighLevel.ToC (ConstIncompleteArray (..),
                                        Nullable (..), ToC (..))
