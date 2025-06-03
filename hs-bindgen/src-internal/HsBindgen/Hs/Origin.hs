-- | Origin information
--
-- When we generate Haskell code, we keep a record of their \"origin\": what
-- part of the C input corresponds to these definitions? We need this
-- information for various purposes:
--
-- * Generating tests
-- * Generating external binding specification
-- * Generating documentation
--
-- Intended for qualified import.
--
-- > import HsBindgen.Hs.Origin qualified as Origin
module HsBindgen.Hs.Origin (
    -- * Declarations
    Decl(..)
  , Struct(..)
  , Newtype(..)
  , EmptyData(..)
    -- * Fields
  , Field(..)
  ) where

import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

data Decl a = Decl{
      declInfo :: C.DeclInfo
    , declKind :: a            -- ^ Kind-specific information
    , declSpec :: C.DeclSpec
    }
  deriving stock (Generic, Show)

data Struct =
    Struct C.Struct
  deriving stock (Generic, Show)

data Newtype =
    Enum    C.Enum
  | Typedef C.Typedef
  | Union   C.Union
  | Macro   C.CheckedMacroType
  deriving stock (Generic, Show)

data EmptyData =
    OpaqueStruct
  | OpaqueEnum
  deriving stock (Generic, Show)

{-------------------------------------------------------------------------------
  Fields
-------------------------------------------------------------------------------}

data Field =
    GeneratedField             -- ^ Field without a direct counterpart in C
  | StructField C.StructField
  deriving stock (Generic, Show)

