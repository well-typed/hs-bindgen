{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module RPM.Types where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.Prelude
import qualified Text.Read
import Data.Bits (FiniteBits)
import Data.Void (Void)
import Prelude ((<*>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure, showsPrec)

{-| __C declaration:__ @errmsg_t@

    __defined at:__ @rpm\/rpmtypes.h:17:25@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Errmsg_t = Errmsg_t
  { un_Errmsg_t :: Ptr.Ptr FC.CChar
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @headerToken_s@

    __defined at:__ @rpm\/rpmtypes.h:24:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data HeaderToken_s

{-|

  > rpmtypes

  RPM header and data retrieval types. @ {

__C declaration:__ @Header@

__defined at:__ @rpm\/rpmtypes.h:24:32@

__exported by:__ @rpm\/rpmtypes.h@
-}
newtype Header = Header
  { un_Header :: Ptr.Ptr HeaderToken_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @headerIterator_s@

    __defined at:__ @rpm\/rpmtypes.h:25:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data HeaderIterator_s

{-| __C declaration:__ @HeaderIterator@

    __defined at:__ @rpm\/rpmtypes.h:25:35@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype HeaderIterator = HeaderIterator
  { un_HeaderIterator :: Ptr.Ptr HeaderIterator_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpm_tag_t@

    __defined at:__ @rpm\/rpmtypes.h:27:18@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpm_tag_t = Rpm_tag_t
  { un_Rpm_tag_t :: HsBindgen.Runtime.Prelude.Word32
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @rpm_tagtype_t@

    __defined at:__ @rpm\/rpmtypes.h:28:18@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpm_tagtype_t = Rpm_tagtype_t
  { un_Rpm_tagtype_t :: HsBindgen.Runtime.Prelude.Word32
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @rpm_count_t@

    __defined at:__ @rpm\/rpmtypes.h:29:18@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpm_count_t = Rpm_count_t
  { un_Rpm_count_t :: HsBindgen.Runtime.Prelude.Word32
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @rpmTagVal@

    __defined at:__ @rpm\/rpmtypes.h:30:19@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype RpmTagVal = RpmTagVal
  { un_RpmTagVal :: Rpm_tag_t
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @rpmDbiTagVal@

    __defined at:__ @rpm\/rpmtypes.h:31:19@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype RpmDbiTagVal = RpmDbiTagVal
  { un_RpmDbiTagVal :: Rpm_tag_t
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @rpm_data_t@

    __defined at:__ @rpm\/rpmtypes.h:33:17@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpm_data_t = Rpm_data_t
  { un_Rpm_data_t :: Ptr.Ptr Void
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpm_constdata_t@

    __defined at:__ @rpm\/rpmtypes.h:34:22@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpm_constdata_t = Rpm_constdata_t
  { un_Rpm_constdata_t :: Ptr.Ptr Void
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmtd_s@

    __defined at:__ @rpm\/rpmtypes.h:36:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data Rpmtd_s

{-| __C declaration:__ @rpmtd@

    __defined at:__ @rpm\/rpmtypes.h:36:26@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpmtd = Rpmtd
  { un_Rpmtd :: Ptr.Ptr Rpmtd_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpm_color_t@

    __defined at:__ @rpm\/rpmtypes.h:38:18@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpm_color_t = Rpm_color_t
  { un_Rpm_color_t :: HsBindgen.Runtime.Prelude.Word32
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @rpm_flag_t@

    __defined at:__ @rpm\/rpmtypes.h:39:18@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpm_flag_t = Rpm_flag_t
  { un_Rpm_flag_t :: HsBindgen.Runtime.Prelude.Word32
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @rpm_tid_t@

    __defined at:__ @rpm\/rpmtypes.h:40:18@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpm_tid_t = Rpm_tid_t
  { un_Rpm_tid_t :: HsBindgen.Runtime.Prelude.Word32
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @rpmFlags@

    __defined at:__ @rpm\/rpmtypes.h:42:18@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype RpmFlags = RpmFlags
  { un_RpmFlags :: HsBindgen.Runtime.Prelude.Word32
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-|

  > rpmtypes

  In-header hardcoded sizes for various POSIXy types @ {

__C declaration:__ @rpm_off_t@

__defined at:__ @rpm\/rpmtypes.h:50:18@

__exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpm_off_t = Rpm_off_t
  { un_Rpm_off_t :: HsBindgen.Runtime.Prelude.Word32
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @rpm_loff_t@

    __defined at:__ @rpm\/rpmtypes.h:51:18@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpm_loff_t = Rpm_loff_t
  { un_Rpm_loff_t :: HsBindgen.Runtime.Prelude.Word64
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @rpm_time_t@

    __defined at:__ @rpm\/rpmtypes.h:52:18@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpm_time_t = Rpm_time_t
  { un_Rpm_time_t :: HsBindgen.Runtime.Prelude.Word32
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @rpm_mode_t@

    __defined at:__ @rpm\/rpmtypes.h:53:18@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpm_mode_t = Rpm_mode_t
  { un_Rpm_mode_t :: HsBindgen.Runtime.Prelude.Word16
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @rpm_rdev_t@

    __defined at:__ @rpm\/rpmtypes.h:54:18@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpm_rdev_t = Rpm_rdev_t
  { un_Rpm_rdev_t :: HsBindgen.Runtime.Prelude.Word16
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @rpm_dev_t@

    __defined at:__ @rpm\/rpmtypes.h:55:18@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpm_dev_t = Rpm_dev_t
  { un_Rpm_dev_t :: HsBindgen.Runtime.Prelude.Word32
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @rpm_ino_t@

    __defined at:__ @rpm\/rpmtypes.h:56:18@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpm_ino_t = Rpm_ino_t
  { un_Rpm_ino_t :: HsBindgen.Runtime.Prelude.Word32
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @rpmts_s@

    __defined at:__ @rpm\/rpmtypes.h:63:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data Rpmts_s

{-|

  > rpmtypes

  The main types involved in transaction manipulation @ {

__C declaration:__ @rpmts@

__defined at:__ @rpm\/rpmtypes.h:63:26@

__exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpmts = Rpmts
  { un_Rpmts :: Ptr.Ptr Rpmts_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmte_s@

    __defined at:__ @rpm\/rpmtypes.h:64:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data Rpmte_s

{-| __C declaration:__ @rpmte@

    __defined at:__ @rpm\/rpmtypes.h:64:26@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpmte = Rpmte
  { un_Rpmte :: Ptr.Ptr Rpmte_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmds_s@

    __defined at:__ @rpm\/rpmtypes.h:65:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data Rpmds_s

{-| __C declaration:__ @rpmds@

    __defined at:__ @rpm\/rpmtypes.h:65:26@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpmds = Rpmds
  { un_Rpmds :: Ptr.Ptr Rpmds_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmfi_s@

    __defined at:__ @rpm\/rpmtypes.h:66:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data Rpmfi_s

{-| __C declaration:__ @rpmfi@

    __defined at:__ @rpm\/rpmtypes.h:66:26@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpmfi = Rpmfi
  { un_Rpmfi :: Ptr.Ptr Rpmfi_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmfiles_s@

    __defined at:__ @rpm\/rpmtypes.h:67:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data Rpmfiles_s

{-| __C declaration:__ @rpmfiles@

    __defined at:__ @rpm\/rpmtypes.h:67:29@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpmfiles = Rpmfiles
  { un_Rpmfiles :: Ptr.Ptr Rpmfiles_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmdb_s@

    __defined at:__ @rpm\/rpmtypes.h:68:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data Rpmdb_s

{-| __C declaration:__ @rpmdb@

    __defined at:__ @rpm\/rpmtypes.h:68:26@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpmdb = Rpmdb
  { un_Rpmdb :: Ptr.Ptr Rpmdb_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmdbMatchIterator_s@

    __defined at:__ @rpm\/rpmtypes.h:69:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data RpmdbMatchIterator_s

{-| __C declaration:__ @rpmdbMatchIterator@

    __defined at:__ @rpm\/rpmtypes.h:69:39@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype RpmdbMatchIterator = RpmdbMatchIterator
  { un_RpmdbMatchIterator :: Ptr.Ptr RpmdbMatchIterator_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmtsi_s@

    __defined at:__ @rpm\/rpmtypes.h:70:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data Rpmtsi_s

{-| __C declaration:__ @rpmtsi@

    __defined at:__ @rpm\/rpmtypes.h:70:27@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpmtsi = Rpmtsi
  { un_Rpmtsi :: Ptr.Ptr Rpmtsi_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmps_s@

    __defined at:__ @rpm\/rpmtypes.h:71:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data Rpmps_s

{-| __C declaration:__ @rpmps@

    __defined at:__ @rpm\/rpmtypes.h:71:26@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpmps = Rpmps
  { un_Rpmps :: Ptr.Ptr Rpmps_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmtxn_s@

    __defined at:__ @rpm\/rpmtypes.h:72:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data Rpmtxn_s

{-| __C declaration:__ @rpmtxn@

    __defined at:__ @rpm\/rpmtypes.h:72:27@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpmtxn = Rpmtxn
  { un_Rpmtxn :: Ptr.Ptr Rpmtxn_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmver_s@

    __defined at:__ @rpm\/rpmtypes.h:73:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data Rpmver_s

{-| __C declaration:__ @rpmver@

    __defined at:__ @rpm\/rpmtypes.h:73:27@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpmver = Rpmver
  { un_Rpmver :: Ptr.Ptr Rpmver_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmdbIndexIterator_s@

    __defined at:__ @rpm\/rpmtypes.h:75:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data RpmdbIndexIterator_s

{-| __C declaration:__ @rpmdbIndexIterator@

    __defined at:__ @rpm\/rpmtypes.h:75:39@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype RpmdbIndexIterator = RpmdbIndexIterator
  { un_RpmdbIndexIterator :: Ptr.Ptr RpmdbIndexIterator_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @fnpyKey@

    __defined at:__ @rpm\/rpmtypes.h:76:22@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype FnpyKey = FnpyKey
  { un_FnpyKey :: Ptr.Ptr Void
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmCallbackData@

    __defined at:__ @rpm\/rpmtypes.h:77:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype RpmCallbackData = RpmCallbackData
  { un_RpmCallbackData :: Ptr.Ptr Void
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmPubkey_s@

    __defined at:__ @rpm\/rpmtypes.h:80:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data RpmPubkey_s

{-| @ }

__C declaration:__ @rpmPubkey@

__defined at:__ @rpm\/rpmtypes.h:80:30@

__exported by:__ @rpm\/rpmtypes.h@
-}
newtype RpmPubkey = RpmPubkey
  { un_RpmPubkey :: Ptr.Ptr RpmPubkey_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmKeyring_s@

    __defined at:__ @rpm\/rpmtypes.h:81:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data RpmKeyring_s

{-| __C declaration:__ @rpmKeyring@

    __defined at:__ @rpm\/rpmtypes.h:81:31@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype RpmKeyring = RpmKeyring
  { un_RpmKeyring :: Ptr.Ptr RpmKeyring_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmKeyringIterator_s@

    __defined at:__ @rpm\/rpmtypes.h:82:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data RpmKeyringIterator_s

{-| __C declaration:__ @rpmKeyringIterator@

    __defined at:__ @rpm\/rpmtypes.h:82:39@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype RpmKeyringIterator = RpmKeyringIterator
  { un_RpmKeyringIterator :: Ptr.Ptr RpmKeyringIterator_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmsid@

    __defined at:__ @rpm\/rpmtypes.h:84:18@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpmsid = Rpmsid
  { un_Rpmsid :: HsBindgen.Runtime.Prelude.Word32
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @rpmstrPool_s@

    __defined at:__ @rpm\/rpmtypes.h:85:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data RpmstrPool_s

{-| __C declaration:__ @rpmstrPool@

    __defined at:__ @rpm\/rpmtypes.h:85:31@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype RpmstrPool = RpmstrPool
  { un_RpmstrPool :: Ptr.Ptr RpmstrPool_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmPlugin_s@

    __defined at:__ @rpm\/rpmtypes.h:87:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data RpmPlugin_s

{-| __C declaration:__ @rpmPlugin@

    __defined at:__ @rpm\/rpmtypes.h:87:30@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype RpmPlugin = RpmPlugin
  { un_RpmPlugin :: Ptr.Ptr RpmPlugin_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmPlugins_s@

    __defined at:__ @rpm\/rpmtypes.h:88:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data RpmPlugins_s

{-| __C declaration:__ @rpmPlugins@

    __defined at:__ @rpm\/rpmtypes.h:88:31@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype RpmPlugins = RpmPlugins
  { un_RpmPlugins :: Ptr.Ptr RpmPlugins_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmgi_s@

    __defined at:__ @rpm\/rpmtypes.h:90:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data Rpmgi_s

{-| __C declaration:__ @rpmgi@

    __defined at:__ @rpm\/rpmtypes.h:90:26@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype Rpmgi = Rpmgi
  { un_Rpmgi :: Ptr.Ptr Rpmgi_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmSpec_s@

    __defined at:__ @rpm\/rpmtypes.h:92:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data RpmSpec_s

{-| __C declaration:__ @rpmSpec@

    __defined at:__ @rpm\/rpmtypes.h:92:28@

    __exported by:__ @rpm\/rpmtypes.h@
-}
newtype RpmSpec = RpmSpec
  { un_RpmSpec :: Ptr.Ptr RpmSpec_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmRelocation@

    __defined at:__ @rpm\/rpmtypes.h:94:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data RpmRelocation

{-| __C declaration:__ @FD_s@

    __defined at:__ @rpm\/rpmtypes.h:100:16@

    __exported by:__ @rpm\/rpmtypes.h@
-}
data FD_s

{-|

  > rpmtypes

  RPM IO file descriptor type

__C declaration:__ @FD_t@

__defined at:__ @rpm\/rpmtypes.h:100:23@

__exported by:__ @rpm\/rpmtypes.h@
-}
newtype FD_t = FD_t
  { un_FD_t :: Ptr.Ptr FD_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-|

  > rpmtypes

  Package read return codes.

__C declaration:__ @rpmRC@

__defined at:__ @rpm\/rpmtypes.h:105:14@

__exported by:__ @rpm\/rpmtypes.h@
-}
newtype RpmRC = RpmRC
  { un_RpmRC :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable RpmRC where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure RpmRC
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          RpmRC un_RpmRC2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_RpmRC2

instance HsBindgen.Runtime.CEnum.CEnum RpmRC where

  type CEnumZ RpmRC = FC.CUInt

  toCEnum = RpmRC

  fromCEnum = un_RpmRC

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "RPMRC_OK")
                                                     , (1, Data.List.NonEmpty.singleton "RPMRC_NOTFOUND")
                                                     , (2, Data.List.NonEmpty.singleton "RPMRC_FAIL")
                                                     , (3, Data.List.NonEmpty.singleton "RPMRC_NOTTRUSTED")
                                                     , (4, Data.List.NonEmpty.singleton "RPMRC_NOKEY")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "RpmRC"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "RpmRC"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum RpmRC where

  minDeclaredValue = RPMRC_OK

  maxDeclaredValue = RPMRC_NOKEY

instance Show RpmRC where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read RpmRC where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| Generic success code

__C declaration:__ @RPMRC_OK@

__defined at:__ @rpm\/rpmtypes.h:106:5@

__exported by:__ @rpm\/rpmtypes.h@
-}
pattern RPMRC_OK :: RpmRC
pattern RPMRC_OK = RpmRC 0

{-| Generic not found code.

__C declaration:__ @RPMRC_NOTFOUND@

__defined at:__ @rpm\/rpmtypes.h:107:5@

__exported by:__ @rpm\/rpmtypes.h@
-}
pattern RPMRC_NOTFOUND :: RpmRC
pattern RPMRC_NOTFOUND = RpmRC 1

{-| Generic failure code.

__C declaration:__ @RPMRC_FAIL@

__defined at:__ @rpm\/rpmtypes.h:108:5@

__exported by:__ @rpm\/rpmtypes.h@
-}
pattern RPMRC_FAIL :: RpmRC
pattern RPMRC_FAIL = RpmRC 2

{-| Signature is OK, but key is not trusted.

__C declaration:__ @RPMRC_NOTTRUSTED@

__defined at:__ @rpm\/rpmtypes.h:109:5@

__exported by:__ @rpm\/rpmtypes.h@
-}
pattern RPMRC_NOTTRUSTED :: RpmRC
pattern RPMRC_NOTTRUSTED = RpmRC 3

{-| Public key is unavailable.

__C declaration:__ @RPMRC_NOKEY@

__defined at:__ @rpm\/rpmtypes.h:110:5@

__exported by:__ @rpm\/rpmtypes.h@
-}
pattern RPMRC_NOKEY :: RpmRC
pattern RPMRC_NOKEY = RpmRC 4
