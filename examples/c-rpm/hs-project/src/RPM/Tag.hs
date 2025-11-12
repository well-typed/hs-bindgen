{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module RPM.Tag where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CEnum
import qualified RPM.Types
import qualified Text.Read
import Data.Bits (FiniteBits)
import Prelude ((<*>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure, showsPrec)

{-| __C declaration:__ @HEADER_IMAGE@

    __defined at:__ @rpm\/rpmtag.h:20:9@

    __exported by:__ @rpm\/rpmtag.h@
-}
hEADER_IMAGE :: FC.CInt
hEADER_IMAGE = (61 :: FC.CInt)

{-| __C declaration:__ @HEADER_SIGNATURES@

    __defined at:__ @rpm\/rpmtag.h:21:9@

    __exported by:__ @rpm\/rpmtag.h@
-}
hEADER_SIGNATURES :: FC.CInt
hEADER_SIGNATURES = (62 :: FC.CInt)

{-| __C declaration:__ @HEADER_IMMUTABLE@

    __defined at:__ @rpm\/rpmtag.h:22:9@

    __exported by:__ @rpm\/rpmtag.h@
-}
hEADER_IMMUTABLE :: FC.CInt
hEADER_IMMUTABLE = (63 :: FC.CInt)

{-| __C declaration:__ @HEADER_REGIONS@

    __defined at:__ @rpm\/rpmtag.h:23:9@

    __exported by:__ @rpm\/rpmtag.h@
-}
hEADER_REGIONS :: FC.CInt
hEADER_REGIONS = (64 :: FC.CInt)

{-| __C declaration:__ @HEADER_I18NTABLE@

    __defined at:__ @rpm\/rpmtag.h:24:9@

    __exported by:__ @rpm\/rpmtag.h@
-}
hEADER_I18NTABLE :: FC.CInt
hEADER_I18NTABLE = (100 :: FC.CInt)

{-| __C declaration:__ @HEADER_SIGBASE@

    __defined at:__ @rpm\/rpmtag.h:25:9@

    __exported by:__ @rpm\/rpmtag.h@
-}
hEADER_SIGBASE :: FC.CInt
hEADER_SIGBASE = (256 :: FC.CInt)

{-| __C declaration:__ @HEADER_SIGTOP@

    __defined at:__ @rpm\/rpmtag.h:26:9@

    __exported by:__ @rpm\/rpmtag.h@
-}
hEADER_SIGTOP :: FC.CInt
hEADER_SIGTOP = (999 :: FC.CInt)

{-| __C declaration:__ @HEADER_TAGBASE@

    __defined at:__ @rpm\/rpmtag.h:27:9@

    __exported by:__ @rpm\/rpmtag.h@
-}
hEADER_TAGBASE :: FC.CInt
hEADER_TAGBASE = (1000 :: FC.CInt)

{-|

  > rpmtag

  Tags identify data in package headers.

  __Note:__ tags should not have value 0!

  __Note:__ all new tags should be added above 5000

  __TODO:__

  : Somehow supply type *

__C declaration:__ @rpmTag@

__defined at:__ @rpm\/rpmtag.h:35:14@

__exported by:__ @rpm\/rpmtag.h@
-}
newtype RpmTag = RpmTag
  { un_RpmTag :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable RpmTag where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure RpmTag
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          RpmTag un_RpmTag2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_RpmTag2

instance HsBindgen.Runtime.CEnum.CEnum RpmTag where

  type CEnumZ RpmTag = FC.CUInt

  toCEnum = RpmTag

  fromCEnum = un_RpmTag

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (61, Data.List.NonEmpty.singleton "RPMTAG_HEADERIMAGE")
                                                     , (62, Data.List.NonEmpty.singleton "RPMTAG_HEADERSIGNATURES")
                                                     , (63, Data.List.NonEmpty.singleton "RPMTAG_HEADERIMMUTABLE")
                                                     , (64, Data.List.NonEmpty.singleton "RPMTAG_HEADERREGIONS")
                                                     , (100, Data.List.NonEmpty.singleton "RPMTAG_HEADERI18NTABLE")
                                                     , (256, Data.List.NonEmpty.singleton "RPMTAG_SIG_BASE")
                                                     , (257, Data.List.NonEmpty.singleton "RPMTAG_SIGSIZE")
                                                     , (258, Data.List.NonEmpty.singleton "RPMTAG_SIGLEMD5_1")
                                                     , (259, Data.List.NonEmpty.singleton "RPMTAG_SIGPGP")
                                                     , (260, Data.List.NonEmpty.singleton "RPMTAG_SIGLEMD5_2")
                                                     , (261, Data.List.NonEmpty.singleton "RPMTAG_SIGMD5")
                                                     , (262, Data.List.NonEmpty.singleton "RPMTAG_SIGGPG")
                                                     , (263, Data.List.NonEmpty.singleton "RPMTAG_SIGPGP5")
                                                     , (264, Data.List.NonEmpty.singleton "RPMTAG_BADSHA1_1")
                                                     , (265, Data.List.NonEmpty.singleton "RPMTAG_BADSHA1_2")
                                                     , (266, Data.List.NonEmpty.singleton "RPMTAG_PUBKEYS")
                                                     , (267, Data.List.NonEmpty.singleton "RPMTAG_DSAHEADER")
                                                     , (268, Data.List.NonEmpty.singleton "RPMTAG_RSAHEADER")
                                                     , (269, Data.List.NonEmpty.singleton "RPMTAG_SHA1HEADER")
                                                     , (270, Data.List.NonEmpty.singleton "RPMTAG_LONGSIGSIZE")
                                                     , (271, Data.List.NonEmpty.singleton "RPMTAG_LONGARCHIVESIZE")
                                                     , (273, Data.List.NonEmpty.singleton "RPMTAG_SHA256HEADER")
                                                     , (276, Data.List.NonEmpty.singleton "RPMTAG_VERITYSIGNATURES")
                                                     , (277, Data.List.NonEmpty.singleton "RPMTAG_VERITYSIGNATUREALGO")
                                                     , (278, Data.List.NonEmpty.singleton "RPMTAG_OPENPGP")
                                                     , (279, Data.List.NonEmpty.singleton "RPMTAG_SHA3_256HEADER")
                                                     , (999, Data.List.NonEmpty.singleton "RPMTAG_SIG_TOP")
                                                     , (1000, Data.List.NonEmpty.singleton "RPMTAG_NAME")
                                                     , (1001, Data.List.NonEmpty.singleton "RPMTAG_VERSION")
                                                     , (1002, Data.List.NonEmpty.singleton "RPMTAG_RELEASE")
                                                     , (1003, Data.List.NonEmpty.singleton "RPMTAG_EPOCH")
                                                     , (1004, Data.List.NonEmpty.singleton "RPMTAG_SUMMARY")
                                                     , (1005, Data.List.NonEmpty.singleton "RPMTAG_DESCRIPTION")
                                                     , (1006, Data.List.NonEmpty.singleton "RPMTAG_BUILDTIME")
                                                     , (1007, Data.List.NonEmpty.singleton "RPMTAG_BUILDHOST")
                                                     , (1008, Data.List.NonEmpty.singleton "RPMTAG_INSTALLTIME")
                                                     , (1009, Data.List.NonEmpty.singleton "RPMTAG_SIZE")
                                                     , (1010, Data.List.NonEmpty.singleton "RPMTAG_DISTRIBUTION")
                                                     , (1011, Data.List.NonEmpty.singleton "RPMTAG_VENDOR")
                                                     , (1012, Data.List.NonEmpty.singleton "RPMTAG_GIF")
                                                     , (1013, Data.List.NonEmpty.singleton "RPMTAG_XPM")
                                                     , (1014, Data.List.NonEmpty.singleton "RPMTAG_LICENSE")
                                                     , (1015, Data.List.NonEmpty.singleton "RPMTAG_PACKAGER")
                                                     , (1016, Data.List.NonEmpty.singleton "RPMTAG_GROUP")
                                                     , (1017, Data.List.NonEmpty.singleton "RPMTAG_CHANGELOG")
                                                     , (1018, Data.List.NonEmpty.singleton "RPMTAG_SOURCE")
                                                     , (1019, Data.List.NonEmpty.singleton "RPMTAG_PATCH")
                                                     , (1020, Data.List.NonEmpty.singleton "RPMTAG_URL")
                                                     , (1021, Data.List.NonEmpty.singleton "RPMTAG_OS")
                                                     , (1022, Data.List.NonEmpty.singleton "RPMTAG_ARCH")
                                                     , (1023, Data.List.NonEmpty.singleton "RPMTAG_PREIN")
                                                     , (1024, Data.List.NonEmpty.singleton "RPMTAG_POSTIN")
                                                     , (1025, Data.List.NonEmpty.singleton "RPMTAG_PREUN")
                                                     , (1026, Data.List.NonEmpty.singleton "RPMTAG_POSTUN")
                                                     , (1027, Data.List.NonEmpty.singleton "RPMTAG_OLDFILENAMES")
                                                     , (1028, Data.List.NonEmpty.singleton "RPMTAG_FILESIZES")
                                                     , (1029, Data.List.NonEmpty.singleton "RPMTAG_FILESTATES")
                                                     , (1030, Data.List.NonEmpty.singleton "RPMTAG_FILEMODES")
                                                     , (1031, Data.List.NonEmpty.singleton "RPMTAG_FILEUIDS")
                                                     , (1032, Data.List.NonEmpty.singleton "RPMTAG_FILEGIDS")
                                                     , (1033, Data.List.NonEmpty.singleton "RPMTAG_FILERDEVS")
                                                     , (1034, Data.List.NonEmpty.singleton "RPMTAG_FILEMTIMES")
                                                     , (1035, Data.List.NonEmpty.singleton "RPMTAG_FILEDIGESTS")
                                                     , (1036, Data.List.NonEmpty.singleton "RPMTAG_FILELINKTOS")
                                                     , (1037, Data.List.NonEmpty.singleton "RPMTAG_FILEFLAGS")
                                                     , (1038, Data.List.NonEmpty.singleton "RPMTAG_ROOT")
                                                     , (1039, Data.List.NonEmpty.singleton "RPMTAG_FILEUSERNAME")
                                                     , (1040, Data.List.NonEmpty.singleton "RPMTAG_FILEGROUPNAME")
                                                     , (1041, Data.List.NonEmpty.singleton "RPMTAG_EXCLUDE")
                                                     , (1042, Data.List.NonEmpty.singleton "RPMTAG_EXCLUSIVE")
                                                     , (1043, Data.List.NonEmpty.singleton "RPMTAG_ICON")
                                                     , (1044, Data.List.NonEmpty.singleton "RPMTAG_SOURCERPM")
                                                     , (1045, Data.List.NonEmpty.singleton "RPMTAG_FILEVERIFYFLAGS")
                                                     , (1046, Data.List.NonEmpty.singleton "RPMTAG_ARCHIVESIZE")
                                                     , (1047, Data.List.NonEmpty.singleton "RPMTAG_PROVIDENAME")
                                                     , (1048, Data.List.NonEmpty.singleton "RPMTAG_REQUIREFLAGS")
                                                     , (1049, Data.List.NonEmpty.singleton "RPMTAG_REQUIRENAME")
                                                     , (1050, Data.List.NonEmpty.singleton "RPMTAG_REQUIREVERSION")
                                                     , (1051, Data.List.NonEmpty.singleton "RPMTAG_NOSOURCE")
                                                     , (1052, Data.List.NonEmpty.singleton "RPMTAG_NOPATCH")
                                                     , (1053, Data.List.NonEmpty.singleton "RPMTAG_CONFLICTFLAGS")
                                                     , (1054, Data.List.NonEmpty.singleton "RPMTAG_CONFLICTNAME")
                                                     , (1055, Data.List.NonEmpty.singleton "RPMTAG_CONFLICTVERSION")
                                                     , (1056, Data.List.NonEmpty.singleton "RPMTAG_DEFAULTPREFIX")
                                                     , (1057, Data.List.NonEmpty.singleton "RPMTAG_BUILDROOT")
                                                     , (1058, Data.List.NonEmpty.singleton "RPMTAG_INSTALLPREFIX")
                                                     , (1059, Data.List.NonEmpty.singleton "RPMTAG_EXCLUDEARCH")
                                                     , (1060, Data.List.NonEmpty.singleton "RPMTAG_EXCLUDEOS")
                                                     , (1061, Data.List.NonEmpty.singleton "RPMTAG_EXCLUSIVEARCH")
                                                     , (1062, Data.List.NonEmpty.singleton "RPMTAG_EXCLUSIVEOS")
                                                     , (1063, Data.List.NonEmpty.singleton "RPMTAG_AUTOREQPROV")
                                                     , (1064, Data.List.NonEmpty.singleton "RPMTAG_RPMVERSION")
                                                     , (1065, Data.List.NonEmpty.singleton "RPMTAG_TRIGGERSCRIPTS")
                                                     , (1066, Data.List.NonEmpty.singleton "RPMTAG_TRIGGERNAME")
                                                     , (1067, Data.List.NonEmpty.singleton "RPMTAG_TRIGGERVERSION")
                                                     , (1068, Data.List.NonEmpty.singleton "RPMTAG_TRIGGERFLAGS")
                                                     , (1069, Data.List.NonEmpty.singleton "RPMTAG_TRIGGERINDEX")
                                                     , (1079, Data.List.NonEmpty.singleton "RPMTAG_VERIFYSCRIPT")
                                                     , (1080, Data.List.NonEmpty.singleton "RPMTAG_CHANGELOGTIME")
                                                     , (1081, Data.List.NonEmpty.singleton "RPMTAG_CHANGELOGNAME")
                                                     , (1082, Data.List.NonEmpty.singleton "RPMTAG_CHANGELOGTEXT")
                                                     , (1083, Data.List.NonEmpty.singleton "RPMTAG_BROKENMD5")
                                                     , (1084, Data.List.NonEmpty.singleton "RPMTAG_PREREQ")
                                                     , (1085, Data.List.NonEmpty.singleton "RPMTAG_PREINPROG")
                                                     , (1086, Data.List.NonEmpty.singleton "RPMTAG_POSTINPROG")
                                                     , (1087, Data.List.NonEmpty.singleton "RPMTAG_PREUNPROG")
                                                     , (1088, Data.List.NonEmpty.singleton "RPMTAG_POSTUNPROG")
                                                     , (1089, Data.List.NonEmpty.singleton "RPMTAG_BUILDARCHS")
                                                     , (1090, Data.List.NonEmpty.singleton "RPMTAG_OBSOLETENAME")
                                                     , (1091, Data.List.NonEmpty.singleton "RPMTAG_VERIFYSCRIPTPROG")
                                                     , (1092, Data.List.NonEmpty.singleton "RPMTAG_TRIGGERSCRIPTPROG")
                                                     , (1093, Data.List.NonEmpty.singleton "RPMTAG_DOCDIR")
                                                     , (1094, Data.List.NonEmpty.singleton "RPMTAG_COOKIE")
                                                     , (1095, Data.List.NonEmpty.singleton "RPMTAG_FILEDEVICES")
                                                     , (1096, Data.List.NonEmpty.singleton "RPMTAG_FILEINODES")
                                                     , (1097, Data.List.NonEmpty.singleton "RPMTAG_FILELANGS")
                                                     , (1098, Data.List.NonEmpty.singleton "RPMTAG_PREFIXES")
                                                     , (1099, Data.List.NonEmpty.singleton "RPMTAG_INSTPREFIXES")
                                                     , (1100, Data.List.NonEmpty.singleton "RPMTAG_TRIGGERIN")
                                                     , (1101, Data.List.NonEmpty.singleton "RPMTAG_TRIGGERUN")
                                                     , (1102, Data.List.NonEmpty.singleton "RPMTAG_TRIGGERPOSTUN")
                                                     , (1103, Data.List.NonEmpty.singleton "RPMTAG_AUTOREQ")
                                                     , (1104, Data.List.NonEmpty.singleton "RPMTAG_AUTOPROV")
                                                     , (1105, Data.List.NonEmpty.singleton "RPMTAG_CAPABILITY")
                                                     , (1106, Data.List.NonEmpty.singleton "RPMTAG_SOURCEPACKAGE")
                                                     , (1107, Data.List.NonEmpty.singleton "RPMTAG_OLDORIGFILENAMES")
                                                     , (1108, Data.List.NonEmpty.singleton "RPMTAG_BUILDPREREQ")
                                                     , (1109, Data.List.NonEmpty.singleton "RPMTAG_BUILDREQUIRES")
                                                     , (1110, Data.List.NonEmpty.singleton "RPMTAG_BUILDCONFLICTS")
                                                     , (1111, Data.List.NonEmpty.singleton "RPMTAG_BUILDMACROS")
                                                     , (1112, Data.List.NonEmpty.singleton "RPMTAG_PROVIDEFLAGS")
                                                     , (1113, Data.List.NonEmpty.singleton "RPMTAG_PROVIDEVERSION")
                                                     , (1114, Data.List.NonEmpty.singleton "RPMTAG_OBSOLETEFLAGS")
                                                     , (1115, Data.List.NonEmpty.singleton "RPMTAG_OBSOLETEVERSION")
                                                     , (1116, Data.List.NonEmpty.singleton "RPMTAG_DIRINDEXES")
                                                     , (1117, Data.List.NonEmpty.singleton "RPMTAG_BASENAMES")
                                                     , (1118, Data.List.NonEmpty.singleton "RPMTAG_DIRNAMES")
                                                     , (1119, Data.List.NonEmpty.singleton "RPMTAG_ORIGDIRINDEXES")
                                                     , (1120, Data.List.NonEmpty.singleton "RPMTAG_ORIGBASENAMES")
                                                     , (1121, Data.List.NonEmpty.singleton "RPMTAG_ORIGDIRNAMES")
                                                     , (1122, Data.List.NonEmpty.singleton "RPMTAG_OPTFLAGS")
                                                     , (1123, Data.List.NonEmpty.singleton "RPMTAG_DISTURL")
                                                     , (1124, Data.List.NonEmpty.singleton "RPMTAG_PAYLOADFORMAT")
                                                     , (1125, Data.List.NonEmpty.singleton "RPMTAG_PAYLOADCOMPRESSOR")
                                                     , (1126, Data.List.NonEmpty.singleton "RPMTAG_PAYLOADFLAGS")
                                                     , (1127, Data.List.NonEmpty.singleton "RPMTAG_INSTALLCOLOR")
                                                     , (1128, Data.List.NonEmpty.singleton "RPMTAG_INSTALLTID")
                                                     , (1129, Data.List.NonEmpty.singleton "RPMTAG_REMOVETID")
                                                     , (1130, Data.List.NonEmpty.singleton "RPMTAG_SHA1RHN")
                                                     , (1131, Data.List.NonEmpty.singleton "RPMTAG_RHNPLATFORM")
                                                     , (1132, Data.List.NonEmpty.singleton "RPMTAG_PLATFORM")
                                                     , (1133, Data.List.NonEmpty.singleton "RPMTAG_PATCHESNAME")
                                                     , (1134, Data.List.NonEmpty.singleton "RPMTAG_PATCHESFLAGS")
                                                     , (1135, Data.List.NonEmpty.singleton "RPMTAG_PATCHESVERSION")
                                                     , (1136, Data.List.NonEmpty.singleton "RPMTAG_CACHECTIME")
                                                     , (1137, Data.List.NonEmpty.singleton "RPMTAG_CACHEPKGPATH")
                                                     , (1138, Data.List.NonEmpty.singleton "RPMTAG_CACHEPKGSIZE")
                                                     , (1139, Data.List.NonEmpty.singleton "RPMTAG_CACHEPKGMTIME")
                                                     , (1140, Data.List.NonEmpty.singleton "RPMTAG_FILECOLORS")
                                                     , (1141, Data.List.NonEmpty.singleton "RPMTAG_FILECLASS")
                                                     , (1142, Data.List.NonEmpty.singleton "RPMTAG_CLASSDICT")
                                                     , (1143, Data.List.NonEmpty.singleton "RPMTAG_FILEDEPENDSX")
                                                     , (1144, Data.List.NonEmpty.singleton "RPMTAG_FILEDEPENDSN")
                                                     , (1145, Data.List.NonEmpty.singleton "RPMTAG_DEPENDSDICT")
                                                     , (1146, Data.List.NonEmpty.singleton "RPMTAG_SOURCESIGMD5")
                                                     , (1147, Data.List.NonEmpty.singleton "RPMTAG_FILECONTEXTS")
                                                     , (1148, Data.List.NonEmpty.singleton "RPMTAG_FSCONTEXTS")
                                                     , (1149, Data.List.NonEmpty.singleton "RPMTAG_RECONTEXTS")
                                                     , (1150, Data.List.NonEmpty.singleton "RPMTAG_POLICIES")
                                                     , (1151, Data.List.NonEmpty.singleton "RPMTAG_PRETRANS")
                                                     , (1152, Data.List.NonEmpty.singleton "RPMTAG_POSTTRANS")
                                                     , (1153, Data.List.NonEmpty.singleton "RPMTAG_PRETRANSPROG")
                                                     , (1154, Data.List.NonEmpty.singleton "RPMTAG_POSTTRANSPROG")
                                                     , (1155, Data.List.NonEmpty.singleton "RPMTAG_DISTTAG")
                                                     , (1156, Data.List.NonEmpty.singleton "RPMTAG_OLDSUGGESTSNAME")
                                                     , (1157, Data.List.NonEmpty.singleton "RPMTAG_OLDSUGGESTSVERSION")
                                                     , (1158, Data.List.NonEmpty.singleton "RPMTAG_OLDSUGGESTSFLAGS")
                                                     , (1159, Data.List.NonEmpty.singleton "RPMTAG_OLDENHANCESNAME")
                                                     , (1160, Data.List.NonEmpty.singleton "RPMTAG_OLDENHANCESVERSION")
                                                     , (1161, Data.List.NonEmpty.singleton "RPMTAG_OLDENHANCESFLAGS")
                                                     , (1162, Data.List.NonEmpty.singleton "RPMTAG_PRIORITY")
                                                     , (1163, Data.List.NonEmpty.singleton "RPMTAG_CVSID")
                                                     , (1164, Data.List.NonEmpty.singleton "RPMTAG_BLINKPKGID")
                                                     , (1165, Data.List.NonEmpty.singleton "RPMTAG_BLINKHDRID")
                                                     , (1166, Data.List.NonEmpty.singleton "RPMTAG_BLINKNEVRA")
                                                     , (1167, Data.List.NonEmpty.singleton "RPMTAG_FLINKPKGID")
                                                     , (1168, Data.List.NonEmpty.singleton "RPMTAG_FLINKHDRID")
                                                     , (1169, Data.List.NonEmpty.singleton "RPMTAG_FLINKNEVRA")
                                                     , (1170, Data.List.NonEmpty.singleton "RPMTAG_PACKAGEORIGIN")
                                                     , (1171, Data.List.NonEmpty.singleton "RPMTAG_TRIGGERPREIN")
                                                     , (1172, Data.List.NonEmpty.singleton "RPMTAG_BUILDSUGGESTS")
                                                     , (1173, Data.List.NonEmpty.singleton "RPMTAG_BUILDENHANCES")
                                                     , (1174, Data.List.NonEmpty.singleton "RPMTAG_SCRIPTSTATES")
                                                     , (1175, Data.List.NonEmpty.singleton "RPMTAG_SCRIPTMETRICS")
                                                     , (1176, Data.List.NonEmpty.singleton "RPMTAG_BUILDCPUCLOCK")
                                                     , (1177, Data.List.NonEmpty.singleton "RPMTAG_FILEDIGESTALGOS")
                                                     , (1178, Data.List.NonEmpty.singleton "RPMTAG_VARIANTS")
                                                     , (1179, Data.List.NonEmpty.singleton "RPMTAG_XMAJOR")
                                                     , (1180, Data.List.NonEmpty.singleton "RPMTAG_XMINOR")
                                                     , (1181, Data.List.NonEmpty.singleton "RPMTAG_REPOTAG")
                                                     , (1182, Data.List.NonEmpty.singleton "RPMTAG_KEYWORDS")
                                                     , (1183, Data.List.NonEmpty.singleton "RPMTAG_BUILDPLATFORMS")
                                                     , (1184, Data.List.NonEmpty.singleton "RPMTAG_PACKAGECOLOR")
                                                     , (1185, Data.List.NonEmpty.singleton "RPMTAG_PACKAGEPREFCOLOR")
                                                     , (1186, Data.List.NonEmpty.singleton "RPMTAG_XATTRSDICT")
                                                     , (1187, Data.List.NonEmpty.singleton "RPMTAG_FILEXATTRSX")
                                                     , (1188, Data.List.NonEmpty.singleton "RPMTAG_DEPATTRSDICT")
                                                     , (1189, Data.List.NonEmpty.singleton "RPMTAG_CONFLICTATTRSX")
                                                     , (1190, Data.List.NonEmpty.singleton "RPMTAG_OBSOLETEATTRSX")
                                                     , (1191, Data.List.NonEmpty.singleton "RPMTAG_PROVIDEATTRSX")
                                                     , (1192, Data.List.NonEmpty.singleton "RPMTAG_REQUIREATTRSX")
                                                     , (1193, Data.List.NonEmpty.singleton "RPMTAG_BUILDPROVIDES")
                                                     , (1194, Data.List.NonEmpty.singleton "RPMTAG_BUILDOBSOLETES")
                                                     , (1195, Data.List.NonEmpty.singleton "RPMTAG_DBINSTANCE")
                                                     , (1196, Data.List.NonEmpty.singleton "RPMTAG_NVRA")
                                                     , (5000, Data.List.NonEmpty.singleton "RPMTAG_FILENAMES")
                                                     , (5001, Data.List.NonEmpty.singleton "RPMTAG_FILEPROVIDE")
                                                     , (5002, Data.List.NonEmpty.singleton "RPMTAG_FILEREQUIRE")
                                                     , (5003, Data.List.NonEmpty.singleton "RPMTAG_FSNAMES")
                                                     , (5004, Data.List.NonEmpty.singleton "RPMTAG_FSSIZES")
                                                     , (5005, Data.List.NonEmpty.singleton "RPMTAG_TRIGGERCONDS")
                                                     , (5006, Data.List.NonEmpty.singleton "RPMTAG_TRIGGERTYPE")
                                                     , (5007, Data.List.NonEmpty.singleton "RPMTAG_ORIGFILENAMES")
                                                     , (5008, Data.List.NonEmpty.singleton "RPMTAG_LONGFILESIZES")
                                                     , (5009, Data.List.NonEmpty.singleton "RPMTAG_LONGSIZE")
                                                     , (5010, Data.List.NonEmpty.singleton "RPMTAG_FILECAPS")
                                                     , (5011, Data.List.NonEmpty.singleton "RPMTAG_FILEDIGESTALGO")
                                                     , (5012, Data.List.NonEmpty.singleton "RPMTAG_BUGURL")
                                                     , (5013, Data.List.NonEmpty.singleton "RPMTAG_EVR")
                                                     , (5014, Data.List.NonEmpty.singleton "RPMTAG_NVR")
                                                     , (5015, Data.List.NonEmpty.singleton "RPMTAG_NEVR")
                                                     , (5016, Data.List.NonEmpty.singleton "RPMTAG_NEVRA")
                                                     , (5017, Data.List.NonEmpty.singleton "RPMTAG_HEADERCOLOR")
                                                     , (5018, Data.List.NonEmpty.singleton "RPMTAG_VERBOSE")
                                                     , (5019, Data.List.NonEmpty.singleton "RPMTAG_EPOCHNUM")
                                                     , (5020, Data.List.NonEmpty.singleton "RPMTAG_PREINFLAGS")
                                                     , (5021, Data.List.NonEmpty.singleton "RPMTAG_POSTINFLAGS")
                                                     , (5022, Data.List.NonEmpty.singleton "RPMTAG_PREUNFLAGS")
                                                     , (5023, Data.List.NonEmpty.singleton "RPMTAG_POSTUNFLAGS")
                                                     , (5024, Data.List.NonEmpty.singleton "RPMTAG_PRETRANSFLAGS")
                                                     , (5025, Data.List.NonEmpty.singleton "RPMTAG_POSTTRANSFLAGS")
                                                     , (5026, Data.List.NonEmpty.singleton "RPMTAG_VERIFYSCRIPTFLAGS")
                                                     , (5027, Data.List.NonEmpty.singleton "RPMTAG_TRIGGERSCRIPTFLAGS")
                                                     , (5029, Data.List.NonEmpty.singleton "RPMTAG_COLLECTIONS")
                                                     , (5030, Data.List.NonEmpty.singleton "RPMTAG_POLICYNAMES")
                                                     , (5031, Data.List.NonEmpty.singleton "RPMTAG_POLICYTYPES")
                                                     , (5032, Data.List.NonEmpty.singleton "RPMTAG_POLICYTYPESINDEXES")
                                                     , (5033, Data.List.NonEmpty.singleton "RPMTAG_POLICYFLAGS")
                                                     , (5034, Data.List.NonEmpty.singleton "RPMTAG_VCS")
                                                     , (5035, Data.List.NonEmpty.singleton "RPMTAG_ORDERNAME")
                                                     , (5036, Data.List.NonEmpty.singleton "RPMTAG_ORDERVERSION")
                                                     , (5037, Data.List.NonEmpty.singleton "RPMTAG_ORDERFLAGS")
                                                     , (5038, Data.List.NonEmpty.singleton "RPMTAG_MSSFMANIFEST")
                                                     , (5039, Data.List.NonEmpty.singleton "RPMTAG_MSSFDOMAIN")
                                                     , (5040, Data.List.NonEmpty.singleton "RPMTAG_INSTFILENAMES")
                                                     , (5041, Data.List.NonEmpty.singleton "RPMTAG_REQUIRENEVRS")
                                                     , (5042, Data.List.NonEmpty.singleton "RPMTAG_PROVIDENEVRS")
                                                     , (5043, Data.List.NonEmpty.singleton "RPMTAG_OBSOLETENEVRS")
                                                     , (5044, Data.List.NonEmpty.singleton "RPMTAG_CONFLICTNEVRS")
                                                     , (5045, Data.List.NonEmpty.singleton "RPMTAG_FILENLINKS")
                                                     , (5046, Data.List.NonEmpty.singleton "RPMTAG_RECOMMENDNAME")
                                                     , (5047, Data.List.NonEmpty.singleton "RPMTAG_RECOMMENDVERSION")
                                                     , (5048, Data.List.NonEmpty.singleton "RPMTAG_RECOMMENDFLAGS")
                                                     , (5049, Data.List.NonEmpty.singleton "RPMTAG_SUGGESTNAME")
                                                     , (5050, Data.List.NonEmpty.singleton "RPMTAG_SUGGESTVERSION")
                                                     , (5051, Data.List.NonEmpty.singleton "RPMTAG_SUGGESTFLAGS")
                                                     , (5052, Data.List.NonEmpty.singleton "RPMTAG_SUPPLEMENTNAME")
                                                     , (5053, Data.List.NonEmpty.singleton "RPMTAG_SUPPLEMENTVERSION")
                                                     , (5054, Data.List.NonEmpty.singleton "RPMTAG_SUPPLEMENTFLAGS")
                                                     , (5055, Data.List.NonEmpty.singleton "RPMTAG_ENHANCENAME")
                                                     , (5056, Data.List.NonEmpty.singleton "RPMTAG_ENHANCEVERSION")
                                                     , (5057, Data.List.NonEmpty.singleton "RPMTAG_ENHANCEFLAGS")
                                                     , (5058, Data.List.NonEmpty.singleton "RPMTAG_RECOMMENDNEVRS")
                                                     , (5059, Data.List.NonEmpty.singleton "RPMTAG_SUGGESTNEVRS")
                                                     , (5060, Data.List.NonEmpty.singleton "RPMTAG_SUPPLEMENTNEVRS")
                                                     , (5061, Data.List.NonEmpty.singleton "RPMTAG_ENHANCENEVRS")
                                                     , (5062, Data.List.NonEmpty.singleton "RPMTAG_ENCODING")
                                                     , (5063, Data.List.NonEmpty.singleton "RPMTAG_FILETRIGGERIN")
                                                     , (5064, Data.List.NonEmpty.singleton "RPMTAG_FILETRIGGERUN")
                                                     , (5065, Data.List.NonEmpty.singleton "RPMTAG_FILETRIGGERPOSTUN")
                                                     , (5066, Data.List.NonEmpty.singleton "RPMTAG_FILETRIGGERSCRIPTS")
                                                     , (5067, Data.List.NonEmpty.singleton "RPMTAG_FILETRIGGERSCRIPTPROG")
                                                     , (5068, Data.List.NonEmpty.singleton "RPMTAG_FILETRIGGERSCRIPTFLAGS")
                                                     , (5069, Data.List.NonEmpty.singleton "RPMTAG_FILETRIGGERNAME")
                                                     , (5070, Data.List.NonEmpty.singleton "RPMTAG_FILETRIGGERINDEX")
                                                     , (5071, Data.List.NonEmpty.singleton "RPMTAG_FILETRIGGERVERSION")
                                                     , (5072, Data.List.NonEmpty.singleton "RPMTAG_FILETRIGGERFLAGS")
                                                     , (5073, Data.List.NonEmpty.singleton "RPMTAG_TRANSFILETRIGGERIN")
                                                     , (5074, Data.List.NonEmpty.singleton "RPMTAG_TRANSFILETRIGGERUN")
                                                     , (5075, Data.List.NonEmpty.singleton "RPMTAG_TRANSFILETRIGGERPOSTUN")
                                                     , (5076, Data.List.NonEmpty.singleton "RPMTAG_TRANSFILETRIGGERSCRIPTS")
                                                     , (5077, Data.List.NonEmpty.singleton "RPMTAG_TRANSFILETRIGGERSCRIPTPROG")
                                                     , (5078, Data.List.NonEmpty.singleton "RPMTAG_TRANSFILETRIGGERSCRIPTFLAGS")
                                                     , (5079, Data.List.NonEmpty.singleton "RPMTAG_TRANSFILETRIGGERNAME")
                                                     , (5080, Data.List.NonEmpty.singleton "RPMTAG_TRANSFILETRIGGERINDEX")
                                                     , (5081, Data.List.NonEmpty.singleton "RPMTAG_TRANSFILETRIGGERVERSION")
                                                     , (5082, Data.List.NonEmpty.singleton "RPMTAG_TRANSFILETRIGGERFLAGS")
                                                     , (5083, Data.List.NonEmpty.singleton "RPMTAG_REMOVEPATHPOSTFIXES")
                                                     , (5084, Data.List.NonEmpty.singleton "RPMTAG_FILETRIGGERPRIORITIES")
                                                     , (5085, Data.List.NonEmpty.singleton "RPMTAG_TRANSFILETRIGGERPRIORITIES")
                                                     , (5086, Data.List.NonEmpty.singleton "RPMTAG_FILETRIGGERCONDS")
                                                     , (5087, Data.List.NonEmpty.singleton "RPMTAG_FILETRIGGERTYPE")
                                                     , (5088, Data.List.NonEmpty.singleton "RPMTAG_TRANSFILETRIGGERCONDS")
                                                     , (5089, Data.List.NonEmpty.singleton "RPMTAG_TRANSFILETRIGGERTYPE")
                                                     , (5090, Data.List.NonEmpty.singleton "RPMTAG_FILESIGNATURES")
                                                     , (5091, Data.List.NonEmpty.singleton "RPMTAG_FILESIGNATURELENGTH")
                                                     , (5092, Data.List.NonEmpty.singleton "RPMTAG_PAYLOADSHA256")
                                                     , (5093, Data.List.NonEmpty.singleton "RPMTAG_PAYLOADSHA256ALGO")
                                                     , (5094, Data.List.NonEmpty.singleton "RPMTAG_AUTOINSTALLED")
                                                     , (5095, Data.List.NonEmpty.singleton "RPMTAG_IDENTITY")
                                                     , (5096, Data.List.NonEmpty.singleton "RPMTAG_MODULARITYLABEL")
                                                     , (5097, Data.List.NonEmpty.singleton "RPMTAG_PAYLOADSHA256ALT")
                                                     , (5098, Data.List.NonEmpty.singleton "RPMTAG_ARCHSUFFIX")
                                                     , (5099, Data.List.NonEmpty.singleton "RPMTAG_SPEC")
                                                     , (5100, Data.List.NonEmpty.singleton "RPMTAG_TRANSLATIONURL")
                                                     , (5101, Data.List.NonEmpty.singleton "RPMTAG_UPSTREAMRELEASES")
                                                     , (5102, Data.List.NonEmpty.singleton "RPMTAG_SOURCELICENSE")
                                                     , (5103, Data.List.NonEmpty.singleton "RPMTAG_PREUNTRANS")
                                                     , (5104, Data.List.NonEmpty.singleton "RPMTAG_POSTUNTRANS")
                                                     , (5105, Data.List.NonEmpty.singleton "RPMTAG_PREUNTRANSPROG")
                                                     , (5106, Data.List.NonEmpty.singleton "RPMTAG_POSTUNTRANSPROG")
                                                     , (5107, Data.List.NonEmpty.singleton "RPMTAG_PREUNTRANSFLAGS")
                                                     , (5108, Data.List.NonEmpty.singleton "RPMTAG_POSTUNTRANSFLAGS")
                                                     , (5109, Data.List.NonEmpty.singleton "RPMTAG_SYSUSERS")
                                                     , (5110, Data.List.NonEmpty.singleton "RPMTAG_BUILDSYSTEM")
                                                     , (5111, Data.List.NonEmpty.singleton "RPMTAG_BUILDOPTION")
                                                     , (5112, Data.List.NonEmpty.singleton "RPMTAG_PAYLOADSIZE")
                                                     , (5113, Data.List.NonEmpty.singleton "RPMTAG_PAYLOADSIZEALT")
                                                     , (5114, Data.List.NonEmpty.singleton "RPMTAG_RPMFORMAT")
                                                     , (5115, Data.List.NonEmpty.singleton "RPMTAG_FILEMIMEINDEX")
                                                     , (5116, Data.List.NonEmpty.singleton "RPMTAG_MIMEDICT")
                                                     , (5117, Data.List.NonEmpty.singleton "RPMTAG_FILEMIMES")
                                                     , (5118, Data.List.NonEmpty.singleton "RPMTAG_PACKAGEDIGESTS")
                                                     , (5119, Data.List.NonEmpty.singleton "RPMTAG_PACKAGEDIGESTALGOS")
                                                     , (5120, Data.List.NonEmpty.singleton "RPMTAG_SOURCENEVR")
                                                     , (5121, Data.List.NonEmpty.singleton "RPMTAG_PAYLOADSHA512")
                                                     , (5122, Data.List.NonEmpty.singleton "RPMTAG_PAYLOADSHA512ALT")
                                                     , (5123, Data.List.NonEmpty.singleton "RPMTAG_PAYLOADSHA3_256")
                                                     , (5124, Data.List.NonEmpty.singleton "RPMTAG_PAYLOADSHA3_256ALT")
                                                     , (5125, Data.List.NonEmpty.singleton "RPMTAG_FIRSTFREE_TAG")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "RpmTag"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "RpmTag"

instance Show RpmTag where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read RpmTag where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| Current image.

__C declaration:__ @RPMTAG_HEADERIMAGE@

__defined at:__ @rpm\/rpmtag.h:36:5@

__exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_HEADERIMAGE :: RpmTag
pattern RPMTAG_HEADERIMAGE = RpmTag 61

{-| Signatures.

__C declaration:__ @RPMTAG_HEADERSIGNATURES@

__defined at:__ @rpm\/rpmtag.h:37:5@

__exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_HEADERSIGNATURES :: RpmTag
pattern RPMTAG_HEADERSIGNATURES = RpmTag 62

{-| __C declaration:__ @RPMTAG_HEADERIMMUTABLE@

    __defined at:__ @rpm\/rpmtag.h:38:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_HEADERIMMUTABLE :: RpmTag
pattern RPMTAG_HEADERIMMUTABLE = RpmTag 63

{-| Regions.

__C declaration:__ @RPMTAG_HEADERREGIONS@

__defined at:__ @rpm\/rpmtag.h:39:5@

__exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_HEADERREGIONS :: RpmTag
pattern RPMTAG_HEADERREGIONS = RpmTag 64

{-| __C declaration:__ @RPMTAG_HEADERI18NTABLE@

    __defined at:__ @rpm\/rpmtag.h:41:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_HEADERI18NTABLE :: RpmTag
pattern RPMTAG_HEADERI18NTABLE = RpmTag 100

{-| __C declaration:__ @RPMTAG_SIG_BASE@

    __defined at:__ @rpm\/rpmtag.h:46:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SIG_BASE :: RpmTag
pattern RPMTAG_SIG_BASE = RpmTag 256

{-| __C declaration:__ @RPMTAG_SIGSIZE@

    __defined at:__ @rpm\/rpmtag.h:47:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SIGSIZE :: RpmTag
pattern RPMTAG_SIGSIZE = RpmTag 257

{-| __C declaration:__ @RPMTAG_SIGLEMD5_1@

    __defined at:__ @rpm\/rpmtag.h:48:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SIGLEMD5_1 :: RpmTag
pattern RPMTAG_SIGLEMD5_1 = RpmTag 258

{-| __C declaration:__ @RPMTAG_SIGPGP@

    __defined at:__ @rpm\/rpmtag.h:49:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SIGPGP :: RpmTag
pattern RPMTAG_SIGPGP = RpmTag 259

{-| __C declaration:__ @RPMTAG_SIGLEMD5_2@

    __defined at:__ @rpm\/rpmtag.h:50:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SIGLEMD5_2 :: RpmTag
pattern RPMTAG_SIGLEMD5_2 = RpmTag 260

{-| __C declaration:__ @RPMTAG_SIGMD5@

    __defined at:__ @rpm\/rpmtag.h:51:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SIGMD5 :: RpmTag
pattern RPMTAG_SIGMD5 = RpmTag 261

{-| __C declaration:__ @RPMTAG_SIGGPG@

    __defined at:__ @rpm\/rpmtag.h:52:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SIGGPG :: RpmTag
pattern RPMTAG_SIGGPG = RpmTag 262

{-| __C declaration:__ @RPMTAG_SIGPGP5@

    __defined at:__ @rpm\/rpmtag.h:53:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SIGPGP5 :: RpmTag
pattern RPMTAG_SIGPGP5 = RpmTag 263

{-| __C declaration:__ @RPMTAG_BADSHA1_1@

    __defined at:__ @rpm\/rpmtag.h:55:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BADSHA1_1 :: RpmTag
pattern RPMTAG_BADSHA1_1 = RpmTag 264

{-| __C declaration:__ @RPMTAG_BADSHA1_2@

    __defined at:__ @rpm\/rpmtag.h:56:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BADSHA1_2 :: RpmTag
pattern RPMTAG_BADSHA1_2 = RpmTag 265

{-| __C declaration:__ @RPMTAG_PUBKEYS@

    __defined at:__ @rpm\/rpmtag.h:57:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PUBKEYS :: RpmTag
pattern RPMTAG_PUBKEYS = RpmTag 266

{-| __C declaration:__ @RPMTAG_DSAHEADER@

    __defined at:__ @rpm\/rpmtag.h:58:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_DSAHEADER :: RpmTag
pattern RPMTAG_DSAHEADER = RpmTag 267

{-| __C declaration:__ @RPMTAG_RSAHEADER@

    __defined at:__ @rpm\/rpmtag.h:59:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_RSAHEADER :: RpmTag
pattern RPMTAG_RSAHEADER = RpmTag 268

{-| __C declaration:__ @RPMTAG_SHA1HEADER@

    __defined at:__ @rpm\/rpmtag.h:60:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SHA1HEADER :: RpmTag
pattern RPMTAG_SHA1HEADER = RpmTag 269

{-| __C declaration:__ @RPMTAG_LONGSIGSIZE@

    __defined at:__ @rpm\/rpmtag.h:61:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_LONGSIGSIZE :: RpmTag
pattern RPMTAG_LONGSIGSIZE = RpmTag 270

{-| __C declaration:__ @RPMTAG_LONGARCHIVESIZE@

    __defined at:__ @rpm\/rpmtag.h:62:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_LONGARCHIVESIZE :: RpmTag
pattern RPMTAG_LONGARCHIVESIZE = RpmTag 271

{-| __C declaration:__ @RPMTAG_SHA256HEADER@

    __defined at:__ @rpm\/rpmtag.h:64:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SHA256HEADER :: RpmTag
pattern RPMTAG_SHA256HEADER = RpmTag 273

{-| __C declaration:__ @RPMTAG_VERITYSIGNATURES@

    __defined at:__ @rpm\/rpmtag.h:67:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_VERITYSIGNATURES :: RpmTag
pattern RPMTAG_VERITYSIGNATURES = RpmTag 276

{-| __C declaration:__ @RPMTAG_VERITYSIGNATUREALGO@

    __defined at:__ @rpm\/rpmtag.h:68:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_VERITYSIGNATUREALGO :: RpmTag
pattern RPMTAG_VERITYSIGNATUREALGO = RpmTag 277

{-| __C declaration:__ @RPMTAG_OPENPGP@

    __defined at:__ @rpm\/rpmtag.h:69:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_OPENPGP :: RpmTag
pattern RPMTAG_OPENPGP = RpmTag 278

{-| __C declaration:__ @RPMTAG_SHA3_256HEADER@

    __defined at:__ @rpm\/rpmtag.h:70:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SHA3_256HEADER :: RpmTag
pattern RPMTAG_SHA3_256HEADER = RpmTag 279

{-| __C declaration:__ @RPMTAG_SIG_TOP@

    __defined at:__ @rpm\/rpmtag.h:71:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SIG_TOP :: RpmTag
pattern RPMTAG_SIG_TOP = RpmTag 999

{-| __C declaration:__ @RPMTAG_NAME@

    __defined at:__ @rpm\/rpmtag.h:73:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_NAME :: RpmTag
pattern RPMTAG_NAME = RpmTag 1000

{-| __C declaration:__ @RPMTAG_VERSION@

    __defined at:__ @rpm\/rpmtag.h:75:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_VERSION :: RpmTag
pattern RPMTAG_VERSION = RpmTag 1001

{-| __C declaration:__ @RPMTAG_RELEASE@

    __defined at:__ @rpm\/rpmtag.h:77:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_RELEASE :: RpmTag
pattern RPMTAG_RELEASE = RpmTag 1002

{-| __C declaration:__ @RPMTAG_EPOCH@

    __defined at:__ @rpm\/rpmtag.h:79:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_EPOCH :: RpmTag
pattern RPMTAG_EPOCH = RpmTag 1003

{-| __C declaration:__ @RPMTAG_SUMMARY@

    __defined at:__ @rpm\/rpmtag.h:81:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SUMMARY :: RpmTag
pattern RPMTAG_SUMMARY = RpmTag 1004

{-| __C declaration:__ @RPMTAG_DESCRIPTION@

    __defined at:__ @rpm\/rpmtag.h:82:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_DESCRIPTION :: RpmTag
pattern RPMTAG_DESCRIPTION = RpmTag 1005

{-| __C declaration:__ @RPMTAG_BUILDTIME@

    __defined at:__ @rpm\/rpmtag.h:83:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BUILDTIME :: RpmTag
pattern RPMTAG_BUILDTIME = RpmTag 1006

{-| __C declaration:__ @RPMTAG_BUILDHOST@

    __defined at:__ @rpm\/rpmtag.h:84:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BUILDHOST :: RpmTag
pattern RPMTAG_BUILDHOST = RpmTag 1007

{-| __C declaration:__ @RPMTAG_INSTALLTIME@

    __defined at:__ @rpm\/rpmtag.h:85:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_INSTALLTIME :: RpmTag
pattern RPMTAG_INSTALLTIME = RpmTag 1008

{-| __C declaration:__ @RPMTAG_SIZE@

    __defined at:__ @rpm\/rpmtag.h:86:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SIZE :: RpmTag
pattern RPMTAG_SIZE = RpmTag 1009

{-| __C declaration:__ @RPMTAG_DISTRIBUTION@

    __defined at:__ @rpm\/rpmtag.h:87:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_DISTRIBUTION :: RpmTag
pattern RPMTAG_DISTRIBUTION = RpmTag 1010

{-| __C declaration:__ @RPMTAG_VENDOR@

    __defined at:__ @rpm\/rpmtag.h:88:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_VENDOR :: RpmTag
pattern RPMTAG_VENDOR = RpmTag 1011

{-| __C declaration:__ @RPMTAG_GIF@

    __defined at:__ @rpm\/rpmtag.h:89:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_GIF :: RpmTag
pattern RPMTAG_GIF = RpmTag 1012

{-| __C declaration:__ @RPMTAG_XPM@

    __defined at:__ @rpm\/rpmtag.h:90:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_XPM :: RpmTag
pattern RPMTAG_XPM = RpmTag 1013

{-| __C declaration:__ @RPMTAG_LICENSE@

    __defined at:__ @rpm\/rpmtag.h:91:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_LICENSE :: RpmTag
pattern RPMTAG_LICENSE = RpmTag 1014

{-| __C declaration:__ @RPMTAG_PACKAGER@

    __defined at:__ @rpm\/rpmtag.h:92:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PACKAGER :: RpmTag
pattern RPMTAG_PACKAGER = RpmTag 1015

{-| __C declaration:__ @RPMTAG_GROUP@

    __defined at:__ @rpm\/rpmtag.h:93:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_GROUP :: RpmTag
pattern RPMTAG_GROUP = RpmTag 1016

{-| __C declaration:__ @RPMTAG_CHANGELOG@

    __defined at:__ @rpm\/rpmtag.h:94:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_CHANGELOG :: RpmTag
pattern RPMTAG_CHANGELOG = RpmTag 1017

{-| __C declaration:__ @RPMTAG_SOURCE@

    __defined at:__ @rpm\/rpmtag.h:95:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SOURCE :: RpmTag
pattern RPMTAG_SOURCE = RpmTag 1018

{-| __C declaration:__ @RPMTAG_PATCH@

    __defined at:__ @rpm\/rpmtag.h:96:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PATCH :: RpmTag
pattern RPMTAG_PATCH = RpmTag 1019

{-| __C declaration:__ @RPMTAG_URL@

    __defined at:__ @rpm\/rpmtag.h:97:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_URL :: RpmTag
pattern RPMTAG_URL = RpmTag 1020

{-| __C declaration:__ @RPMTAG_OS@

    __defined at:__ @rpm\/rpmtag.h:98:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_OS :: RpmTag
pattern RPMTAG_OS = RpmTag 1021

{-| __C declaration:__ @RPMTAG_ARCH@

    __defined at:__ @rpm\/rpmtag.h:99:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_ARCH :: RpmTag
pattern RPMTAG_ARCH = RpmTag 1022

{-| __C declaration:__ @RPMTAG_PREIN@

    __defined at:__ @rpm\/rpmtag.h:100:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PREIN :: RpmTag
pattern RPMTAG_PREIN = RpmTag 1023

{-| __C declaration:__ @RPMTAG_POSTIN@

    __defined at:__ @rpm\/rpmtag.h:101:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_POSTIN :: RpmTag
pattern RPMTAG_POSTIN = RpmTag 1024

{-| __C declaration:__ @RPMTAG_PREUN@

    __defined at:__ @rpm\/rpmtag.h:102:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PREUN :: RpmTag
pattern RPMTAG_PREUN = RpmTag 1025

{-| __C declaration:__ @RPMTAG_POSTUN@

    __defined at:__ @rpm\/rpmtag.h:103:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_POSTUN :: RpmTag
pattern RPMTAG_POSTUN = RpmTag 1026

{-| __C declaration:__ @RPMTAG_OLDFILENAMES@

    __defined at:__ @rpm\/rpmtag.h:104:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_OLDFILENAMES :: RpmTag
pattern RPMTAG_OLDFILENAMES = RpmTag 1027

{-| __C declaration:__ @RPMTAG_FILESIZES@

    __defined at:__ @rpm\/rpmtag.h:105:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILESIZES :: RpmTag
pattern RPMTAG_FILESIZES = RpmTag 1028

{-| __C declaration:__ @RPMTAG_FILESTATES@

    __defined at:__ @rpm\/rpmtag.h:106:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILESTATES :: RpmTag
pattern RPMTAG_FILESTATES = RpmTag 1029

{-| __C declaration:__ @RPMTAG_FILEMODES@

    __defined at:__ @rpm\/rpmtag.h:107:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILEMODES :: RpmTag
pattern RPMTAG_FILEMODES = RpmTag 1030

{-| __C declaration:__ @RPMTAG_FILEUIDS@

    __defined at:__ @rpm\/rpmtag.h:108:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILEUIDS :: RpmTag
pattern RPMTAG_FILEUIDS = RpmTag 1031

{-| __C declaration:__ @RPMTAG_FILEGIDS@

    __defined at:__ @rpm\/rpmtag.h:109:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILEGIDS :: RpmTag
pattern RPMTAG_FILEGIDS = RpmTag 1032

{-| __C declaration:__ @RPMTAG_FILERDEVS@

    __defined at:__ @rpm\/rpmtag.h:110:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILERDEVS :: RpmTag
pattern RPMTAG_FILERDEVS = RpmTag 1033

{-| __C declaration:__ @RPMTAG_FILEMTIMES@

    __defined at:__ @rpm\/rpmtag.h:111:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILEMTIMES :: RpmTag
pattern RPMTAG_FILEMTIMES = RpmTag 1034

{-| __C declaration:__ @RPMTAG_FILEDIGESTS@

    __defined at:__ @rpm\/rpmtag.h:112:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILEDIGESTS :: RpmTag
pattern RPMTAG_FILEDIGESTS = RpmTag 1035

{-| __C declaration:__ @RPMTAG_FILELINKTOS@

    __defined at:__ @rpm\/rpmtag.h:114:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILELINKTOS :: RpmTag
pattern RPMTAG_FILELINKTOS = RpmTag 1036

{-| __C declaration:__ @RPMTAG_FILEFLAGS@

    __defined at:__ @rpm\/rpmtag.h:115:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILEFLAGS :: RpmTag
pattern RPMTAG_FILEFLAGS = RpmTag 1037

{-| __C declaration:__ @RPMTAG_ROOT@

    __defined at:__ @rpm\/rpmtag.h:116:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_ROOT :: RpmTag
pattern RPMTAG_ROOT = RpmTag 1038

{-| __C declaration:__ @RPMTAG_FILEUSERNAME@

    __defined at:__ @rpm\/rpmtag.h:117:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILEUSERNAME :: RpmTag
pattern RPMTAG_FILEUSERNAME = RpmTag 1039

{-| __C declaration:__ @RPMTAG_FILEGROUPNAME@

    __defined at:__ @rpm\/rpmtag.h:118:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILEGROUPNAME :: RpmTag
pattern RPMTAG_FILEGROUPNAME = RpmTag 1040

{-| __C declaration:__ @RPMTAG_EXCLUDE@

    __defined at:__ @rpm\/rpmtag.h:119:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_EXCLUDE :: RpmTag
pattern RPMTAG_EXCLUDE = RpmTag 1041

{-| __C declaration:__ @RPMTAG_EXCLUSIVE@

    __defined at:__ @rpm\/rpmtag.h:120:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_EXCLUSIVE :: RpmTag
pattern RPMTAG_EXCLUSIVE = RpmTag 1042

{-| __C declaration:__ @RPMTAG_ICON@

    __defined at:__ @rpm\/rpmtag.h:121:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_ICON :: RpmTag
pattern RPMTAG_ICON = RpmTag 1043

{-| __C declaration:__ @RPMTAG_SOURCERPM@

    __defined at:__ @rpm\/rpmtag.h:122:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SOURCERPM :: RpmTag
pattern RPMTAG_SOURCERPM = RpmTag 1044

{-| __C declaration:__ @RPMTAG_FILEVERIFYFLAGS@

    __defined at:__ @rpm\/rpmtag.h:123:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILEVERIFYFLAGS :: RpmTag
pattern RPMTAG_FILEVERIFYFLAGS = RpmTag 1045

{-| __C declaration:__ @RPMTAG_ARCHIVESIZE@

    __defined at:__ @rpm\/rpmtag.h:124:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_ARCHIVESIZE :: RpmTag
pattern RPMTAG_ARCHIVESIZE = RpmTag 1046

{-| __C declaration:__ @RPMTAG_PROVIDENAME@

    __defined at:__ @rpm\/rpmtag.h:125:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PROVIDENAME :: RpmTag
pattern RPMTAG_PROVIDENAME = RpmTag 1047

{-| __C declaration:__ @RPMTAG_REQUIREFLAGS@

    __defined at:__ @rpm\/rpmtag.h:128:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_REQUIREFLAGS :: RpmTag
pattern RPMTAG_REQUIREFLAGS = RpmTag 1048

{-| __C declaration:__ @RPMTAG_REQUIRENAME@

    __defined at:__ @rpm\/rpmtag.h:129:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_REQUIRENAME :: RpmTag
pattern RPMTAG_REQUIRENAME = RpmTag 1049

{-| __C declaration:__ @RPMTAG_REQUIREVERSION@

    __defined at:__ @rpm\/rpmtag.h:131:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_REQUIREVERSION :: RpmTag
pattern RPMTAG_REQUIREVERSION = RpmTag 1050

{-| __C declaration:__ @RPMTAG_NOSOURCE@

    __defined at:__ @rpm\/rpmtag.h:132:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_NOSOURCE :: RpmTag
pattern RPMTAG_NOSOURCE = RpmTag 1051

{-| __C declaration:__ @RPMTAG_NOPATCH@

    __defined at:__ @rpm\/rpmtag.h:133:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_NOPATCH :: RpmTag
pattern RPMTAG_NOPATCH = RpmTag 1052

{-| __C declaration:__ @RPMTAG_CONFLICTFLAGS@

    __defined at:__ @rpm\/rpmtag.h:134:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_CONFLICTFLAGS :: RpmTag
pattern RPMTAG_CONFLICTFLAGS = RpmTag 1053

{-| __C declaration:__ @RPMTAG_CONFLICTNAME@

    __defined at:__ @rpm\/rpmtag.h:135:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_CONFLICTNAME :: RpmTag
pattern RPMTAG_CONFLICTNAME = RpmTag 1054

{-| __C declaration:__ @RPMTAG_CONFLICTVERSION@

    __defined at:__ @rpm\/rpmtag.h:138:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_CONFLICTVERSION :: RpmTag
pattern RPMTAG_CONFLICTVERSION = RpmTag 1055

{-| __C declaration:__ @RPMTAG_DEFAULTPREFIX@

    __defined at:__ @rpm\/rpmtag.h:139:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_DEFAULTPREFIX :: RpmTag
pattern RPMTAG_DEFAULTPREFIX = RpmTag 1056

{-| __C declaration:__ @RPMTAG_BUILDROOT@

    __defined at:__ @rpm\/rpmtag.h:140:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BUILDROOT :: RpmTag
pattern RPMTAG_BUILDROOT = RpmTag 1057

{-| __C declaration:__ @RPMTAG_INSTALLPREFIX@

    __defined at:__ @rpm\/rpmtag.h:141:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_INSTALLPREFIX :: RpmTag
pattern RPMTAG_INSTALLPREFIX = RpmTag 1058

{-| __C declaration:__ @RPMTAG_EXCLUDEARCH@

    __defined at:__ @rpm\/rpmtag.h:142:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_EXCLUDEARCH :: RpmTag
pattern RPMTAG_EXCLUDEARCH = RpmTag 1059

{-| __C declaration:__ @RPMTAG_EXCLUDEOS@

    __defined at:__ @rpm\/rpmtag.h:143:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_EXCLUDEOS :: RpmTag
pattern RPMTAG_EXCLUDEOS = RpmTag 1060

{-| __C declaration:__ @RPMTAG_EXCLUSIVEARCH@

    __defined at:__ @rpm\/rpmtag.h:144:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_EXCLUSIVEARCH :: RpmTag
pattern RPMTAG_EXCLUSIVEARCH = RpmTag 1061

{-| __C declaration:__ @RPMTAG_EXCLUSIVEOS@

    __defined at:__ @rpm\/rpmtag.h:145:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_EXCLUSIVEOS :: RpmTag
pattern RPMTAG_EXCLUSIVEOS = RpmTag 1062

{-| __C declaration:__ @RPMTAG_AUTOREQPROV@

    __defined at:__ @rpm\/rpmtag.h:146:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_AUTOREQPROV :: RpmTag
pattern RPMTAG_AUTOREQPROV = RpmTag 1063

{-| __C declaration:__ @RPMTAG_RPMVERSION@

    __defined at:__ @rpm\/rpmtag.h:147:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_RPMVERSION :: RpmTag
pattern RPMTAG_RPMVERSION = RpmTag 1064

{-| __C declaration:__ @RPMTAG_TRIGGERSCRIPTS@

    __defined at:__ @rpm\/rpmtag.h:148:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRIGGERSCRIPTS :: RpmTag
pattern RPMTAG_TRIGGERSCRIPTS = RpmTag 1065

{-| __C declaration:__ @RPMTAG_TRIGGERNAME@

    __defined at:__ @rpm\/rpmtag.h:149:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRIGGERNAME :: RpmTag
pattern RPMTAG_TRIGGERNAME = RpmTag 1066

{-| __C declaration:__ @RPMTAG_TRIGGERVERSION@

    __defined at:__ @rpm\/rpmtag.h:150:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRIGGERVERSION :: RpmTag
pattern RPMTAG_TRIGGERVERSION = RpmTag 1067

{-| __C declaration:__ @RPMTAG_TRIGGERFLAGS@

    __defined at:__ @rpm\/rpmtag.h:151:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRIGGERFLAGS :: RpmTag
pattern RPMTAG_TRIGGERFLAGS = RpmTag 1068

{-| __C declaration:__ @RPMTAG_TRIGGERINDEX@

    __defined at:__ @rpm\/rpmtag.h:152:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRIGGERINDEX :: RpmTag
pattern RPMTAG_TRIGGERINDEX = RpmTag 1069

{-| __C declaration:__ @RPMTAG_VERIFYSCRIPT@

    __defined at:__ @rpm\/rpmtag.h:153:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_VERIFYSCRIPT :: RpmTag
pattern RPMTAG_VERIFYSCRIPT = RpmTag 1079

{-| __C declaration:__ @RPMTAG_CHANGELOGTIME@

    __defined at:__ @rpm\/rpmtag.h:154:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_CHANGELOGTIME :: RpmTag
pattern RPMTAG_CHANGELOGTIME = RpmTag 1080

{-| __C declaration:__ @RPMTAG_CHANGELOGNAME@

    __defined at:__ @rpm\/rpmtag.h:155:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_CHANGELOGNAME :: RpmTag
pattern RPMTAG_CHANGELOGNAME = RpmTag 1081

{-| __C declaration:__ @RPMTAG_CHANGELOGTEXT@

    __defined at:__ @rpm\/rpmtag.h:156:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_CHANGELOGTEXT :: RpmTag
pattern RPMTAG_CHANGELOGTEXT = RpmTag 1082

{-| __C declaration:__ @RPMTAG_BROKENMD5@

    __defined at:__ @rpm\/rpmtag.h:157:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BROKENMD5 :: RpmTag
pattern RPMTAG_BROKENMD5 = RpmTag 1083

{-| __C declaration:__ @RPMTAG_PREREQ@

    __defined at:__ @rpm\/rpmtag.h:158:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PREREQ :: RpmTag
pattern RPMTAG_PREREQ = RpmTag 1084

{-| __C declaration:__ @RPMTAG_PREINPROG@

    __defined at:__ @rpm\/rpmtag.h:159:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PREINPROG :: RpmTag
pattern RPMTAG_PREINPROG = RpmTag 1085

{-| __C declaration:__ @RPMTAG_POSTINPROG@

    __defined at:__ @rpm\/rpmtag.h:160:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_POSTINPROG :: RpmTag
pattern RPMTAG_POSTINPROG = RpmTag 1086

{-| __C declaration:__ @RPMTAG_PREUNPROG@

    __defined at:__ @rpm\/rpmtag.h:161:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PREUNPROG :: RpmTag
pattern RPMTAG_PREUNPROG = RpmTag 1087

{-| __C declaration:__ @RPMTAG_POSTUNPROG@

    __defined at:__ @rpm\/rpmtag.h:162:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_POSTUNPROG :: RpmTag
pattern RPMTAG_POSTUNPROG = RpmTag 1088

{-| __C declaration:__ @RPMTAG_BUILDARCHS@

    __defined at:__ @rpm\/rpmtag.h:163:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BUILDARCHS :: RpmTag
pattern RPMTAG_BUILDARCHS = RpmTag 1089

{-| __C declaration:__ @RPMTAG_OBSOLETENAME@

    __defined at:__ @rpm\/rpmtag.h:164:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_OBSOLETENAME :: RpmTag
pattern RPMTAG_OBSOLETENAME = RpmTag 1090

{-| __C declaration:__ @RPMTAG_VERIFYSCRIPTPROG@

    __defined at:__ @rpm\/rpmtag.h:167:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_VERIFYSCRIPTPROG :: RpmTag
pattern RPMTAG_VERIFYSCRIPTPROG = RpmTag 1091

{-| __C declaration:__ @RPMTAG_TRIGGERSCRIPTPROG@

    __defined at:__ @rpm\/rpmtag.h:168:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRIGGERSCRIPTPROG :: RpmTag
pattern RPMTAG_TRIGGERSCRIPTPROG = RpmTag 1092

{-| __C declaration:__ @RPMTAG_DOCDIR@

    __defined at:__ @rpm\/rpmtag.h:169:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_DOCDIR :: RpmTag
pattern RPMTAG_DOCDIR = RpmTag 1093

{-| __C declaration:__ @RPMTAG_COOKIE@

    __defined at:__ @rpm\/rpmtag.h:170:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_COOKIE :: RpmTag
pattern RPMTAG_COOKIE = RpmTag 1094

{-| __C declaration:__ @RPMTAG_FILEDEVICES@

    __defined at:__ @rpm\/rpmtag.h:171:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILEDEVICES :: RpmTag
pattern RPMTAG_FILEDEVICES = RpmTag 1095

{-| __C declaration:__ @RPMTAG_FILEINODES@

    __defined at:__ @rpm\/rpmtag.h:172:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILEINODES :: RpmTag
pattern RPMTAG_FILEINODES = RpmTag 1096

{-| __C declaration:__ @RPMTAG_FILELANGS@

    __defined at:__ @rpm\/rpmtag.h:173:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILELANGS :: RpmTag
pattern RPMTAG_FILELANGS = RpmTag 1097

{-| __C declaration:__ @RPMTAG_PREFIXES@

    __defined at:__ @rpm\/rpmtag.h:174:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PREFIXES :: RpmTag
pattern RPMTAG_PREFIXES = RpmTag 1098

{-| __C declaration:__ @RPMTAG_INSTPREFIXES@

    __defined at:__ @rpm\/rpmtag.h:175:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_INSTPREFIXES :: RpmTag
pattern RPMTAG_INSTPREFIXES = RpmTag 1099

{-| __C declaration:__ @RPMTAG_TRIGGERIN@

    __defined at:__ @rpm\/rpmtag.h:176:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRIGGERIN :: RpmTag
pattern RPMTAG_TRIGGERIN = RpmTag 1100

{-| __C declaration:__ @RPMTAG_TRIGGERUN@

    __defined at:__ @rpm\/rpmtag.h:177:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRIGGERUN :: RpmTag
pattern RPMTAG_TRIGGERUN = RpmTag 1101

{-| __C declaration:__ @RPMTAG_TRIGGERPOSTUN@

    __defined at:__ @rpm\/rpmtag.h:178:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRIGGERPOSTUN :: RpmTag
pattern RPMTAG_TRIGGERPOSTUN = RpmTag 1102

{-| __C declaration:__ @RPMTAG_AUTOREQ@

    __defined at:__ @rpm\/rpmtag.h:179:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_AUTOREQ :: RpmTag
pattern RPMTAG_AUTOREQ = RpmTag 1103

{-| __C declaration:__ @RPMTAG_AUTOPROV@

    __defined at:__ @rpm\/rpmtag.h:180:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_AUTOPROV :: RpmTag
pattern RPMTAG_AUTOPROV = RpmTag 1104

{-| __C declaration:__ @RPMTAG_CAPABILITY@

    __defined at:__ @rpm\/rpmtag.h:181:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_CAPABILITY :: RpmTag
pattern RPMTAG_CAPABILITY = RpmTag 1105

{-| __C declaration:__ @RPMTAG_SOURCEPACKAGE@

    __defined at:__ @rpm\/rpmtag.h:182:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SOURCEPACKAGE :: RpmTag
pattern RPMTAG_SOURCEPACKAGE = RpmTag 1106

{-| __C declaration:__ @RPMTAG_OLDORIGFILENAMES@

    __defined at:__ @rpm\/rpmtag.h:183:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_OLDORIGFILENAMES :: RpmTag
pattern RPMTAG_OLDORIGFILENAMES = RpmTag 1107

{-| __C declaration:__ @RPMTAG_BUILDPREREQ@

    __defined at:__ @rpm\/rpmtag.h:184:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BUILDPREREQ :: RpmTag
pattern RPMTAG_BUILDPREREQ = RpmTag 1108

{-| __C declaration:__ @RPMTAG_BUILDREQUIRES@

    __defined at:__ @rpm\/rpmtag.h:185:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BUILDREQUIRES :: RpmTag
pattern RPMTAG_BUILDREQUIRES = RpmTag 1109

{-| __C declaration:__ @RPMTAG_BUILDCONFLICTS@

    __defined at:__ @rpm\/rpmtag.h:186:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BUILDCONFLICTS :: RpmTag
pattern RPMTAG_BUILDCONFLICTS = RpmTag 1110

{-| __C declaration:__ @RPMTAG_BUILDMACROS@

    __defined at:__ @rpm\/rpmtag.h:187:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BUILDMACROS :: RpmTag
pattern RPMTAG_BUILDMACROS = RpmTag 1111

{-| __C declaration:__ @RPMTAG_PROVIDEFLAGS@

    __defined at:__ @rpm\/rpmtag.h:188:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PROVIDEFLAGS :: RpmTag
pattern RPMTAG_PROVIDEFLAGS = RpmTag 1112

{-| __C declaration:__ @RPMTAG_PROVIDEVERSION@

    __defined at:__ @rpm\/rpmtag.h:189:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PROVIDEVERSION :: RpmTag
pattern RPMTAG_PROVIDEVERSION = RpmTag 1113

{-| __C declaration:__ @RPMTAG_OBSOLETEFLAGS@

    __defined at:__ @rpm\/rpmtag.h:190:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_OBSOLETEFLAGS :: RpmTag
pattern RPMTAG_OBSOLETEFLAGS = RpmTag 1114

{-| __C declaration:__ @RPMTAG_OBSOLETEVERSION@

    __defined at:__ @rpm\/rpmtag.h:191:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_OBSOLETEVERSION :: RpmTag
pattern RPMTAG_OBSOLETEVERSION = RpmTag 1115

{-| __C declaration:__ @RPMTAG_DIRINDEXES@

    __defined at:__ @rpm\/rpmtag.h:192:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_DIRINDEXES :: RpmTag
pattern RPMTAG_DIRINDEXES = RpmTag 1116

{-| __C declaration:__ @RPMTAG_BASENAMES@

    __defined at:__ @rpm\/rpmtag.h:193:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BASENAMES :: RpmTag
pattern RPMTAG_BASENAMES = RpmTag 1117

{-| __C declaration:__ @RPMTAG_DIRNAMES@

    __defined at:__ @rpm\/rpmtag.h:194:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_DIRNAMES :: RpmTag
pattern RPMTAG_DIRNAMES = RpmTag 1118

{-| __C declaration:__ @RPMTAG_ORIGDIRINDEXES@

    __defined at:__ @rpm\/rpmtag.h:195:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_ORIGDIRINDEXES :: RpmTag
pattern RPMTAG_ORIGDIRINDEXES = RpmTag 1119

{-| __C declaration:__ @RPMTAG_ORIGBASENAMES@

    __defined at:__ @rpm\/rpmtag.h:196:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_ORIGBASENAMES :: RpmTag
pattern RPMTAG_ORIGBASENAMES = RpmTag 1120

{-| __C declaration:__ @RPMTAG_ORIGDIRNAMES@

    __defined at:__ @rpm\/rpmtag.h:197:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_ORIGDIRNAMES :: RpmTag
pattern RPMTAG_ORIGDIRNAMES = RpmTag 1121

{-| __C declaration:__ @RPMTAG_OPTFLAGS@

    __defined at:__ @rpm\/rpmtag.h:198:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_OPTFLAGS :: RpmTag
pattern RPMTAG_OPTFLAGS = RpmTag 1122

{-| __C declaration:__ @RPMTAG_DISTURL@

    __defined at:__ @rpm\/rpmtag.h:199:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_DISTURL :: RpmTag
pattern RPMTAG_DISTURL = RpmTag 1123

{-| __C declaration:__ @RPMTAG_PAYLOADFORMAT@

    __defined at:__ @rpm\/rpmtag.h:200:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PAYLOADFORMAT :: RpmTag
pattern RPMTAG_PAYLOADFORMAT = RpmTag 1124

{-| __C declaration:__ @RPMTAG_PAYLOADCOMPRESSOR@

    __defined at:__ @rpm\/rpmtag.h:201:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PAYLOADCOMPRESSOR :: RpmTag
pattern RPMTAG_PAYLOADCOMPRESSOR = RpmTag 1125

{-| __C declaration:__ @RPMTAG_PAYLOADFLAGS@

    __defined at:__ @rpm\/rpmtag.h:202:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PAYLOADFLAGS :: RpmTag
pattern RPMTAG_PAYLOADFLAGS = RpmTag 1126

{-| __C declaration:__ @RPMTAG_INSTALLCOLOR@

    __defined at:__ @rpm\/rpmtag.h:203:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_INSTALLCOLOR :: RpmTag
pattern RPMTAG_INSTALLCOLOR = RpmTag 1127

{-| __C declaration:__ @RPMTAG_INSTALLTID@

    __defined at:__ @rpm\/rpmtag.h:204:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_INSTALLTID :: RpmTag
pattern RPMTAG_INSTALLTID = RpmTag 1128

{-| __C declaration:__ @RPMTAG_REMOVETID@

    __defined at:__ @rpm\/rpmtag.h:205:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_REMOVETID :: RpmTag
pattern RPMTAG_REMOVETID = RpmTag 1129

{-| __C declaration:__ @RPMTAG_SHA1RHN@

    __defined at:__ @rpm\/rpmtag.h:206:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SHA1RHN :: RpmTag
pattern RPMTAG_SHA1RHN = RpmTag 1130

{-| __C declaration:__ @RPMTAG_RHNPLATFORM@

    __defined at:__ @rpm\/rpmtag.h:207:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_RHNPLATFORM :: RpmTag
pattern RPMTAG_RHNPLATFORM = RpmTag 1131

{-| __C declaration:__ @RPMTAG_PLATFORM@

    __defined at:__ @rpm\/rpmtag.h:208:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PLATFORM :: RpmTag
pattern RPMTAG_PLATFORM = RpmTag 1132

{-| __C declaration:__ @RPMTAG_PATCHESNAME@

    __defined at:__ @rpm\/rpmtag.h:209:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PATCHESNAME :: RpmTag
pattern RPMTAG_PATCHESNAME = RpmTag 1133

{-| __C declaration:__ @RPMTAG_PATCHESFLAGS@

    __defined at:__ @rpm\/rpmtag.h:210:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PATCHESFLAGS :: RpmTag
pattern RPMTAG_PATCHESFLAGS = RpmTag 1134

{-| __C declaration:__ @RPMTAG_PATCHESVERSION@

    __defined at:__ @rpm\/rpmtag.h:211:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PATCHESVERSION :: RpmTag
pattern RPMTAG_PATCHESVERSION = RpmTag 1135

{-| __C declaration:__ @RPMTAG_CACHECTIME@

    __defined at:__ @rpm\/rpmtag.h:212:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_CACHECTIME :: RpmTag
pattern RPMTAG_CACHECTIME = RpmTag 1136

{-| __C declaration:__ @RPMTAG_CACHEPKGPATH@

    __defined at:__ @rpm\/rpmtag.h:213:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_CACHEPKGPATH :: RpmTag
pattern RPMTAG_CACHEPKGPATH = RpmTag 1137

{-| __C declaration:__ @RPMTAG_CACHEPKGSIZE@

    __defined at:__ @rpm\/rpmtag.h:214:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_CACHEPKGSIZE :: RpmTag
pattern RPMTAG_CACHEPKGSIZE = RpmTag 1138

{-| __C declaration:__ @RPMTAG_CACHEPKGMTIME@

    __defined at:__ @rpm\/rpmtag.h:215:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_CACHEPKGMTIME :: RpmTag
pattern RPMTAG_CACHEPKGMTIME = RpmTag 1139

{-| __C declaration:__ @RPMTAG_FILECOLORS@

    __defined at:__ @rpm\/rpmtag.h:216:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILECOLORS :: RpmTag
pattern RPMTAG_FILECOLORS = RpmTag 1140

{-| __C declaration:__ @RPMTAG_FILECLASS@

    __defined at:__ @rpm\/rpmtag.h:217:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILECLASS :: RpmTag
pattern RPMTAG_FILECLASS = RpmTag 1141

{-| __C declaration:__ @RPMTAG_CLASSDICT@

    __defined at:__ @rpm\/rpmtag.h:218:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_CLASSDICT :: RpmTag
pattern RPMTAG_CLASSDICT = RpmTag 1142

{-| __C declaration:__ @RPMTAG_FILEDEPENDSX@

    __defined at:__ @rpm\/rpmtag.h:219:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILEDEPENDSX :: RpmTag
pattern RPMTAG_FILEDEPENDSX = RpmTag 1143

{-| __C declaration:__ @RPMTAG_FILEDEPENDSN@

    __defined at:__ @rpm\/rpmtag.h:220:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILEDEPENDSN :: RpmTag
pattern RPMTAG_FILEDEPENDSN = RpmTag 1144

{-| __C declaration:__ @RPMTAG_DEPENDSDICT@

    __defined at:__ @rpm\/rpmtag.h:221:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_DEPENDSDICT :: RpmTag
pattern RPMTAG_DEPENDSDICT = RpmTag 1145

{-| __C declaration:__ @RPMTAG_SOURCESIGMD5@

    __defined at:__ @rpm\/rpmtag.h:222:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SOURCESIGMD5 :: RpmTag
pattern RPMTAG_SOURCESIGMD5 = RpmTag 1146

{-| __C declaration:__ @RPMTAG_FILECONTEXTS@

    __defined at:__ @rpm\/rpmtag.h:223:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILECONTEXTS :: RpmTag
pattern RPMTAG_FILECONTEXTS = RpmTag 1147

{-| __C declaration:__ @RPMTAG_FSCONTEXTS@

    __defined at:__ @rpm\/rpmtag.h:224:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FSCONTEXTS :: RpmTag
pattern RPMTAG_FSCONTEXTS = RpmTag 1148

{-| __C declaration:__ @RPMTAG_RECONTEXTS@

    __defined at:__ @rpm\/rpmtag.h:225:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_RECONTEXTS :: RpmTag
pattern RPMTAG_RECONTEXTS = RpmTag 1149

{-| __C declaration:__ @RPMTAG_POLICIES@

    __defined at:__ @rpm\/rpmtag.h:226:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_POLICIES :: RpmTag
pattern RPMTAG_POLICIES = RpmTag 1150

{-| __C declaration:__ @RPMTAG_PRETRANS@

    __defined at:__ @rpm\/rpmtag.h:227:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PRETRANS :: RpmTag
pattern RPMTAG_PRETRANS = RpmTag 1151

{-| __C declaration:__ @RPMTAG_POSTTRANS@

    __defined at:__ @rpm\/rpmtag.h:228:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_POSTTRANS :: RpmTag
pattern RPMTAG_POSTTRANS = RpmTag 1152

{-| __C declaration:__ @RPMTAG_PRETRANSPROG@

    __defined at:__ @rpm\/rpmtag.h:229:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PRETRANSPROG :: RpmTag
pattern RPMTAG_PRETRANSPROG = RpmTag 1153

{-| __C declaration:__ @RPMTAG_POSTTRANSPROG@

    __defined at:__ @rpm\/rpmtag.h:230:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_POSTTRANSPROG :: RpmTag
pattern RPMTAG_POSTTRANSPROG = RpmTag 1154

{-| __C declaration:__ @RPMTAG_DISTTAG@

    __defined at:__ @rpm\/rpmtag.h:231:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_DISTTAG :: RpmTag
pattern RPMTAG_DISTTAG = RpmTag 1155

{-| __C declaration:__ @RPMTAG_OLDSUGGESTSNAME@

    __defined at:__ @rpm\/rpmtag.h:232:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_OLDSUGGESTSNAME :: RpmTag
pattern RPMTAG_OLDSUGGESTSNAME = RpmTag 1156

{-| __C declaration:__ @RPMTAG_OLDSUGGESTSVERSION@

    __defined at:__ @rpm\/rpmtag.h:234:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_OLDSUGGESTSVERSION :: RpmTag
pattern RPMTAG_OLDSUGGESTSVERSION = RpmTag 1157

{-| __C declaration:__ @RPMTAG_OLDSUGGESTSFLAGS@

    __defined at:__ @rpm\/rpmtag.h:235:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_OLDSUGGESTSFLAGS :: RpmTag
pattern RPMTAG_OLDSUGGESTSFLAGS = RpmTag 1158

{-| __C declaration:__ @RPMTAG_OLDENHANCESNAME@

    __defined at:__ @rpm\/rpmtag.h:236:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_OLDENHANCESNAME :: RpmTag
pattern RPMTAG_OLDENHANCESNAME = RpmTag 1159

{-| __C declaration:__ @RPMTAG_OLDENHANCESVERSION@

    __defined at:__ @rpm\/rpmtag.h:238:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_OLDENHANCESVERSION :: RpmTag
pattern RPMTAG_OLDENHANCESVERSION = RpmTag 1160

{-| __C declaration:__ @RPMTAG_OLDENHANCESFLAGS@

    __defined at:__ @rpm\/rpmtag.h:239:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_OLDENHANCESFLAGS :: RpmTag
pattern RPMTAG_OLDENHANCESFLAGS = RpmTag 1161

{-| __C declaration:__ @RPMTAG_PRIORITY@

    __defined at:__ @rpm\/rpmtag.h:240:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PRIORITY :: RpmTag
pattern RPMTAG_PRIORITY = RpmTag 1162

{-| __C declaration:__ @RPMTAG_CVSID@

    __defined at:__ @rpm\/rpmtag.h:241:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_CVSID :: RpmTag
pattern RPMTAG_CVSID = RpmTag 1163

{-| __C declaration:__ @RPMTAG_BLINKPKGID@

    __defined at:__ @rpm\/rpmtag.h:243:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BLINKPKGID :: RpmTag
pattern RPMTAG_BLINKPKGID = RpmTag 1164

{-| __C declaration:__ @RPMTAG_BLINKHDRID@

    __defined at:__ @rpm\/rpmtag.h:244:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BLINKHDRID :: RpmTag
pattern RPMTAG_BLINKHDRID = RpmTag 1165

{-| __C declaration:__ @RPMTAG_BLINKNEVRA@

    __defined at:__ @rpm\/rpmtag.h:245:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BLINKNEVRA :: RpmTag
pattern RPMTAG_BLINKNEVRA = RpmTag 1166

{-| __C declaration:__ @RPMTAG_FLINKPKGID@

    __defined at:__ @rpm\/rpmtag.h:246:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FLINKPKGID :: RpmTag
pattern RPMTAG_FLINKPKGID = RpmTag 1167

{-| __C declaration:__ @RPMTAG_FLINKHDRID@

    __defined at:__ @rpm\/rpmtag.h:247:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FLINKHDRID :: RpmTag
pattern RPMTAG_FLINKHDRID = RpmTag 1168

{-| __C declaration:__ @RPMTAG_FLINKNEVRA@

    __defined at:__ @rpm\/rpmtag.h:248:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FLINKNEVRA :: RpmTag
pattern RPMTAG_FLINKNEVRA = RpmTag 1169

{-| __C declaration:__ @RPMTAG_PACKAGEORIGIN@

    __defined at:__ @rpm\/rpmtag.h:249:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PACKAGEORIGIN :: RpmTag
pattern RPMTAG_PACKAGEORIGIN = RpmTag 1170

{-| __C declaration:__ @RPMTAG_TRIGGERPREIN@

    __defined at:__ @rpm\/rpmtag.h:250:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRIGGERPREIN :: RpmTag
pattern RPMTAG_TRIGGERPREIN = RpmTag 1171

{-| __C declaration:__ @RPMTAG_BUILDSUGGESTS@

    __defined at:__ @rpm\/rpmtag.h:251:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BUILDSUGGESTS :: RpmTag
pattern RPMTAG_BUILDSUGGESTS = RpmTag 1172

{-| __C declaration:__ @RPMTAG_BUILDENHANCES@

    __defined at:__ @rpm\/rpmtag.h:252:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BUILDENHANCES :: RpmTag
pattern RPMTAG_BUILDENHANCES = RpmTag 1173

{-| __C declaration:__ @RPMTAG_SCRIPTSTATES@

    __defined at:__ @rpm\/rpmtag.h:253:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SCRIPTSTATES :: RpmTag
pattern RPMTAG_SCRIPTSTATES = RpmTag 1174

{-| __C declaration:__ @RPMTAG_SCRIPTMETRICS@

    __defined at:__ @rpm\/rpmtag.h:254:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SCRIPTMETRICS :: RpmTag
pattern RPMTAG_SCRIPTMETRICS = RpmTag 1175

{-| __C declaration:__ @RPMTAG_BUILDCPUCLOCK@

    __defined at:__ @rpm\/rpmtag.h:255:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BUILDCPUCLOCK :: RpmTag
pattern RPMTAG_BUILDCPUCLOCK = RpmTag 1176

{-| __C declaration:__ @RPMTAG_FILEDIGESTALGOS@

    __defined at:__ @rpm\/rpmtag.h:256:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILEDIGESTALGOS :: RpmTag
pattern RPMTAG_FILEDIGESTALGOS = RpmTag 1177

{-| __C declaration:__ @RPMTAG_VARIANTS@

    __defined at:__ @rpm\/rpmtag.h:257:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_VARIANTS :: RpmTag
pattern RPMTAG_VARIANTS = RpmTag 1178

{-| __C declaration:__ @RPMTAG_XMAJOR@

    __defined at:__ @rpm\/rpmtag.h:258:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_XMAJOR :: RpmTag
pattern RPMTAG_XMAJOR = RpmTag 1179

{-| __C declaration:__ @RPMTAG_XMINOR@

    __defined at:__ @rpm\/rpmtag.h:259:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_XMINOR :: RpmTag
pattern RPMTAG_XMINOR = RpmTag 1180

{-| __C declaration:__ @RPMTAG_REPOTAG@

    __defined at:__ @rpm\/rpmtag.h:260:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_REPOTAG :: RpmTag
pattern RPMTAG_REPOTAG = RpmTag 1181

{-| __C declaration:__ @RPMTAG_KEYWORDS@

    __defined at:__ @rpm\/rpmtag.h:261:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_KEYWORDS :: RpmTag
pattern RPMTAG_KEYWORDS = RpmTag 1182

{-| __C declaration:__ @RPMTAG_BUILDPLATFORMS@

    __defined at:__ @rpm\/rpmtag.h:262:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BUILDPLATFORMS :: RpmTag
pattern RPMTAG_BUILDPLATFORMS = RpmTag 1183

{-| __C declaration:__ @RPMTAG_PACKAGECOLOR@

    __defined at:__ @rpm\/rpmtag.h:263:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PACKAGECOLOR :: RpmTag
pattern RPMTAG_PACKAGECOLOR = RpmTag 1184

{-| __C declaration:__ @RPMTAG_PACKAGEPREFCOLOR@

    __defined at:__ @rpm\/rpmtag.h:264:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PACKAGEPREFCOLOR :: RpmTag
pattern RPMTAG_PACKAGEPREFCOLOR = RpmTag 1185

{-| __C declaration:__ @RPMTAG_XATTRSDICT@

    __defined at:__ @rpm\/rpmtag.h:265:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_XATTRSDICT :: RpmTag
pattern RPMTAG_XATTRSDICT = RpmTag 1186

{-| __C declaration:__ @RPMTAG_FILEXATTRSX@

    __defined at:__ @rpm\/rpmtag.h:266:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILEXATTRSX :: RpmTag
pattern RPMTAG_FILEXATTRSX = RpmTag 1187

{-| __C declaration:__ @RPMTAG_DEPATTRSDICT@

    __defined at:__ @rpm\/rpmtag.h:267:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_DEPATTRSDICT :: RpmTag
pattern RPMTAG_DEPATTRSDICT = RpmTag 1188

{-| __C declaration:__ @RPMTAG_CONFLICTATTRSX@

    __defined at:__ @rpm\/rpmtag.h:268:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_CONFLICTATTRSX :: RpmTag
pattern RPMTAG_CONFLICTATTRSX = RpmTag 1189

{-| __C declaration:__ @RPMTAG_OBSOLETEATTRSX@

    __defined at:__ @rpm\/rpmtag.h:269:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_OBSOLETEATTRSX :: RpmTag
pattern RPMTAG_OBSOLETEATTRSX = RpmTag 1190

{-| __C declaration:__ @RPMTAG_PROVIDEATTRSX@

    __defined at:__ @rpm\/rpmtag.h:270:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PROVIDEATTRSX :: RpmTag
pattern RPMTAG_PROVIDEATTRSX = RpmTag 1191

{-| __C declaration:__ @RPMTAG_REQUIREATTRSX@

    __defined at:__ @rpm\/rpmtag.h:271:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_REQUIREATTRSX :: RpmTag
pattern RPMTAG_REQUIREATTRSX = RpmTag 1192

{-| __C declaration:__ @RPMTAG_BUILDPROVIDES@

    __defined at:__ @rpm\/rpmtag.h:272:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BUILDPROVIDES :: RpmTag
pattern RPMTAG_BUILDPROVIDES = RpmTag 1193

{-| __C declaration:__ @RPMTAG_BUILDOBSOLETES@

    __defined at:__ @rpm\/rpmtag.h:273:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BUILDOBSOLETES :: RpmTag
pattern RPMTAG_BUILDOBSOLETES = RpmTag 1194

{-| __C declaration:__ @RPMTAG_DBINSTANCE@

    __defined at:__ @rpm\/rpmtag.h:274:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_DBINSTANCE :: RpmTag
pattern RPMTAG_DBINSTANCE = RpmTag 1195

{-| __C declaration:__ @RPMTAG_NVRA@

    __defined at:__ @rpm\/rpmtag.h:275:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_NVRA :: RpmTag
pattern RPMTAG_NVRA = RpmTag 1196

{-| __C declaration:__ @RPMTAG_FILENAMES@

    __defined at:__ @rpm\/rpmtag.h:278:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILENAMES :: RpmTag
pattern RPMTAG_FILENAMES = RpmTag 5000

{-| __C declaration:__ @RPMTAG_FILEPROVIDE@

    __defined at:__ @rpm\/rpmtag.h:279:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILEPROVIDE :: RpmTag
pattern RPMTAG_FILEPROVIDE = RpmTag 5001

{-| __C declaration:__ @RPMTAG_FILEREQUIRE@

    __defined at:__ @rpm\/rpmtag.h:280:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILEREQUIRE :: RpmTag
pattern RPMTAG_FILEREQUIRE = RpmTag 5002

{-| __C declaration:__ @RPMTAG_FSNAMES@

    __defined at:__ @rpm\/rpmtag.h:281:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FSNAMES :: RpmTag
pattern RPMTAG_FSNAMES = RpmTag 5003

{-| __C declaration:__ @RPMTAG_FSSIZES@

    __defined at:__ @rpm\/rpmtag.h:282:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FSSIZES :: RpmTag
pattern RPMTAG_FSSIZES = RpmTag 5004

{-| __C declaration:__ @RPMTAG_TRIGGERCONDS@

    __defined at:__ @rpm\/rpmtag.h:283:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRIGGERCONDS :: RpmTag
pattern RPMTAG_TRIGGERCONDS = RpmTag 5005

{-| __C declaration:__ @RPMTAG_TRIGGERTYPE@

    __defined at:__ @rpm\/rpmtag.h:284:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRIGGERTYPE :: RpmTag
pattern RPMTAG_TRIGGERTYPE = RpmTag 5006

{-| __C declaration:__ @RPMTAG_ORIGFILENAMES@

    __defined at:__ @rpm\/rpmtag.h:285:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_ORIGFILENAMES :: RpmTag
pattern RPMTAG_ORIGFILENAMES = RpmTag 5007

{-| __C declaration:__ @RPMTAG_LONGFILESIZES@

    __defined at:__ @rpm\/rpmtag.h:286:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_LONGFILESIZES :: RpmTag
pattern RPMTAG_LONGFILESIZES = RpmTag 5008

{-| __C declaration:__ @RPMTAG_LONGSIZE@

    __defined at:__ @rpm\/rpmtag.h:287:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_LONGSIZE :: RpmTag
pattern RPMTAG_LONGSIZE = RpmTag 5009

{-| __C declaration:__ @RPMTAG_FILECAPS@

    __defined at:__ @rpm\/rpmtag.h:288:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILECAPS :: RpmTag
pattern RPMTAG_FILECAPS = RpmTag 5010

{-| __C declaration:__ @RPMTAG_FILEDIGESTALGO@

    __defined at:__ @rpm\/rpmtag.h:289:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILEDIGESTALGO :: RpmTag
pattern RPMTAG_FILEDIGESTALGO = RpmTag 5011

{-| __C declaration:__ @RPMTAG_BUGURL@

    __defined at:__ @rpm\/rpmtag.h:290:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BUGURL :: RpmTag
pattern RPMTAG_BUGURL = RpmTag 5012

{-| __C declaration:__ @RPMTAG_EVR@

    __defined at:__ @rpm\/rpmtag.h:291:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_EVR :: RpmTag
pattern RPMTAG_EVR = RpmTag 5013

{-| __C declaration:__ @RPMTAG_NVR@

    __defined at:__ @rpm\/rpmtag.h:292:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_NVR :: RpmTag
pattern RPMTAG_NVR = RpmTag 5014

{-| __C declaration:__ @RPMTAG_NEVR@

    __defined at:__ @rpm\/rpmtag.h:293:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_NEVR :: RpmTag
pattern RPMTAG_NEVR = RpmTag 5015

{-| __C declaration:__ @RPMTAG_NEVRA@

    __defined at:__ @rpm\/rpmtag.h:294:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_NEVRA :: RpmTag
pattern RPMTAG_NEVRA = RpmTag 5016

{-| __C declaration:__ @RPMTAG_HEADERCOLOR@

    __defined at:__ @rpm\/rpmtag.h:295:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_HEADERCOLOR :: RpmTag
pattern RPMTAG_HEADERCOLOR = RpmTag 5017

{-| __C declaration:__ @RPMTAG_VERBOSE@

    __defined at:__ @rpm\/rpmtag.h:296:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_VERBOSE :: RpmTag
pattern RPMTAG_VERBOSE = RpmTag 5018

{-| __C declaration:__ @RPMTAG_EPOCHNUM@

    __defined at:__ @rpm\/rpmtag.h:297:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_EPOCHNUM :: RpmTag
pattern RPMTAG_EPOCHNUM = RpmTag 5019

{-| __C declaration:__ @RPMTAG_PREINFLAGS@

    __defined at:__ @rpm\/rpmtag.h:298:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PREINFLAGS :: RpmTag
pattern RPMTAG_PREINFLAGS = RpmTag 5020

{-| __C declaration:__ @RPMTAG_POSTINFLAGS@

    __defined at:__ @rpm\/rpmtag.h:299:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_POSTINFLAGS :: RpmTag
pattern RPMTAG_POSTINFLAGS = RpmTag 5021

{-| __C declaration:__ @RPMTAG_PREUNFLAGS@

    __defined at:__ @rpm\/rpmtag.h:300:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PREUNFLAGS :: RpmTag
pattern RPMTAG_PREUNFLAGS = RpmTag 5022

{-| __C declaration:__ @RPMTAG_POSTUNFLAGS@

    __defined at:__ @rpm\/rpmtag.h:301:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_POSTUNFLAGS :: RpmTag
pattern RPMTAG_POSTUNFLAGS = RpmTag 5023

{-| __C declaration:__ @RPMTAG_PRETRANSFLAGS@

    __defined at:__ @rpm\/rpmtag.h:302:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PRETRANSFLAGS :: RpmTag
pattern RPMTAG_PRETRANSFLAGS = RpmTag 5024

{-| __C declaration:__ @RPMTAG_POSTTRANSFLAGS@

    __defined at:__ @rpm\/rpmtag.h:303:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_POSTTRANSFLAGS :: RpmTag
pattern RPMTAG_POSTTRANSFLAGS = RpmTag 5025

{-| __C declaration:__ @RPMTAG_VERIFYSCRIPTFLAGS@

    __defined at:__ @rpm\/rpmtag.h:304:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_VERIFYSCRIPTFLAGS :: RpmTag
pattern RPMTAG_VERIFYSCRIPTFLAGS = RpmTag 5026

{-| __C declaration:__ @RPMTAG_TRIGGERSCRIPTFLAGS@

    __defined at:__ @rpm\/rpmtag.h:305:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRIGGERSCRIPTFLAGS :: RpmTag
pattern RPMTAG_TRIGGERSCRIPTFLAGS = RpmTag 5027

{-| __C declaration:__ @RPMTAG_COLLECTIONS@

    __defined at:__ @rpm\/rpmtag.h:306:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_COLLECTIONS :: RpmTag
pattern RPMTAG_COLLECTIONS = RpmTag 5029

{-| __C declaration:__ @RPMTAG_POLICYNAMES@

    __defined at:__ @rpm\/rpmtag.h:307:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_POLICYNAMES :: RpmTag
pattern RPMTAG_POLICYNAMES = RpmTag 5030

{-| __C declaration:__ @RPMTAG_POLICYTYPES@

    __defined at:__ @rpm\/rpmtag.h:308:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_POLICYTYPES :: RpmTag
pattern RPMTAG_POLICYTYPES = RpmTag 5031

{-| __C declaration:__ @RPMTAG_POLICYTYPESINDEXES@

    __defined at:__ @rpm\/rpmtag.h:309:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_POLICYTYPESINDEXES :: RpmTag
pattern RPMTAG_POLICYTYPESINDEXES = RpmTag 5032

{-| __C declaration:__ @RPMTAG_POLICYFLAGS@

    __defined at:__ @rpm\/rpmtag.h:310:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_POLICYFLAGS :: RpmTag
pattern RPMTAG_POLICYFLAGS = RpmTag 5033

{-| __C declaration:__ @RPMTAG_VCS@

    __defined at:__ @rpm\/rpmtag.h:311:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_VCS :: RpmTag
pattern RPMTAG_VCS = RpmTag 5034

{-| __C declaration:__ @RPMTAG_ORDERNAME@

    __defined at:__ @rpm\/rpmtag.h:312:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_ORDERNAME :: RpmTag
pattern RPMTAG_ORDERNAME = RpmTag 5035

{-| __C declaration:__ @RPMTAG_ORDERVERSION@

    __defined at:__ @rpm\/rpmtag.h:313:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_ORDERVERSION :: RpmTag
pattern RPMTAG_ORDERVERSION = RpmTag 5036

{-| __C declaration:__ @RPMTAG_ORDERFLAGS@

    __defined at:__ @rpm\/rpmtag.h:314:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_ORDERFLAGS :: RpmTag
pattern RPMTAG_ORDERFLAGS = RpmTag 5037

{-| __C declaration:__ @RPMTAG_MSSFMANIFEST@

    __defined at:__ @rpm\/rpmtag.h:315:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_MSSFMANIFEST :: RpmTag
pattern RPMTAG_MSSFMANIFEST = RpmTag 5038

{-| __C declaration:__ @RPMTAG_MSSFDOMAIN@

    __defined at:__ @rpm\/rpmtag.h:316:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_MSSFDOMAIN :: RpmTag
pattern RPMTAG_MSSFDOMAIN = RpmTag 5039

{-| __C declaration:__ @RPMTAG_INSTFILENAMES@

    __defined at:__ @rpm\/rpmtag.h:317:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_INSTFILENAMES :: RpmTag
pattern RPMTAG_INSTFILENAMES = RpmTag 5040

{-| __C declaration:__ @RPMTAG_REQUIRENEVRS@

    __defined at:__ @rpm\/rpmtag.h:318:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_REQUIRENEVRS :: RpmTag
pattern RPMTAG_REQUIRENEVRS = RpmTag 5041

{-| __C declaration:__ @RPMTAG_PROVIDENEVRS@

    __defined at:__ @rpm\/rpmtag.h:319:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PROVIDENEVRS :: RpmTag
pattern RPMTAG_PROVIDENEVRS = RpmTag 5042

{-| __C declaration:__ @RPMTAG_OBSOLETENEVRS@

    __defined at:__ @rpm\/rpmtag.h:320:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_OBSOLETENEVRS :: RpmTag
pattern RPMTAG_OBSOLETENEVRS = RpmTag 5043

{-| __C declaration:__ @RPMTAG_CONFLICTNEVRS@

    __defined at:__ @rpm\/rpmtag.h:321:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_CONFLICTNEVRS :: RpmTag
pattern RPMTAG_CONFLICTNEVRS = RpmTag 5044

{-| __C declaration:__ @RPMTAG_FILENLINKS@

    __defined at:__ @rpm\/rpmtag.h:322:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILENLINKS :: RpmTag
pattern RPMTAG_FILENLINKS = RpmTag 5045

{-| __C declaration:__ @RPMTAG_RECOMMENDNAME@

    __defined at:__ @rpm\/rpmtag.h:323:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_RECOMMENDNAME :: RpmTag
pattern RPMTAG_RECOMMENDNAME = RpmTag 5046

{-| __C declaration:__ @RPMTAG_RECOMMENDVERSION@

    __defined at:__ @rpm\/rpmtag.h:325:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_RECOMMENDVERSION :: RpmTag
pattern RPMTAG_RECOMMENDVERSION = RpmTag 5047

{-| __C declaration:__ @RPMTAG_RECOMMENDFLAGS@

    __defined at:__ @rpm\/rpmtag.h:326:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_RECOMMENDFLAGS :: RpmTag
pattern RPMTAG_RECOMMENDFLAGS = RpmTag 5048

{-| __C declaration:__ @RPMTAG_SUGGESTNAME@

    __defined at:__ @rpm\/rpmtag.h:327:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SUGGESTNAME :: RpmTag
pattern RPMTAG_SUGGESTNAME = RpmTag 5049

{-| __C declaration:__ @RPMTAG_SUGGESTVERSION@

    __defined at:__ @rpm\/rpmtag.h:329:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SUGGESTVERSION :: RpmTag
pattern RPMTAG_SUGGESTVERSION = RpmTag 5050

{-| __C declaration:__ @RPMTAG_SUGGESTFLAGS@

    __defined at:__ @rpm\/rpmtag.h:330:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SUGGESTFLAGS :: RpmTag
pattern RPMTAG_SUGGESTFLAGS = RpmTag 5051

{-| __C declaration:__ @RPMTAG_SUPPLEMENTNAME@

    __defined at:__ @rpm\/rpmtag.h:331:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SUPPLEMENTNAME :: RpmTag
pattern RPMTAG_SUPPLEMENTNAME = RpmTag 5052

{-| __C declaration:__ @RPMTAG_SUPPLEMENTVERSION@

    __defined at:__ @rpm\/rpmtag.h:333:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SUPPLEMENTVERSION :: RpmTag
pattern RPMTAG_SUPPLEMENTVERSION = RpmTag 5053

{-| __C declaration:__ @RPMTAG_SUPPLEMENTFLAGS@

    __defined at:__ @rpm\/rpmtag.h:334:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SUPPLEMENTFLAGS :: RpmTag
pattern RPMTAG_SUPPLEMENTFLAGS = RpmTag 5054

{-| __C declaration:__ @RPMTAG_ENHANCENAME@

    __defined at:__ @rpm\/rpmtag.h:335:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_ENHANCENAME :: RpmTag
pattern RPMTAG_ENHANCENAME = RpmTag 5055

{-| __C declaration:__ @RPMTAG_ENHANCEVERSION@

    __defined at:__ @rpm\/rpmtag.h:337:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_ENHANCEVERSION :: RpmTag
pattern RPMTAG_ENHANCEVERSION = RpmTag 5056

{-| __C declaration:__ @RPMTAG_ENHANCEFLAGS@

    __defined at:__ @rpm\/rpmtag.h:338:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_ENHANCEFLAGS :: RpmTag
pattern RPMTAG_ENHANCEFLAGS = RpmTag 5057

{-| __C declaration:__ @RPMTAG_RECOMMENDNEVRS@

    __defined at:__ @rpm\/rpmtag.h:339:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_RECOMMENDNEVRS :: RpmTag
pattern RPMTAG_RECOMMENDNEVRS = RpmTag 5058

{-| __C declaration:__ @RPMTAG_SUGGESTNEVRS@

    __defined at:__ @rpm\/rpmtag.h:340:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SUGGESTNEVRS :: RpmTag
pattern RPMTAG_SUGGESTNEVRS = RpmTag 5059

{-| __C declaration:__ @RPMTAG_SUPPLEMENTNEVRS@

    __defined at:__ @rpm\/rpmtag.h:341:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SUPPLEMENTNEVRS :: RpmTag
pattern RPMTAG_SUPPLEMENTNEVRS = RpmTag 5060

{-| __C declaration:__ @RPMTAG_ENHANCENEVRS@

    __defined at:__ @rpm\/rpmtag.h:342:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_ENHANCENEVRS :: RpmTag
pattern RPMTAG_ENHANCENEVRS = RpmTag 5061

{-| __C declaration:__ @RPMTAG_ENCODING@

    __defined at:__ @rpm\/rpmtag.h:343:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_ENCODING :: RpmTag
pattern RPMTAG_ENCODING = RpmTag 5062

{-| __C declaration:__ @RPMTAG_FILETRIGGERIN@

    __defined at:__ @rpm\/rpmtag.h:344:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILETRIGGERIN :: RpmTag
pattern RPMTAG_FILETRIGGERIN = RpmTag 5063

{-| __C declaration:__ @RPMTAG_FILETRIGGERUN@

    __defined at:__ @rpm\/rpmtag.h:345:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILETRIGGERUN :: RpmTag
pattern RPMTAG_FILETRIGGERUN = RpmTag 5064

{-| __C declaration:__ @RPMTAG_FILETRIGGERPOSTUN@

    __defined at:__ @rpm\/rpmtag.h:346:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILETRIGGERPOSTUN :: RpmTag
pattern RPMTAG_FILETRIGGERPOSTUN = RpmTag 5065

{-| __C declaration:__ @RPMTAG_FILETRIGGERSCRIPTS@

    __defined at:__ @rpm\/rpmtag.h:347:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILETRIGGERSCRIPTS :: RpmTag
pattern RPMTAG_FILETRIGGERSCRIPTS = RpmTag 5066

{-| __C declaration:__ @RPMTAG_FILETRIGGERSCRIPTPROG@

    __defined at:__ @rpm\/rpmtag.h:348:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILETRIGGERSCRIPTPROG :: RpmTag
pattern RPMTAG_FILETRIGGERSCRIPTPROG = RpmTag 5067

{-| __C declaration:__ @RPMTAG_FILETRIGGERSCRIPTFLAGS@

    __defined at:__ @rpm\/rpmtag.h:349:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILETRIGGERSCRIPTFLAGS :: RpmTag
pattern RPMTAG_FILETRIGGERSCRIPTFLAGS = RpmTag 5068

{-| __C declaration:__ @RPMTAG_FILETRIGGERNAME@

    __defined at:__ @rpm\/rpmtag.h:350:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILETRIGGERNAME :: RpmTag
pattern RPMTAG_FILETRIGGERNAME = RpmTag 5069

{-| __C declaration:__ @RPMTAG_FILETRIGGERINDEX@

    __defined at:__ @rpm\/rpmtag.h:351:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILETRIGGERINDEX :: RpmTag
pattern RPMTAG_FILETRIGGERINDEX = RpmTag 5070

{-| __C declaration:__ @RPMTAG_FILETRIGGERVERSION@

    __defined at:__ @rpm\/rpmtag.h:352:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILETRIGGERVERSION :: RpmTag
pattern RPMTAG_FILETRIGGERVERSION = RpmTag 5071

{-| __C declaration:__ @RPMTAG_FILETRIGGERFLAGS@

    __defined at:__ @rpm\/rpmtag.h:353:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILETRIGGERFLAGS :: RpmTag
pattern RPMTAG_FILETRIGGERFLAGS = RpmTag 5072

{-| __C declaration:__ @RPMTAG_TRANSFILETRIGGERIN@

    __defined at:__ @rpm\/rpmtag.h:354:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRANSFILETRIGGERIN :: RpmTag
pattern RPMTAG_TRANSFILETRIGGERIN = RpmTag 5073

{-| __C declaration:__ @RPMTAG_TRANSFILETRIGGERUN@

    __defined at:__ @rpm\/rpmtag.h:355:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRANSFILETRIGGERUN :: RpmTag
pattern RPMTAG_TRANSFILETRIGGERUN = RpmTag 5074

{-| __C declaration:__ @RPMTAG_TRANSFILETRIGGERPOSTUN@

    __defined at:__ @rpm\/rpmtag.h:356:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRANSFILETRIGGERPOSTUN :: RpmTag
pattern RPMTAG_TRANSFILETRIGGERPOSTUN = RpmTag 5075

{-| __C declaration:__ @RPMTAG_TRANSFILETRIGGERSCRIPTS@

    __defined at:__ @rpm\/rpmtag.h:357:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRANSFILETRIGGERSCRIPTS :: RpmTag
pattern RPMTAG_TRANSFILETRIGGERSCRIPTS = RpmTag 5076

{-| __C declaration:__ @RPMTAG_TRANSFILETRIGGERSCRIPTPROG@

    __defined at:__ @rpm\/rpmtag.h:358:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRANSFILETRIGGERSCRIPTPROG :: RpmTag
pattern RPMTAG_TRANSFILETRIGGERSCRIPTPROG = RpmTag 5077

{-| __C declaration:__ @RPMTAG_TRANSFILETRIGGERSCRIPTFLAGS@

    __defined at:__ @rpm\/rpmtag.h:359:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRANSFILETRIGGERSCRIPTFLAGS :: RpmTag
pattern RPMTAG_TRANSFILETRIGGERSCRIPTFLAGS = RpmTag 5078

{-| __C declaration:__ @RPMTAG_TRANSFILETRIGGERNAME@

    __defined at:__ @rpm\/rpmtag.h:360:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRANSFILETRIGGERNAME :: RpmTag
pattern RPMTAG_TRANSFILETRIGGERNAME = RpmTag 5079

{-| __C declaration:__ @RPMTAG_TRANSFILETRIGGERINDEX@

    __defined at:__ @rpm\/rpmtag.h:361:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRANSFILETRIGGERINDEX :: RpmTag
pattern RPMTAG_TRANSFILETRIGGERINDEX = RpmTag 5080

{-| __C declaration:__ @RPMTAG_TRANSFILETRIGGERVERSION@

    __defined at:__ @rpm\/rpmtag.h:362:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRANSFILETRIGGERVERSION :: RpmTag
pattern RPMTAG_TRANSFILETRIGGERVERSION = RpmTag 5081

{-| __C declaration:__ @RPMTAG_TRANSFILETRIGGERFLAGS@

    __defined at:__ @rpm\/rpmtag.h:363:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRANSFILETRIGGERFLAGS :: RpmTag
pattern RPMTAG_TRANSFILETRIGGERFLAGS = RpmTag 5082

{-| __C declaration:__ @RPMTAG_REMOVEPATHPOSTFIXES@

    __defined at:__ @rpm\/rpmtag.h:364:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_REMOVEPATHPOSTFIXES :: RpmTag
pattern RPMTAG_REMOVEPATHPOSTFIXES = RpmTag 5083

{-| __C declaration:__ @RPMTAG_FILETRIGGERPRIORITIES@

    __defined at:__ @rpm\/rpmtag.h:365:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILETRIGGERPRIORITIES :: RpmTag
pattern RPMTAG_FILETRIGGERPRIORITIES = RpmTag 5084

{-| __C declaration:__ @RPMTAG_TRANSFILETRIGGERPRIORITIES@

    __defined at:__ @rpm\/rpmtag.h:366:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRANSFILETRIGGERPRIORITIES :: RpmTag
pattern RPMTAG_TRANSFILETRIGGERPRIORITIES = RpmTag 5085

{-| __C declaration:__ @RPMTAG_FILETRIGGERCONDS@

    __defined at:__ @rpm\/rpmtag.h:367:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILETRIGGERCONDS :: RpmTag
pattern RPMTAG_FILETRIGGERCONDS = RpmTag 5086

{-| __C declaration:__ @RPMTAG_FILETRIGGERTYPE@

    __defined at:__ @rpm\/rpmtag.h:368:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILETRIGGERTYPE :: RpmTag
pattern RPMTAG_FILETRIGGERTYPE = RpmTag 5087

{-| __C declaration:__ @RPMTAG_TRANSFILETRIGGERCONDS@

    __defined at:__ @rpm\/rpmtag.h:369:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRANSFILETRIGGERCONDS :: RpmTag
pattern RPMTAG_TRANSFILETRIGGERCONDS = RpmTag 5088

{-| __C declaration:__ @RPMTAG_TRANSFILETRIGGERTYPE@

    __defined at:__ @rpm\/rpmtag.h:370:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRANSFILETRIGGERTYPE :: RpmTag
pattern RPMTAG_TRANSFILETRIGGERTYPE = RpmTag 5089

{-| __C declaration:__ @RPMTAG_FILESIGNATURES@

    __defined at:__ @rpm\/rpmtag.h:371:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILESIGNATURES :: RpmTag
pattern RPMTAG_FILESIGNATURES = RpmTag 5090

{-| __C declaration:__ @RPMTAG_FILESIGNATURELENGTH@

    __defined at:__ @rpm\/rpmtag.h:372:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILESIGNATURELENGTH :: RpmTag
pattern RPMTAG_FILESIGNATURELENGTH = RpmTag 5091

{-| __C declaration:__ @RPMTAG_PAYLOADSHA256@

    __defined at:__ @rpm\/rpmtag.h:373:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PAYLOADSHA256 :: RpmTag
pattern RPMTAG_PAYLOADSHA256 = RpmTag 5092

{-| __C declaration:__ @RPMTAG_PAYLOADSHA256ALGO@

    __defined at:__ @rpm\/rpmtag.h:374:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PAYLOADSHA256ALGO :: RpmTag
pattern RPMTAG_PAYLOADSHA256ALGO = RpmTag 5093

{-| __C declaration:__ @RPMTAG_AUTOINSTALLED@

    __defined at:__ @rpm\/rpmtag.h:375:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_AUTOINSTALLED :: RpmTag
pattern RPMTAG_AUTOINSTALLED = RpmTag 5094

{-| __C declaration:__ @RPMTAG_IDENTITY@

    __defined at:__ @rpm\/rpmtag.h:376:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_IDENTITY :: RpmTag
pattern RPMTAG_IDENTITY = RpmTag 5095

{-| __C declaration:__ @RPMTAG_MODULARITYLABEL@

    __defined at:__ @rpm\/rpmtag.h:377:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_MODULARITYLABEL :: RpmTag
pattern RPMTAG_MODULARITYLABEL = RpmTag 5096

{-| __C declaration:__ @RPMTAG_PAYLOADSHA256ALT@

    __defined at:__ @rpm\/rpmtag.h:378:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PAYLOADSHA256ALT :: RpmTag
pattern RPMTAG_PAYLOADSHA256ALT = RpmTag 5097

{-| __C declaration:__ @RPMTAG_ARCHSUFFIX@

    __defined at:__ @rpm\/rpmtag.h:379:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_ARCHSUFFIX :: RpmTag
pattern RPMTAG_ARCHSUFFIX = RpmTag 5098

{-| __C declaration:__ @RPMTAG_SPEC@

    __defined at:__ @rpm\/rpmtag.h:380:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SPEC :: RpmTag
pattern RPMTAG_SPEC = RpmTag 5099

{-| __C declaration:__ @RPMTAG_TRANSLATIONURL@

    __defined at:__ @rpm\/rpmtag.h:381:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_TRANSLATIONURL :: RpmTag
pattern RPMTAG_TRANSLATIONURL = RpmTag 5100

{-| __C declaration:__ @RPMTAG_UPSTREAMRELEASES@

    __defined at:__ @rpm\/rpmtag.h:382:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_UPSTREAMRELEASES :: RpmTag
pattern RPMTAG_UPSTREAMRELEASES = RpmTag 5101

{-| __C declaration:__ @RPMTAG_SOURCELICENSE@

    __defined at:__ @rpm\/rpmtag.h:383:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SOURCELICENSE :: RpmTag
pattern RPMTAG_SOURCELICENSE = RpmTag 5102

{-| __C declaration:__ @RPMTAG_PREUNTRANS@

    __defined at:__ @rpm\/rpmtag.h:384:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PREUNTRANS :: RpmTag
pattern RPMTAG_PREUNTRANS = RpmTag 5103

{-| __C declaration:__ @RPMTAG_POSTUNTRANS@

    __defined at:__ @rpm\/rpmtag.h:385:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_POSTUNTRANS :: RpmTag
pattern RPMTAG_POSTUNTRANS = RpmTag 5104

{-| __C declaration:__ @RPMTAG_PREUNTRANSPROG@

    __defined at:__ @rpm\/rpmtag.h:386:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PREUNTRANSPROG :: RpmTag
pattern RPMTAG_PREUNTRANSPROG = RpmTag 5105

{-| __C declaration:__ @RPMTAG_POSTUNTRANSPROG@

    __defined at:__ @rpm\/rpmtag.h:387:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_POSTUNTRANSPROG :: RpmTag
pattern RPMTAG_POSTUNTRANSPROG = RpmTag 5106

{-| __C declaration:__ @RPMTAG_PREUNTRANSFLAGS@

    __defined at:__ @rpm\/rpmtag.h:388:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PREUNTRANSFLAGS :: RpmTag
pattern RPMTAG_PREUNTRANSFLAGS = RpmTag 5107

{-| __C declaration:__ @RPMTAG_POSTUNTRANSFLAGS@

    __defined at:__ @rpm\/rpmtag.h:389:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_POSTUNTRANSFLAGS :: RpmTag
pattern RPMTAG_POSTUNTRANSFLAGS = RpmTag 5108

{-| __C declaration:__ @RPMTAG_SYSUSERS@

    __defined at:__ @rpm\/rpmtag.h:390:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SYSUSERS :: RpmTag
pattern RPMTAG_SYSUSERS = RpmTag 5109

{-| __C declaration:__ @RPMTAG_BUILDSYSTEM@

    __defined at:__ @rpm\/rpmtag.h:391:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BUILDSYSTEM :: RpmTag
pattern RPMTAG_BUILDSYSTEM = RpmTag 5110

{-| __C declaration:__ @RPMTAG_BUILDOPTION@

    __defined at:__ @rpm\/rpmtag.h:392:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_BUILDOPTION :: RpmTag
pattern RPMTAG_BUILDOPTION = RpmTag 5111

{-| __C declaration:__ @RPMTAG_PAYLOADSIZE@

    __defined at:__ @rpm\/rpmtag.h:393:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PAYLOADSIZE :: RpmTag
pattern RPMTAG_PAYLOADSIZE = RpmTag 5112

{-| __C declaration:__ @RPMTAG_PAYLOADSIZEALT@

    __defined at:__ @rpm\/rpmtag.h:394:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PAYLOADSIZEALT :: RpmTag
pattern RPMTAG_PAYLOADSIZEALT = RpmTag 5113

{-| __C declaration:__ @RPMTAG_RPMFORMAT@

    __defined at:__ @rpm\/rpmtag.h:395:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_RPMFORMAT :: RpmTag
pattern RPMTAG_RPMFORMAT = RpmTag 5114

{-| __C declaration:__ @RPMTAG_FILEMIMEINDEX@

    __defined at:__ @rpm\/rpmtag.h:396:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILEMIMEINDEX :: RpmTag
pattern RPMTAG_FILEMIMEINDEX = RpmTag 5115

{-| __C declaration:__ @RPMTAG_MIMEDICT@

    __defined at:__ @rpm\/rpmtag.h:397:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_MIMEDICT :: RpmTag
pattern RPMTAG_MIMEDICT = RpmTag 5116

{-| __C declaration:__ @RPMTAG_FILEMIMES@

    __defined at:__ @rpm\/rpmtag.h:398:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FILEMIMES :: RpmTag
pattern RPMTAG_FILEMIMES = RpmTag 5117

{-| __C declaration:__ @RPMTAG_PACKAGEDIGESTS@

    __defined at:__ @rpm\/rpmtag.h:399:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PACKAGEDIGESTS :: RpmTag
pattern RPMTAG_PACKAGEDIGESTS = RpmTag 5118

{-| __C declaration:__ @RPMTAG_PACKAGEDIGESTALGOS@

    __defined at:__ @rpm\/rpmtag.h:400:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PACKAGEDIGESTALGOS :: RpmTag
pattern RPMTAG_PACKAGEDIGESTALGOS = RpmTag 5119

{-| __C declaration:__ @RPMTAG_SOURCENEVR@

    __defined at:__ @rpm\/rpmtag.h:401:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_SOURCENEVR :: RpmTag
pattern RPMTAG_SOURCENEVR = RpmTag 5120

{-| __C declaration:__ @RPMTAG_PAYLOADSHA512@

    __defined at:__ @rpm\/rpmtag.h:402:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PAYLOADSHA512 :: RpmTag
pattern RPMTAG_PAYLOADSHA512 = RpmTag 5121

{-| __C declaration:__ @RPMTAG_PAYLOADSHA512ALT@

    __defined at:__ @rpm\/rpmtag.h:403:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PAYLOADSHA512ALT :: RpmTag
pattern RPMTAG_PAYLOADSHA512ALT = RpmTag 5122

{-| __C declaration:__ @RPMTAG_PAYLOADSHA3_256@

    __defined at:__ @rpm\/rpmtag.h:404:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PAYLOADSHA3_256 :: RpmTag
pattern RPMTAG_PAYLOADSHA3_256 = RpmTag 5123

{-| __C declaration:__ @RPMTAG_PAYLOADSHA3_256ALT@

    __defined at:__ @rpm\/rpmtag.h:405:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_PAYLOADSHA3_256ALT :: RpmTag
pattern RPMTAG_PAYLOADSHA3_256ALT = RpmTag 5124

{-| internal

__C declaration:__ @RPMTAG_FIRSTFREE_TAG@

__defined at:__ @rpm\/rpmtag.h:407:5@

__exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMTAG_FIRSTFREE_TAG :: RpmTag
pattern RPMTAG_FIRSTFREE_TAG = RpmTag 5125

{-| __C declaration:__ @RPMTAG_EXTERNAL_TAG@

    __defined at:__ @rpm\/rpmtag.h:411:9@

    __exported by:__ @rpm\/rpmtag.h@
-}
rPMTAG_EXTERNAL_TAG :: FC.CInt
rPMTAG_EXTERNAL_TAG = (1000000 :: FC.CInt)

{-|

  > rpmtag

  Rpm database index tags.

__C declaration:__ @rpmDbiTag@

__defined at:__ @rpm\/rpmtag.h:416:14@

__exported by:__ @rpm\/rpmtag.h@
-}
newtype RpmDbiTag = RpmDbiTag
  { un_RpmDbiTag :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable RpmDbiTag where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure RpmDbiTag
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          RpmDbiTag un_RpmDbiTag2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_RpmDbiTag2

instance HsBindgen.Runtime.CEnum.CEnum RpmDbiTag where

  type CEnumZ RpmDbiTag = FC.CUInt

  toCEnum = RpmDbiTag

  fromCEnum = un_RpmDbiTag

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "RPMDBI_PACKAGES")
                                                     , (2, Data.List.NonEmpty.singleton "RPMDBI_LABEL")
                                                     , (261, Data.List.NonEmpty.singleton "RPMDBI_SIGMD5")
                                                     , (269, Data.List.NonEmpty.singleton "RPMDBI_SHA1HEADER")
                                                     , (1000, Data.List.NonEmpty.singleton "RPMDBI_NAME")
                                                     , (1016, Data.List.NonEmpty.singleton "RPMDBI_GROUP")
                                                     , (1047, Data.List.NonEmpty.singleton "RPMDBI_PROVIDENAME")
                                                     , (1049, Data.List.NonEmpty.singleton "RPMDBI_REQUIRENAME")
                                                     , (1054, Data.List.NonEmpty.singleton "RPMDBI_CONFLICTNAME")
                                                     , (1066, Data.List.NonEmpty.singleton "RPMDBI_TRIGGERNAME")
                                                     , (1090, Data.List.NonEmpty.singleton "RPMDBI_OBSOLETENAME")
                                                     , (1117, Data.List.NonEmpty.singleton "RPMDBI_BASENAMES")
                                                     , (1118, Data.List.NonEmpty.singleton "RPMDBI_DIRNAMES")
                                                     , (1128, Data.List.NonEmpty.singleton "RPMDBI_INSTALLTID")
                                                     , (5040, Data.List.NonEmpty.singleton "RPMDBI_INSTFILENAMES")
                                                     , (5046, Data.List.NonEmpty.singleton "RPMDBI_RECOMMENDNAME")
                                                     , (5049, Data.List.NonEmpty.singleton "RPMDBI_SUGGESTNAME")
                                                     , (5052, Data.List.NonEmpty.singleton "RPMDBI_SUPPLEMENTNAME")
                                                     , (5055, Data.List.NonEmpty.singleton "RPMDBI_ENHANCENAME")
                                                     , (5069, Data.List.NonEmpty.singleton "RPMDBI_FILETRIGGERNAME")
                                                     , (5079, Data.List.NonEmpty.singleton "RPMDBI_TRANSFILETRIGGERNAME")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "RpmDbiTag"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "RpmDbiTag"

instance Show RpmDbiTag where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read RpmDbiTag where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @RPMDBI_PACKAGES@

    __defined at:__ @rpm\/rpmtag.h:417:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMDBI_PACKAGES :: RpmDbiTag
pattern RPMDBI_PACKAGES = RpmDbiTag 0

{-| __C declaration:__ @RPMDBI_LABEL@

    __defined at:__ @rpm\/rpmtag.h:418:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMDBI_LABEL :: RpmDbiTag
pattern RPMDBI_LABEL = RpmDbiTag 2

{-| __C declaration:__ @RPMDBI_NAME@

    __defined at:__ @rpm\/rpmtag.h:419:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMDBI_NAME :: RpmDbiTag
pattern RPMDBI_NAME = RpmDbiTag 1000

{-| __C declaration:__ @RPMDBI_BASENAMES@

    __defined at:__ @rpm\/rpmtag.h:420:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMDBI_BASENAMES :: RpmDbiTag
pattern RPMDBI_BASENAMES = RpmDbiTag 1117

{-| __C declaration:__ @RPMDBI_GROUP@

    __defined at:__ @rpm\/rpmtag.h:421:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMDBI_GROUP :: RpmDbiTag
pattern RPMDBI_GROUP = RpmDbiTag 1016

{-| __C declaration:__ @RPMDBI_REQUIRENAME@

    __defined at:__ @rpm\/rpmtag.h:422:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMDBI_REQUIRENAME :: RpmDbiTag
pattern RPMDBI_REQUIRENAME = RpmDbiTag 1049

{-| __C declaration:__ @RPMDBI_PROVIDENAME@

    __defined at:__ @rpm\/rpmtag.h:423:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMDBI_PROVIDENAME :: RpmDbiTag
pattern RPMDBI_PROVIDENAME = RpmDbiTag 1047

{-| __C declaration:__ @RPMDBI_CONFLICTNAME@

    __defined at:__ @rpm\/rpmtag.h:424:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMDBI_CONFLICTNAME :: RpmDbiTag
pattern RPMDBI_CONFLICTNAME = RpmDbiTag 1054

{-| __C declaration:__ @RPMDBI_OBSOLETENAME@

    __defined at:__ @rpm\/rpmtag.h:425:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMDBI_OBSOLETENAME :: RpmDbiTag
pattern RPMDBI_OBSOLETENAME = RpmDbiTag 1090

{-| __C declaration:__ @RPMDBI_TRIGGERNAME@

    __defined at:__ @rpm\/rpmtag.h:426:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMDBI_TRIGGERNAME :: RpmDbiTag
pattern RPMDBI_TRIGGERNAME = RpmDbiTag 1066

{-| __C declaration:__ @RPMDBI_DIRNAMES@

    __defined at:__ @rpm\/rpmtag.h:427:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMDBI_DIRNAMES :: RpmDbiTag
pattern RPMDBI_DIRNAMES = RpmDbiTag 1118

{-| __C declaration:__ @RPMDBI_INSTALLTID@

    __defined at:__ @rpm\/rpmtag.h:428:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMDBI_INSTALLTID :: RpmDbiTag
pattern RPMDBI_INSTALLTID = RpmDbiTag 1128

{-| __C declaration:__ @RPMDBI_SIGMD5@

    __defined at:__ @rpm\/rpmtag.h:429:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMDBI_SIGMD5 :: RpmDbiTag
pattern RPMDBI_SIGMD5 = RpmDbiTag 261

{-| __C declaration:__ @RPMDBI_SHA1HEADER@

    __defined at:__ @rpm\/rpmtag.h:430:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMDBI_SHA1HEADER :: RpmDbiTag
pattern RPMDBI_SHA1HEADER = RpmDbiTag 269

{-| __C declaration:__ @RPMDBI_INSTFILENAMES@

    __defined at:__ @rpm\/rpmtag.h:431:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMDBI_INSTFILENAMES :: RpmDbiTag
pattern RPMDBI_INSTFILENAMES = RpmDbiTag 5040

{-| __C declaration:__ @RPMDBI_FILETRIGGERNAME@

    __defined at:__ @rpm\/rpmtag.h:432:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMDBI_FILETRIGGERNAME :: RpmDbiTag
pattern RPMDBI_FILETRIGGERNAME = RpmDbiTag 5069

{-| __C declaration:__ @RPMDBI_TRANSFILETRIGGERNAME@

    __defined at:__ @rpm\/rpmtag.h:433:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMDBI_TRANSFILETRIGGERNAME :: RpmDbiTag
pattern RPMDBI_TRANSFILETRIGGERNAME = RpmDbiTag 5079

{-| __C declaration:__ @RPMDBI_RECOMMENDNAME@

    __defined at:__ @rpm\/rpmtag.h:434:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMDBI_RECOMMENDNAME :: RpmDbiTag
pattern RPMDBI_RECOMMENDNAME = RpmDbiTag 5046

{-| __C declaration:__ @RPMDBI_SUGGESTNAME@

    __defined at:__ @rpm\/rpmtag.h:435:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMDBI_SUGGESTNAME :: RpmDbiTag
pattern RPMDBI_SUGGESTNAME = RpmDbiTag 5049

{-| __C declaration:__ @RPMDBI_SUPPLEMENTNAME@

    __defined at:__ @rpm\/rpmtag.h:436:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMDBI_SUPPLEMENTNAME :: RpmDbiTag
pattern RPMDBI_SUPPLEMENTNAME = RpmDbiTag 5052

{-| __C declaration:__ @RPMDBI_ENHANCENAME@

    __defined at:__ @rpm\/rpmtag.h:437:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMDBI_ENHANCENAME :: RpmDbiTag
pattern RPMDBI_ENHANCENAME = RpmDbiTag 5055

{-|

  > signature

  Tags found in signature header from package.

__C declaration:__ @rpmSigTag@

__defined at:__ @rpm\/rpmtag.h:443:14@

__exported by:__ @rpm\/rpmtag.h@
-}
newtype RpmSigTag = RpmSigTag
  { un_RpmSigTag :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable RpmSigTag where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure RpmSigTag
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          RpmSigTag un_RpmSigTag2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_RpmSigTag2

instance HsBindgen.Runtime.CEnum.CEnum RpmSigTag where

  type CEnumZ RpmSigTag = FC.CUInt

  toCEnum = RpmSigTag

  fromCEnum = un_RpmSigTag

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (264, Data.List.NonEmpty.singleton "RPMSIGTAG_BADSHA1_1")
                                                     , (265, Data.List.NonEmpty.singleton "RPMSIGTAG_BADSHA1_2")
                                                     , (267, Data.List.NonEmpty.singleton "RPMSIGTAG_DSA")
                                                     , (268, Data.List.NonEmpty.singleton "RPMSIGTAG_RSA")
                                                     , (269, Data.List.NonEmpty.singleton "RPMSIGTAG_SHA1")
                                                     , (270, Data.List.NonEmpty.singleton "RPMSIGTAG_LONGSIZE")
                                                     , (271, Data.List.NonEmpty.singleton "RPMSIGTAG_LONGARCHIVESIZE")
                                                     , (273, Data.List.NonEmpty.singleton "RPMSIGTAG_SHA256")
                                                     , (274, Data.List.NonEmpty.singleton "RPMSIGTAG_FILESIGNATURES")
                                                     , (275, Data.List.NonEmpty.singleton "RPMSIGTAG_FILESIGNATURELENGTH")
                                                     , (276, Data.List.NonEmpty.singleton "RPMSIGTAG_VERITYSIGNATURES")
                                                     , (277, Data.List.NonEmpty.singleton "RPMSIGTAG_VERITYSIGNATUREALGO")
                                                     , (278, Data.List.NonEmpty.singleton "RPMSIGTAG_OPENPGP")
                                                     , (279, Data.List.NonEmpty.singleton "RPMSIGTAG_SHA3_256")
                                                     , (999, Data.List.NonEmpty.singleton "RPMSIGTAG_RESERVED")
                                                     , (1000, Data.List.NonEmpty.singleton "RPMSIGTAG_SIZE")
                                                     , (1001, Data.List.NonEmpty.singleton "RPMSIGTAG_LEMD5_1")
                                                     , (1002, Data.List.NonEmpty.singleton "RPMSIGTAG_PGP")
                                                     , (1003, Data.List.NonEmpty.singleton "RPMSIGTAG_LEMD5_2")
                                                     , (1004, Data.List.NonEmpty.singleton "RPMSIGTAG_MD5")
                                                     , (1005, Data.List.NonEmpty.singleton "RPMSIGTAG_GPG")
                                                     , (1006, Data.List.NonEmpty.singleton "RPMSIGTAG_PGP5")
                                                     , (1007, Data.List.NonEmpty.singleton "RPMSIGTAG_PAYLOADSIZE")
                                                     , (1008, Data.List.NonEmpty.singleton "RPMSIGTAG_RESERVEDSPACE")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "RpmSigTag"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "RpmSigTag"

instance Show RpmSigTag where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read RpmSigTag where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| internal Header+Payload size (32bit) in bytes.

__C declaration:__ @RPMSIGTAG_SIZE@

__defined at:__ @rpm\/rpmtag.h:444:5@

__exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_SIZE :: RpmSigTag
pattern RPMSIGTAG_SIZE = RpmSigTag 1000

{-| internal Broken MD5, take 1

  __deprecated:__ legacy.

__C declaration:__ @RPMSIGTAG_LEMD5_1@

__defined at:__ @rpm\/rpmtag.h:445:5@

__exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_LEMD5_1 :: RpmSigTag
pattern RPMSIGTAG_LEMD5_1 = RpmSigTag 1001

{-| internal PGP 2.6.3 signature.

__C declaration:__ @RPMSIGTAG_PGP@

__defined at:__ @rpm\/rpmtag.h:446:5@

__exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_PGP :: RpmSigTag
pattern RPMSIGTAG_PGP = RpmSigTag 1002

{-| internal Broken MD5, take 2

  __deprecated:__ legacy.

__C declaration:__ @RPMSIGTAG_LEMD5_2@

__defined at:__ @rpm\/rpmtag.h:447:5@

__exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_LEMD5_2 :: RpmSigTag
pattern RPMSIGTAG_LEMD5_2 = RpmSigTag 1003

{-| internal MD5 signature.

__C declaration:__ @RPMSIGTAG_MD5@

__defined at:__ @rpm\/rpmtag.h:448:5@

__exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_MD5 :: RpmSigTag
pattern RPMSIGTAG_MD5 = RpmSigTag 1004

{-| internal GnuPG signature.

__C declaration:__ @RPMSIGTAG_GPG@

__defined at:__ @rpm\/rpmtag.h:449:5@

__exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_GPG :: RpmSigTag
pattern RPMSIGTAG_GPG = RpmSigTag 1005

{-| internal PGP5 signature

  __deprecated:__ legacy.

__C declaration:__ @RPMSIGTAG_PGP5@

__defined at:__ @rpm\/rpmtag.h:450:5@

__exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_PGP5 :: RpmSigTag
pattern RPMSIGTAG_PGP5 = RpmSigTag 1006

{-| internal uncompressed payload size (32bit) in bytes.

__C declaration:__ @RPMSIGTAG_PAYLOADSIZE@

__defined at:__ @rpm\/rpmtag.h:451:5@

__exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_PAYLOADSIZE :: RpmSigTag
pattern RPMSIGTAG_PAYLOADSIZE = RpmSigTag 1007

{-| internal space reserved for signatures

__C declaration:__ @RPMSIGTAG_RESERVEDSPACE@

__defined at:__ @rpm\/rpmtag.h:452:5@

__exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_RESERVEDSPACE :: RpmSigTag
pattern RPMSIGTAG_RESERVEDSPACE = RpmSigTag 1008

{-| internal Broken SHA1, take 1.

__C declaration:__ @RPMSIGTAG_BADSHA1_1@

__defined at:__ @rpm\/rpmtag.h:453:5@

__exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_BADSHA1_1 :: RpmSigTag
pattern RPMSIGTAG_BADSHA1_1 = RpmSigTag 264

{-| internal Broken SHA1, take 2.

__C declaration:__ @RPMSIGTAG_BADSHA1_2@

__defined at:__ @rpm\/rpmtag.h:454:5@

__exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_BADSHA1_2 :: RpmSigTag
pattern RPMSIGTAG_BADSHA1_2 = RpmSigTag 265

{-| internal DSA header signature.

__C declaration:__ @RPMSIGTAG_DSA@

__defined at:__ @rpm\/rpmtag.h:455:5@

__exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_DSA :: RpmSigTag
pattern RPMSIGTAG_DSA = RpmSigTag 267

{-| internal RSA header signature.

__C declaration:__ @RPMSIGTAG_RSA@

__defined at:__ @rpm\/rpmtag.h:456:5@

__exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_RSA :: RpmSigTag
pattern RPMSIGTAG_RSA = RpmSigTag 268

{-| internal sha1 header digest.

__C declaration:__ @RPMSIGTAG_SHA1@

__defined at:__ @rpm\/rpmtag.h:457:5@

__exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_SHA1 :: RpmSigTag
pattern RPMSIGTAG_SHA1 = RpmSigTag 269

{-| internal Header+Payload size (64bit) in bytes.

__C declaration:__ @RPMSIGTAG_LONGSIZE@

__defined at:__ @rpm\/rpmtag.h:458:5@

__exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_LONGSIZE :: RpmSigTag
pattern RPMSIGTAG_LONGSIZE = RpmSigTag 270

{-| internal uncompressed payload size (64bit) in bytes.

__C declaration:__ @RPMSIGTAG_LONGARCHIVESIZE@

__defined at:__ @rpm\/rpmtag.h:459:5@

__exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_LONGARCHIVESIZE :: RpmSigTag
pattern RPMSIGTAG_LONGARCHIVESIZE = RpmSigTag 271

{-| __C declaration:__ @RPMSIGTAG_SHA256@

    __defined at:__ @rpm\/rpmtag.h:460:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_SHA256 :: RpmSigTag
pattern RPMSIGTAG_SHA256 = RpmSigTag 273

{-| __C declaration:__ @RPMSIGTAG_FILESIGNATURES@

    __defined at:__ @rpm\/rpmtag.h:461:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_FILESIGNATURES :: RpmSigTag
pattern RPMSIGTAG_FILESIGNATURES = RpmSigTag 274

{-| __C declaration:__ @RPMSIGTAG_FILESIGNATURELENGTH@

    __defined at:__ @rpm\/rpmtag.h:462:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_FILESIGNATURELENGTH :: RpmSigTag
pattern RPMSIGTAG_FILESIGNATURELENGTH = RpmSigTag 275

{-| __C declaration:__ @RPMSIGTAG_VERITYSIGNATURES@

    __defined at:__ @rpm\/rpmtag.h:463:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_VERITYSIGNATURES :: RpmSigTag
pattern RPMSIGTAG_VERITYSIGNATURES = RpmSigTag 276

{-| __C declaration:__ @RPMSIGTAG_VERITYSIGNATUREALGO@

    __defined at:__ @rpm\/rpmtag.h:464:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_VERITYSIGNATUREALGO :: RpmSigTag
pattern RPMSIGTAG_VERITYSIGNATUREALGO = RpmSigTag 277

{-| __C declaration:__ @RPMSIGTAG_OPENPGP@

    __defined at:__ @rpm\/rpmtag.h:465:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_OPENPGP :: RpmSigTag
pattern RPMSIGTAG_OPENPGP = RpmSigTag 278

{-| __C declaration:__ @RPMSIGTAG_SHA3_256@

    __defined at:__ @rpm\/rpmtag.h:466:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_SHA3_256 :: RpmSigTag
pattern RPMSIGTAG_SHA3_256 = RpmSigTag 279

{-| __C declaration:__ @RPMSIGTAG_RESERVED@

    __defined at:__ @rpm\/rpmtag.h:467:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPMSIGTAG_RESERVED :: RpmSigTag
pattern RPMSIGTAG_RESERVED = RpmSigTag 999

{-|

  > header

  The basic types of data in tags from headers.

__C declaration:__ @rpmTagType_e@

__defined at:__ @rpm\/rpmtag.h:474:6@

__exported by:__ @rpm\/rpmtag.h@
-}
newtype RpmTagType_e = RpmTagType_e
  { un_RpmTagType_e :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable RpmTagType_e where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure RpmTagType_e
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          RpmTagType_e un_RpmTagType_e2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_RpmTagType_e2

instance HsBindgen.Runtime.CEnum.CEnum RpmTagType_e where

  type CEnumZ RpmTagType_e = FC.CUInt

  toCEnum = RpmTagType_e

  fromCEnum = un_RpmTagType_e

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "RPM_NULL_TYPE")
                                                     , (1, Data.List.NonEmpty.singleton "RPM_CHAR_TYPE")
                                                     , (2, Data.List.NonEmpty.singleton "RPM_INT8_TYPE")
                                                     , (3, Data.List.NonEmpty.singleton "RPM_INT16_TYPE")
                                                     , (4, Data.List.NonEmpty.singleton "RPM_INT32_TYPE")
                                                     , (5, Data.List.NonEmpty.singleton "RPM_INT64_TYPE")
                                                     , (6, Data.List.NonEmpty.singleton "RPM_STRING_TYPE")
                                                     , (7, Data.List.NonEmpty.singleton "RPM_BIN_TYPE")
                                                     , (8, Data.List.NonEmpty.singleton "RPM_STRING_ARRAY_TYPE")
                                                     , (9, Data.List.NonEmpty.singleton "RPM_I18NSTRING_TYPE")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "RpmTagType_e"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "RpmTagType_e"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum RpmTagType_e where

  minDeclaredValue = RPM_NULL_TYPE

  maxDeclaredValue = RPM_I18NSTRING_TYPE

instance Show RpmTagType_e where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read RpmTagType_e where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @RPM_NULL_TYPE@

    __defined at:__ @rpm\/rpmtag.h:476:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPM_NULL_TYPE :: RpmTagType_e
pattern RPM_NULL_TYPE = RpmTagType_e 0

{-| __C declaration:__ @RPM_CHAR_TYPE@

    __defined at:__ @rpm\/rpmtag.h:477:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPM_CHAR_TYPE :: RpmTagType_e
pattern RPM_CHAR_TYPE = RpmTagType_e 1

{-| __C declaration:__ @RPM_INT8_TYPE@

    __defined at:__ @rpm\/rpmtag.h:478:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPM_INT8_TYPE :: RpmTagType_e
pattern RPM_INT8_TYPE = RpmTagType_e 2

{-| __C declaration:__ @RPM_INT16_TYPE@

    __defined at:__ @rpm\/rpmtag.h:479:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPM_INT16_TYPE :: RpmTagType_e
pattern RPM_INT16_TYPE = RpmTagType_e 3

{-| __C declaration:__ @RPM_INT32_TYPE@

    __defined at:__ @rpm\/rpmtag.h:480:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPM_INT32_TYPE :: RpmTagType_e
pattern RPM_INT32_TYPE = RpmTagType_e 4

{-| __C declaration:__ @RPM_INT64_TYPE@

    __defined at:__ @rpm\/rpmtag.h:481:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPM_INT64_TYPE :: RpmTagType_e
pattern RPM_INT64_TYPE = RpmTagType_e 5

{-| __C declaration:__ @RPM_STRING_TYPE@

    __defined at:__ @rpm\/rpmtag.h:482:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPM_STRING_TYPE :: RpmTagType_e
pattern RPM_STRING_TYPE = RpmTagType_e 6

{-| __C declaration:__ @RPM_BIN_TYPE@

    __defined at:__ @rpm\/rpmtag.h:483:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPM_BIN_TYPE :: RpmTagType_e
pattern RPM_BIN_TYPE = RpmTagType_e 7

{-| __C declaration:__ @RPM_STRING_ARRAY_TYPE@

    __defined at:__ @rpm\/rpmtag.h:484:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPM_STRING_ARRAY_TYPE :: RpmTagType_e
pattern RPM_STRING_ARRAY_TYPE = RpmTagType_e 8

{-| __C declaration:__ @RPM_I18NSTRING_TYPE@

    __defined at:__ @rpm\/rpmtag.h:485:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPM_I18NSTRING_TYPE :: RpmTagType_e
pattern RPM_I18NSTRING_TYPE = RpmTagType_e 9

{-| __C declaration:__ @RPM_MIN_TYPE@

    __defined at:__ @rpm\/rpmtag.h:475:9@

    __exported by:__ @rpm\/rpmtag.h@
-}
rPM_MIN_TYPE :: FC.CInt
rPM_MIN_TYPE = (1 :: FC.CInt)

{-| __C declaration:__ @RPM_MAX_TYPE@

    __defined at:__ @rpm\/rpmtag.h:486:9@

    __exported by:__ @rpm\/rpmtag.h@
-}
rPM_MAX_TYPE :: FC.CInt
rPM_MAX_TYPE = (9 :: FC.CInt)

{-| __C declaration:__ @RPM_FORCEFREE_TYPE@

    __defined at:__ @rpm\/rpmtag.h:487:9@

    __exported by:__ @rpm\/rpmtag.h@
-}
rPM_FORCEFREE_TYPE :: FC.CInt
rPM_FORCEFREE_TYPE = (255 :: FC.CInt)

{-| __C declaration:__ @RPM_MASK_TYPE@

    __defined at:__ @rpm\/rpmtag.h:488:9@

    __exported by:__ @rpm\/rpmtag.h@
-}
rPM_MASK_TYPE :: FC.CInt
rPM_MASK_TYPE = (65535 :: FC.CInt)

{-| __C declaration:__ @rpmTagType@

    __defined at:__ @rpm\/rpmtag.h:490:18@

    __exported by:__ @rpm\/rpmtag.h@
-}
newtype RpmTagType = RpmTagType
  { un_RpmTagType :: RPM.Types.RpmFlags
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-|

  > rpmtag

  The classes of data in tags from headers.

__C declaration:__ @rpmTagClass@

__defined at:__ @rpm\/rpmtag.h:495:14@

__exported by:__ @rpm\/rpmtag.h@
-}
newtype RpmTagClass = RpmTagClass
  { un_RpmTagClass :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable RpmTagClass where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure RpmTagClass
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          RpmTagClass un_RpmTagClass2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_RpmTagClass2

instance HsBindgen.Runtime.CEnum.CEnum RpmTagClass where

  type CEnumZ RpmTagClass = FC.CUInt

  toCEnum = RpmTagClass

  fromCEnum = un_RpmTagClass

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "RPM_NULL_CLASS")
                                                     , (1, Data.List.NonEmpty.singleton "RPM_NUMERIC_CLASS")
                                                     , (2, Data.List.NonEmpty.singleton "RPM_STRING_CLASS")
                                                     , (3, Data.List.NonEmpty.singleton "RPM_BINARY_CLASS")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "RpmTagClass"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "RpmTagClass"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum RpmTagClass where

  minDeclaredValue = RPM_NULL_CLASS

  maxDeclaredValue = RPM_BINARY_CLASS

instance Show RpmTagClass where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read RpmTagClass where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @RPM_NULL_CLASS@

    __defined at:__ @rpm\/rpmtag.h:496:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPM_NULL_CLASS :: RpmTagClass
pattern RPM_NULL_CLASS = RpmTagClass 0

{-| __C declaration:__ @RPM_NUMERIC_CLASS@

    __defined at:__ @rpm\/rpmtag.h:497:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPM_NUMERIC_CLASS :: RpmTagClass
pattern RPM_NUMERIC_CLASS = RpmTagClass 1

{-| __C declaration:__ @RPM_STRING_CLASS@

    __defined at:__ @rpm\/rpmtag.h:498:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPM_STRING_CLASS :: RpmTagClass
pattern RPM_STRING_CLASS = RpmTagClass 2

{-| __C declaration:__ @RPM_BINARY_CLASS@

    __defined at:__ @rpm\/rpmtag.h:499:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPM_BINARY_CLASS :: RpmTagClass
pattern RPM_BINARY_CLASS = RpmTagClass 3

{-|

  > header

  * Identify how to return the header data type.

__C declaration:__ @rpmTagReturnType_e@

__defined at:__ @rpm\/rpmtag.h:505:6@

__exported by:__ @rpm\/rpmtag.h@
-}
newtype RpmTagReturnType_e = RpmTagReturnType_e
  { un_RpmTagReturnType_e :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable RpmTagReturnType_e where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure RpmTagReturnType_e
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          RpmTagReturnType_e un_RpmTagReturnType_e2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_RpmTagReturnType_e2

instance HsBindgen.Runtime.CEnum.CEnum RpmTagReturnType_e where

  type CEnumZ RpmTagReturnType_e = FC.CUInt

  toCEnum = RpmTagReturnType_e

  fromCEnum = un_RpmTagReturnType_e

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "RPM_ANY_RETURN_TYPE")
                                                     , (65536, Data.List.NonEmpty.singleton "RPM_SCALAR_RETURN_TYPE")
                                                     , (131072, Data.List.NonEmpty.singleton "RPM_ARRAY_RETURN_TYPE")
                                                     , (262144, Data.List.NonEmpty.singleton "RPM_MAPPING_RETURN_TYPE")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "RpmTagReturnType_e"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "RpmTagReturnType_e"

instance Show RpmTagReturnType_e where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read RpmTagReturnType_e where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @RPM_ANY_RETURN_TYPE@

    __defined at:__ @rpm\/rpmtag.h:506:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPM_ANY_RETURN_TYPE :: RpmTagReturnType_e
pattern RPM_ANY_RETURN_TYPE = RpmTagReturnType_e 0

{-| __C declaration:__ @RPM_SCALAR_RETURN_TYPE@

    __defined at:__ @rpm\/rpmtag.h:507:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPM_SCALAR_RETURN_TYPE :: RpmTagReturnType_e
pattern RPM_SCALAR_RETURN_TYPE = RpmTagReturnType_e 65536

{-| __C declaration:__ @RPM_ARRAY_RETURN_TYPE@

    __defined at:__ @rpm\/rpmtag.h:508:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPM_ARRAY_RETURN_TYPE :: RpmTagReturnType_e
pattern RPM_ARRAY_RETURN_TYPE = RpmTagReturnType_e 131072

{-| __C declaration:__ @RPM_MAPPING_RETURN_TYPE@

    __defined at:__ @rpm\/rpmtag.h:509:5@

    __exported by:__ @rpm\/rpmtag.h@
-}
pattern RPM_MAPPING_RETURN_TYPE :: RpmTagReturnType_e
pattern RPM_MAPPING_RETURN_TYPE = RpmTagReturnType_e 262144

{-| __C declaration:__ @RPM_MASK_RETURN_TYPE@

    __defined at:__ @rpm\/rpmtag.h:510:9@

    __exported by:__ @rpm\/rpmtag.h@
-}
rPM_MASK_RETURN_TYPE :: FC.CInt
rPM_MASK_RETURN_TYPE = (4294901760 :: FC.CInt)

{-| __C declaration:__ @rpmTagReturnType@

    __defined at:__ @rpm\/rpmtag.h:513:18@

    __exported by:__ @rpm\/rpmtag.h@
-}
newtype RpmTagReturnType = RpmTagReturnType
  { un_RpmTagReturnType :: RPM.Types.RpmFlags
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)
