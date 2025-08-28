{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Generated.Pcap where

-- import C.Expr.HostPlatform ((.&.), (.|.), (<<), (>>))
import C.Expr.HostPlatform ((.&.), (.|.), (<<))
import C.Expr.HostPlatform qualified as C
import Data.Bits (FiniteBits)
import Data.Bits qualified as Bits
import Data.Ix qualified as Ix
import Data.List.NonEmpty qualified
import Data.Void (Void)
import Foreign qualified as F
import Foreign.C qualified as FC
import HsBindgen.Runtime.CAPI qualified as CAPI
import HsBindgen.Runtime.CEnum qualified
import HsBindgen.Runtime.ConstantArray qualified
import HsBindgen.Runtime.Prelude qualified
import Prelude (Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real,
                Show, pure, showsPrec, (<*>), (>>))
import Text.Read qualified

$(CAPI.addCSource "#include <pcap.h>\nsigned int hs_bindgen_8e690804091c04ec (unsigned int arg1, char *arg2) { return pcap_init(arg1, arg2); }\nchar *hs_bindgen_46bc906cad541290 (char *arg1) { return pcap_lookupdev(arg1); }\nsigned int hs_bindgen_b19f8a50c4c1eed4 (char *arg1, bpf_u_int32 *arg2, bpf_u_int32 *arg3, char *arg4) { return pcap_lookupnet(arg1, arg2, arg3, arg4); }\npcap_t *hs_bindgen_7b7834bca252cee9 (char *arg1, char *arg2) { return pcap_create(arg1, arg2); }\nsigned int hs_bindgen_8553475d6a4d4fc1 (pcap_t *arg1, signed int arg2) { return pcap_set_snaplen(arg1, arg2); }\nsigned int hs_bindgen_e20ccd92ea74bdd8 (pcap_t *arg1, signed int arg2) { return pcap_set_promisc(arg1, arg2); }\nsigned int hs_bindgen_cc2851957e1781b8 (pcap_t *arg1) { return pcap_can_set_rfmon(arg1); }\nsigned int hs_bindgen_b840e978c312fc4b (pcap_t *arg1, signed int arg2) { return pcap_set_rfmon(arg1, arg2); }\nsigned int hs_bindgen_0b63cda6c932ebd4 (pcap_t *arg1, signed int arg2) { return pcap_set_timeout(arg1, arg2); }\nsigned int hs_bindgen_4aee540f1876beed (pcap_t *arg1, signed int arg2) { return pcap_set_tstamp_type(arg1, arg2); }\nsigned int hs_bindgen_6198eda39caaee0b (pcap_t *arg1, signed int arg2) { return pcap_set_immediate_mode(arg1, arg2); }\nsigned int hs_bindgen_43e52f65308dc82e (pcap_t *arg1, signed int arg2) { return pcap_set_buffer_size(arg1, arg2); }\nsigned int hs_bindgen_bcb7b2d106e0cd52 (pcap_t *arg1, signed int arg2) { return pcap_set_tstamp_precision(arg1, arg2); }\nsigned int hs_bindgen_79d362811d578175 (pcap_t *arg1) { return pcap_get_tstamp_precision(arg1); }\nsigned int hs_bindgen_54ea2d199ac49ebc (pcap_t *arg1) { return pcap_activate(arg1); }\nsigned int hs_bindgen_43af2237985682b1 (pcap_t *arg1, signed int **arg2) { return pcap_list_tstamp_types(arg1, arg2); }\nvoid hs_bindgen_3c888668e9412a05 (signed int *arg1) { pcap_free_tstamp_types(arg1); }\nsigned int hs_bindgen_c8eb219382cf1b46 (char *arg1) { return pcap_tstamp_type_name_to_val(arg1); }\nchar *hs_bindgen_d4a57d99e333b885 (signed int arg1) { return pcap_tstamp_type_val_to_name(arg1); }\nchar *hs_bindgen_9734f5e64e4f9bdc (signed int arg1) { return pcap_tstamp_type_val_to_description(arg1); }\nsigned int hs_bindgen_23886cba4b92f2a2 (pcap_t *arg1, signed int arg2) { return pcap_set_protocol_linux(arg1, arg2); }\npcap_t *hs_bindgen_b7fb79264f562481 (char *arg1, signed int arg2, signed int arg3, signed int arg4, char *arg5) { return pcap_open_live(arg1, arg2, arg3, arg4, arg5); }\npcap_t *hs_bindgen_76136638a41c6eeb (signed int arg1, signed int arg2) { return pcap_open_dead(arg1, arg2); }\npcap_t *hs_bindgen_29a88236a4ecde00 (signed int arg1, signed int arg2, u_int arg3) { return pcap_open_dead_with_tstamp_precision(arg1, arg2, arg3); }\npcap_t *hs_bindgen_16814752302d61ee (char *arg1, u_int arg2, char *arg3) { return pcap_open_offline_with_tstamp_precision(arg1, arg2, arg3); }\npcap_t *hs_bindgen_eb04b53463f10143 (char *arg1, char *arg2) { return pcap_open_offline(arg1, arg2); }\npcap_t *hs_bindgen_ff917f556333c221 (FILE *arg1, u_int arg2, char *arg3) { return pcap_fopen_offline_with_tstamp_precision(arg1, arg2, arg3); }\npcap_t *hs_bindgen_94640023a569342c (FILE *arg1, char *arg2) { return pcap_fopen_offline(arg1, arg2); }\nvoid hs_bindgen_399eb2733162cc22 (pcap_t *arg1) { pcap_close(arg1); }\nsigned int hs_bindgen_604fcacd649d777d (pcap_t *arg1, signed int arg2, pcap_handler arg3, u_char *arg4) { return pcap_loop(arg1, arg2, arg3, arg4); }\nsigned int hs_bindgen_c74069ed8f91b583 (pcap_t *arg1, signed int arg2, pcap_handler arg3, u_char *arg4) { return pcap_dispatch(arg1, arg2, arg3, arg4); }\nu_char *hs_bindgen_33a8bc05c5093742 (pcap_t *arg1, struct pcap_pkthdr *arg2) { return pcap_next(arg1, arg2); }\nsigned int hs_bindgen_9b4a988026a25a55 (pcap_t *arg1, struct pcap_pkthdr **arg2, u_char **arg3) { return pcap_next_ex(arg1, arg2, arg3); }\nvoid hs_bindgen_8f560852ea6217b6 (pcap_t *arg1) { pcap_breakloop(arg1); }\nsigned int hs_bindgen_b3debcdebc5dcc97 (pcap_t *arg1, struct pcap_stat *arg2) { return pcap_stats(arg1, arg2); }\nsigned int hs_bindgen_64b03a9edc688b6b (pcap_t *arg1, struct bpf_program *arg2) { return pcap_setfilter(arg1, arg2); }\nsigned int hs_bindgen_953baf14d7ce632e (pcap_t *arg1, pcap_direction_t arg2) { return pcap_setdirection(arg1, arg2); }\nsigned int hs_bindgen_5bd8498726f4d5d8 (pcap_t *arg1, char *arg2) { return pcap_getnonblock(arg1, arg2); }\nsigned int hs_bindgen_687c80d39a8640a8 (pcap_t *arg1, signed int arg2, char *arg3) { return pcap_setnonblock(arg1, arg2, arg3); }\nsigned int hs_bindgen_2b120405a2329bd9 (pcap_t *arg1, void *arg2, size_t arg3) { return pcap_inject(arg1, arg2, arg3); }\nsigned int hs_bindgen_27b6362138d9d7f5 (pcap_t *arg1, u_char *arg2, signed int arg3) { return pcap_sendpacket(arg1, arg2, arg3); }\nchar *hs_bindgen_edb007bfd4fcc3a6 (signed int arg1) { return pcap_statustostr(arg1); }\nchar *hs_bindgen_eeef2be579076234 (signed int arg1) { return pcap_strerror(arg1); }\nchar *hs_bindgen_59d6290683549d60 (pcap_t *arg1) { return pcap_geterr(arg1); }\nvoid hs_bindgen_9c9c62590faa0950 (pcap_t *arg1, char *arg2) { pcap_perror(arg1, arg2); }\nsigned int hs_bindgen_24112b18ae5550c9 (pcap_t *arg1, struct bpf_program *arg2, char *arg3, signed int arg4, bpf_u_int32 arg5) { return pcap_compile(arg1, arg2, arg3, arg4, arg5); }\nsigned int hs_bindgen_cc5b08bebf5cae34 (signed int arg1, signed int arg2, struct bpf_program *arg3, char *arg4, signed int arg5, bpf_u_int32 arg6) { return pcap_compile_nopcap(arg1, arg2, arg3, arg4, arg5, arg6); }\nvoid hs_bindgen_009c484b78096901 (struct bpf_program *arg1) { pcap_freecode(arg1); }\nsigned int hs_bindgen_ca4bf9fbce12f46d (struct bpf_program *arg1, struct pcap_pkthdr *arg2, u_char *arg3) { return pcap_offline_filter(arg1, arg2, arg3); }\nsigned int hs_bindgen_6310f22b400cfbea (pcap_t *arg1) { return pcap_datalink(arg1); }\nsigned int hs_bindgen_8d263b0b0ccb525f (pcap_t *arg1) { return pcap_datalink_ext(arg1); }\nsigned int hs_bindgen_7a812b86d4676fa1 (pcap_t *arg1, signed int **arg2) { return pcap_list_datalinks(arg1, arg2); }\nsigned int hs_bindgen_3d961067e2512ee8 (pcap_t *arg1, signed int arg2) { return pcap_set_datalink(arg1, arg2); }\nvoid hs_bindgen_2e77a3d02d2e4969 (signed int *arg1) { pcap_free_datalinks(arg1); }\nsigned int hs_bindgen_e26516a9409e18d2 (char *arg1) { return pcap_datalink_name_to_val(arg1); }\nchar *hs_bindgen_da0d5e544e7ffe16 (signed int arg1) { return pcap_datalink_val_to_name(arg1); }\nchar *hs_bindgen_ec025504038351d3 (signed int arg1) { return pcap_datalink_val_to_description(arg1); }\nchar *hs_bindgen_7018c9f6223919b0 (signed int arg1) { return pcap_datalink_val_to_description_or_dlt(arg1); }\nsigned int hs_bindgen_29c299a09deb8827 (pcap_t *arg1) { return pcap_snapshot(arg1); }\nsigned int hs_bindgen_d7d4aeb73315e298 (pcap_t *arg1) { return pcap_is_swapped(arg1); }\nsigned int hs_bindgen_957c445dfb0617cb (pcap_t *arg1) { return pcap_major_version(arg1); }\nsigned int hs_bindgen_f899c77267bc823b (pcap_t *arg1) { return pcap_minor_version(arg1); }\nsigned int hs_bindgen_8a687e5c2618f8ce (pcap_t *arg1) { return pcap_bufsize(arg1); }\nFILE *hs_bindgen_465f70ee9f01bd6f (pcap_t *arg1) { return pcap_file(arg1); }\nsigned int hs_bindgen_72bc897a01fd5c11 (pcap_t *arg1) { return pcap_fileno(arg1); }\npcap_dumper_t *hs_bindgen_82030f5f2650d881 (pcap_t *arg1, char *arg2) { return pcap_dump_open(arg1, arg2); }\npcap_dumper_t *hs_bindgen_35a8ef64632d544c (pcap_t *arg1, FILE *arg2) { return pcap_dump_fopen(arg1, arg2); }\npcap_dumper_t *hs_bindgen_da36fd9375a43c56 (pcap_t *arg1, char *arg2) { return pcap_dump_open_append(arg1, arg2); }\nFILE *hs_bindgen_32f2969fc086c0bf (pcap_dumper_t *arg1) { return pcap_dump_file(arg1); }\nsigned long hs_bindgen_cc64b0e3af70fdb5 (pcap_dumper_t *arg1) { return pcap_dump_ftell(arg1); }\nint64_t hs_bindgen_6e260398dc6bcf99 (pcap_dumper_t *arg1) { return pcap_dump_ftell64(arg1); }\nsigned int hs_bindgen_c04f066c730c44ba (pcap_dumper_t *arg1) { return pcap_dump_flush(arg1); }\nvoid hs_bindgen_d20794d8ba8cb77f (pcap_dumper_t *arg1) { pcap_dump_close(arg1); }\nvoid hs_bindgen_e03b7a33ac92dda3 (u_char *arg1, struct pcap_pkthdr *arg2, u_char *arg3) { pcap_dump(arg1, arg2, arg3); }\nsigned int hs_bindgen_cf38ad940dc3db2c (pcap_if_t **arg1, char *arg2) { return pcap_findalldevs(arg1, arg2); }\nvoid hs_bindgen_6ed42d89cc05b063 (pcap_if_t *arg1) { pcap_freealldevs(arg1); }\nchar *hs_bindgen_5d098d3a885d036a (void) { return pcap_lib_version(); }\nsigned int hs_bindgen_db4f32072d198ad8 (pcap_t *arg1) { return pcap_get_selectable_fd(arg1); }\nstruct timeval *hs_bindgen_9e521ac37c1483c8 (pcap_t *arg1) { return pcap_get_required_select_timeout(arg1); }\npcap_t *hs_bindgen_c2698a21a32b9dff (char *arg1, signed int arg2, signed int arg3, signed int arg4, struct pcap_rmtauth *arg5, char *arg6) { return pcap_open(arg1, arg2, arg3, arg4, arg5, arg6); }\nsigned int hs_bindgen_8df98cacb3acc9dd (char *arg1, signed int arg2, char *arg3, char *arg4, char *arg5, char *arg6) { return pcap_createsrcstr(arg1, arg2, arg3, arg4, arg5, arg6); }\nsigned int hs_bindgen_7f18ff5dfbf6569c (char *arg1, signed int *arg2, char *arg3, char *arg4, char *arg5, char *arg6) { return pcap_parsesrcstr(arg1, arg2, arg3, arg4, arg5, arg6); }\nsigned int hs_bindgen_a8ed87fc59e6b669 (char *arg1, struct pcap_rmtauth *arg2, pcap_if_t **arg3, char *arg4) { return pcap_findalldevs_ex(arg1, arg2, arg3, arg4); }\nstruct pcap_samp *hs_bindgen_842a3be6d3b00873 (pcap_t *arg1) { return pcap_setsampling(arg1); }\nsigned int hs_bindgen_d8464231f68f7e7e (char *arg1, char *arg2, char *arg3, char *arg4, struct pcap_rmtauth *arg5, char *arg6) { return pcap_remoteact_accept(arg1, arg2, arg3, arg4, arg5, arg6); }\nsigned int hs_bindgen_b26cbce58c2eefaa (char *arg1, char *arg2, char *arg3, char *arg4, struct pcap_rmtauth *arg5, signed int arg6, char *arg7) { return pcap_remoteact_accept_ex(arg1, arg2, arg3, arg4, arg5, arg6, arg7); }\nsigned int hs_bindgen_aafbcc994f05a0c6 (char *arg1, char arg2, signed int arg3, char *arg4) { return pcap_remoteact_list(arg1, arg2, arg3, arg4); }\nsigned int hs_bindgen_d6e6b4d7523eb07b (char *arg1, char *arg2) { return pcap_remoteact_close(arg1, arg2); }\nvoid hs_bindgen_722fd158dbfe4678 (void) { pcap_remoteact_cleanup(); }\n")

newtype Sa_family_t = Sa_family_t
  { un_Sa_family_t :: FC.CUShort
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype C__U_char = C__U_char
  { un_C__U_char :: FC.CUChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype C__U_short = C__U_short
  { un_C__U_short :: FC.CUShort
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype C__U_int = C__U_int
  { un_C__U_int :: FC.CUInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype C__Time_t = C__Time_t
  { un_C__Time_t :: FC.CLong
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype C__Suseconds_t = C__Suseconds_t
  { un_C__Suseconds_t :: FC.CLong
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

data Timeval = Timeval
  { timeval_tv_sec :: C__Time_t
  , timeval_tv_usec :: C__Suseconds_t
  }
  deriving stock (Eq, Show)

instance F.Storable Timeval where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Timeval
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Timeval timeval_tv_sec2 timeval_tv_usec3 ->
               F.pokeByteOff ptr0 (0 :: Int) timeval_tv_sec2
            >> F.pokeByteOff ptr0 (8 :: Int) timeval_tv_usec3

newtype U_char = U_char
  { un_U_char :: C__U_char
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype U_short = U_short
  { un_U_short :: C__U_short
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype U_int = U_int
  { un_U_int :: C__U_int
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

data Sockaddr = Sockaddr
  { sockaddr_sa_family :: Sa_family_t
  , sockaddr_sa_data :: (HsBindgen.Runtime.ConstantArray.ConstantArray 14) FC.CChar
  }
  deriving stock (Eq, Show)

instance F.Storable Sockaddr where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (2 :: Int)

  peek =
    \ptr0 ->
          pure Sockaddr
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (2 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Sockaddr sockaddr_sa_family2 sockaddr_sa_data3 ->
               F.pokeByteOff ptr0 (0 :: Int) sockaddr_sa_family2
            >> F.pokeByteOff ptr0 (2 :: Int) sockaddr_sa_data3

newtype Bpf_int32 = Bpf_int32
  { un_Bpf_int32 :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype Bpf_u_int32 = Bpf_u_int32
  { un_Bpf_u_int32 :: U_int
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

data Bpf_insn = Bpf_insn
  { bpf_insn_code :: U_short
  , bpf_insn_jt :: U_char
  , bpf_insn_jf :: U_char
  , bpf_insn_k :: Bpf_u_int32
  }
  deriving stock (Eq, Show)

instance F.Storable Bpf_insn where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Bpf_insn
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (2 :: Int)
      <*> F.peekByteOff ptr0 (3 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bpf_insn bpf_insn_code2 bpf_insn_jt3 bpf_insn_jf4 bpf_insn_k5 ->
               F.pokeByteOff ptr0 (0 :: Int) bpf_insn_code2
            >> F.pokeByteOff ptr0 (2 :: Int) bpf_insn_jt3
            >> F.pokeByteOff ptr0 (3 :: Int) bpf_insn_jf4
            >> F.pokeByteOff ptr0 (4 :: Int) bpf_insn_k5

data Bpf_program = Bpf_program
  { bpf_program_bf_len :: U_int
  , bpf_program_bf_insns :: F.Ptr Bpf_insn
  }
  deriving stock (Eq, Show)

instance F.Storable Bpf_program where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Bpf_program
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bpf_program bpf_program_bf_len2 bpf_program_bf_insns3 ->
               F.pokeByteOff ptr0 (0 :: Int) bpf_program_bf_len2
            >> F.pokeByteOff ptr0 (8 :: Int) bpf_program_bf_insns3

pCAP_VERSION_MAJOR :: FC.CInt
pCAP_VERSION_MAJOR = (2 :: FC.CInt)

pCAP_VERSION_MINOR :: FC.CInt
pCAP_VERSION_MINOR = (4 :: FC.CInt)

pCAP_ERRBUF_SIZE :: FC.CInt
pCAP_ERRBUF_SIZE = (256 :: FC.CInt)

data Pcap_t

data Pcap_dumper_t

data Pcap_addr = Pcap_addr
  { pcap_addr_next :: F.Ptr Pcap_addr
  , pcap_addr_addr :: F.Ptr Sockaddr
  , pcap_addr_netmask :: F.Ptr Sockaddr
  , pcap_addr_broadaddr :: F.Ptr Sockaddr
  , pcap_addr_dstaddr :: F.Ptr Sockaddr
  }
  deriving stock (Eq, Show)

instance F.Storable Pcap_addr where

  sizeOf = \_ -> (40 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Pcap_addr
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (16 :: Int)
      <*> F.peekByteOff ptr0 (24 :: Int)
      <*> F.peekByteOff ptr0 (32 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Pcap_addr
            pcap_addr_next2
            pcap_addr_addr3
            pcap_addr_netmask4
            pcap_addr_broadaddr5
            pcap_addr_dstaddr6 ->
                 F.pokeByteOff ptr0 (0 :: Int) pcap_addr_next2
              >> F.pokeByteOff ptr0 (8 :: Int) pcap_addr_addr3
              >> F.pokeByteOff ptr0 (16 :: Int) pcap_addr_netmask4
              >> F.pokeByteOff ptr0 (24 :: Int) pcap_addr_broadaddr5
              >> F.pokeByteOff ptr0 (32 :: Int) pcap_addr_dstaddr6

data Pcap_if_t = Pcap_if_t
  { pcap_if_t_next :: F.Ptr Pcap_if_t
  , pcap_if_t_name :: F.Ptr FC.CChar
  , pcap_if_t_description :: F.Ptr FC.CChar
  , pcap_if_t_addresses :: F.Ptr Pcap_addr
  , pcap_if_t_flags :: Bpf_u_int32
  }
  deriving stock (Eq, Show)

instance F.Storable Pcap_if_t where

  sizeOf = \_ -> (40 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Pcap_if_t
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (16 :: Int)
      <*> F.peekByteOff ptr0 (24 :: Int)
      <*> F.peekByteOff ptr0 (32 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Pcap_if_t
            pcap_if_t_next2
            pcap_if_t_name3
            pcap_if_t_description4
            pcap_if_t_addresses5
            pcap_if_t_flags6 ->
                 F.pokeByteOff ptr0 (0 :: Int) pcap_if_t_next2
              >> F.pokeByteOff ptr0 (8 :: Int) pcap_if_t_name3
              >> F.pokeByteOff ptr0 (16 :: Int) pcap_if_t_description4
              >> F.pokeByteOff ptr0 (24 :: Int) pcap_if_t_addresses5
              >> F.pokeByteOff ptr0 (32 :: Int) pcap_if_t_flags6

newtype Pcap_addr_t = Pcap_addr_t
  { un_Pcap_addr_t :: Pcap_addr
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

data Pcap_file_header = Pcap_file_header
  { pcap_file_header_magic :: Bpf_u_int32
  , pcap_file_header_version_major :: U_short
  , pcap_file_header_version_minor :: U_short
  , pcap_file_header_thiszone :: Bpf_int32
  , pcap_file_header_sigfigs :: Bpf_u_int32
  , pcap_file_header_snaplen :: Bpf_u_int32
  , pcap_file_header_linktype :: Bpf_u_int32
  }
  deriving stock (Eq, Show)

instance F.Storable Pcap_file_header where

  sizeOf = \_ -> (24 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Pcap_file_header
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)
      <*> F.peekByteOff ptr0 (6 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (12 :: Int)
      <*> F.peekByteOff ptr0 (16 :: Int)
      <*> F.peekByteOff ptr0 (20 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Pcap_file_header
            pcap_file_header_magic2
            pcap_file_header_version_major3
            pcap_file_header_version_minor4
            pcap_file_header_thiszone5
            pcap_file_header_sigfigs6
            pcap_file_header_snaplen7
            pcap_file_header_linktype8 ->
                 F.pokeByteOff ptr0 (0 :: Int) pcap_file_header_magic2
              >> F.pokeByteOff ptr0 (4 :: Int) pcap_file_header_version_major3
              >> F.pokeByteOff ptr0 (6 :: Int) pcap_file_header_version_minor4
              >> F.pokeByteOff ptr0 (8 :: Int) pcap_file_header_thiszone5
              >> F.pokeByteOff ptr0 (12 :: Int) pcap_file_header_sigfigs6
              >> F.pokeByteOff ptr0 (16 :: Int) pcap_file_header_snaplen7
              >> F.pokeByteOff ptr0 (20 :: Int) pcap_file_header_linktype8

lT_LINKTYPE :: forall a0. (C.Bitwise a0) FC.CInt => a0 -> (C.BitsRes a0) FC.CInt
lT_LINKTYPE = \x0 -> (.&.) x0 (65535 :: FC.CInt)

lT_LINKTYPE_EXT :: forall a0. (C.Bitwise a0) FC.CInt => a0 -> (C.BitsRes a0) FC.CInt
lT_LINKTYPE_EXT =
  \x0 -> (.&.) x0 (4294901760 :: FC.CInt)

lT_RESERVED1 :: forall a0. (C.Bitwise a0) FC.CInt => a0 -> (C.BitsRes a0) FC.CInt
lT_RESERVED1 = \x0 -> (.&.) x0 (67043328 :: FC.CInt)

lT_FCS_LENGTH_PRESENT :: forall a0. (C.Bitwise a0) FC.CInt => a0 -> (C.BitsRes a0) FC.CInt
lT_FCS_LENGTH_PRESENT =
  \x0 -> (.&.) x0 (67108864 :: FC.CInt)

lT_FCS_LENGTH :: forall a0. (C.Bitwise a0) FC.CInt => (C.Shift ((C.BitsRes a0) FC.CInt)) FC.CInt => a0 -> C.ShiftRes ((C.BitsRes a0) FC.CInt)
lT_FCS_LENGTH =
  \x0 ->
    (C.>>) ((.&.) x0 (4026531840 :: FC.CInt)) (28 :: FC.CInt)

lT_FCS_DATALINK_EXT :: forall a0. (C.Bitwise a0) FC.CInt => (C.Bitwise (C.ShiftRes ((C.BitsRes a0) FC.CInt))) FC.CInt => (C.Shift ((C.BitsRes a0) FC.CInt)) FC.CInt => a0 -> (C.BitsRes (C.ShiftRes ((C.BitsRes a0) FC.CInt))) FC.CInt
lT_FCS_DATALINK_EXT =
  \x0 ->
    (.|.) ((<<) ((.&.) x0 (15 :: FC.CInt)) (28 :: FC.CInt)) (67108864 :: FC.CInt)

newtype Pcap_direction_t = Pcap_direction_t
  { un_Pcap_direction_t :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable Pcap_direction_t where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Pcap_direction_t
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Pcap_direction_t un_Pcap_direction_t2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_Pcap_direction_t2

instance HsBindgen.Runtime.CEnum.CEnum Pcap_direction_t where

  type CEnumZ Pcap_direction_t = FC.CUInt

  toCEnum = Pcap_direction_t

  fromCEnum = un_Pcap_direction_t

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "PCAP_D_INOUT")
                                                     , (1, Data.List.NonEmpty.singleton "PCAP_D_IN")
                                                     , (2, Data.List.NonEmpty.singleton "PCAP_D_OUT")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Pcap_direction_t"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Pcap_direction_t"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Pcap_direction_t where

  minDeclaredValue = PCAP_D_INOUT

  maxDeclaredValue = PCAP_D_OUT

instance Show Pcap_direction_t where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Pcap_direction_t where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern PCAP_D_INOUT :: Pcap_direction_t
pattern PCAP_D_INOUT = Pcap_direction_t 0

pattern PCAP_D_IN :: Pcap_direction_t
pattern PCAP_D_IN = Pcap_direction_t 1

pattern PCAP_D_OUT :: Pcap_direction_t
pattern PCAP_D_OUT = Pcap_direction_t 2

data Pcap_pkthdr = Pcap_pkthdr
  { pcap_pkthdr_ts :: Timeval
  , pcap_pkthdr_caplen :: Bpf_u_int32
  , pcap_pkthdr_len :: Bpf_u_int32
  }
  deriving stock (Eq, Show)

instance F.Storable Pcap_pkthdr where

  sizeOf = \_ -> (24 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Pcap_pkthdr
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (16 :: Int)
      <*> F.peekByteOff ptr0 (20 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Pcap_pkthdr pcap_pkthdr_ts2 pcap_pkthdr_caplen3 pcap_pkthdr_len4 ->
               F.pokeByteOff ptr0 (0 :: Int) pcap_pkthdr_ts2
            >> F.pokeByteOff ptr0 (16 :: Int) pcap_pkthdr_caplen3
            >> F.pokeByteOff ptr0 (20 :: Int) pcap_pkthdr_len4

data Pcap_stat = Pcap_stat
  { pcap_stat_ps_recv :: U_int
  , pcap_stat_ps_drop :: U_int
  , pcap_stat_ps_ifdrop :: U_int
  }
  deriving stock (Eq, Show)

instance F.Storable Pcap_stat where

  sizeOf = \_ -> (12 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Pcap_stat
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Pcap_stat pcap_stat_ps_recv2 pcap_stat_ps_drop3 pcap_stat_ps_ifdrop4 ->
               F.pokeByteOff ptr0 (0 :: Int) pcap_stat_ps_recv2
            >> F.pokeByteOff ptr0 (4 :: Int) pcap_stat_ps_drop3
            >> F.pokeByteOff ptr0 (8 :: Int) pcap_stat_ps_ifdrop4

pCAP_IF_LOOPBACK :: FC.CInt
pCAP_IF_LOOPBACK = (1 :: FC.CInt)

pCAP_IF_UP :: FC.CInt
pCAP_IF_UP = (2 :: FC.CInt)

pCAP_IF_RUNNING :: FC.CInt
pCAP_IF_RUNNING = (4 :: FC.CInt)

pCAP_IF_WIRELESS :: FC.CInt
pCAP_IF_WIRELESS = (8 :: FC.CInt)

pCAP_IF_CONNECTION_STATUS :: FC.CInt
pCAP_IF_CONNECTION_STATUS = (48 :: FC.CInt)

pCAP_IF_CONNECTION_STATUS_UNKNOWN :: FC.CInt
pCAP_IF_CONNECTION_STATUS_UNKNOWN = (0 :: FC.CInt)

pCAP_IF_CONNECTION_STATUS_CONNECTED :: FC.CInt
pCAP_IF_CONNECTION_STATUS_CONNECTED = (16 :: FC.CInt)

pCAP_IF_CONNECTION_STATUS_DISCONNECTED :: FC.CInt
pCAP_IF_CONNECTION_STATUS_DISCONNECTED =
  (32 :: FC.CInt)

pCAP_IF_CONNECTION_STATUS_NOT_APPLICABLE :: FC.CInt
pCAP_IF_CONNECTION_STATUS_NOT_APPLICABLE =
  (48 :: FC.CInt)

newtype Pcap_handler = Pcap_handler
  { un_Pcap_handler :: F.FunPtr ((F.Ptr U_char) -> (F.Ptr Pcap_pkthdr) -> (F.Ptr U_char) -> IO ())
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

pCAP_ERROR :: FC.CInt
pCAP_ERROR = C.negate (1 :: FC.CInt)

pCAP_ERROR_BREAK :: FC.CInt
pCAP_ERROR_BREAK = C.negate (2 :: FC.CInt)

pCAP_ERROR_NOT_ACTIVATED :: FC.CInt
pCAP_ERROR_NOT_ACTIVATED = C.negate (3 :: FC.CInt)

pCAP_ERROR_ACTIVATED :: FC.CInt
pCAP_ERROR_ACTIVATED = C.negate (4 :: FC.CInt)

pCAP_ERROR_NO_SUCH_DEVICE :: FC.CInt
pCAP_ERROR_NO_SUCH_DEVICE = C.negate (5 :: FC.CInt)

pCAP_ERROR_RFMON_NOTSUP :: FC.CInt
pCAP_ERROR_RFMON_NOTSUP = C.negate (6 :: FC.CInt)

pCAP_ERROR_NOT_RFMON :: FC.CInt
pCAP_ERROR_NOT_RFMON = C.negate (7 :: FC.CInt)

pCAP_ERROR_PERM_DENIED :: FC.CInt
pCAP_ERROR_PERM_DENIED = C.negate (8 :: FC.CInt)

pCAP_ERROR_IFACE_NOT_UP :: FC.CInt
pCAP_ERROR_IFACE_NOT_UP = C.negate (9 :: FC.CInt)

pCAP_ERROR_CANTSET_TSTAMP_TYPE :: FC.CInt
pCAP_ERROR_CANTSET_TSTAMP_TYPE =
  C.negate (10 :: FC.CInt)

pCAP_ERROR_PROMISC_PERM_DENIED :: FC.CInt
pCAP_ERROR_PROMISC_PERM_DENIED =
  C.negate (11 :: FC.CInt)

pCAP_ERROR_TSTAMP_PRECISION_NOTSUP :: FC.CInt
pCAP_ERROR_TSTAMP_PRECISION_NOTSUP =
  C.negate (12 :: FC.CInt)

pCAP_ERROR_CAPTURE_NOTSUP :: FC.CInt
pCAP_ERROR_CAPTURE_NOTSUP = C.negate (13 :: FC.CInt)

pCAP_WARNING :: FC.CInt
pCAP_WARNING = (1 :: FC.CInt)

pCAP_WARNING_PROMISC_NOTSUP :: FC.CInt
pCAP_WARNING_PROMISC_NOTSUP = (2 :: FC.CInt)

pCAP_WARNING_TSTAMP_TYPE_NOTSUP :: FC.CInt
pCAP_WARNING_TSTAMP_TYPE_NOTSUP = (3 :: FC.CInt)

pCAP_NETMASK_UNKNOWN :: FC.CInt
pCAP_NETMASK_UNKNOWN = (4294967295 :: FC.CInt)

pCAP_CHAR_ENC_LOCAL :: FC.CUInt
pCAP_CHAR_ENC_LOCAL = (0 :: FC.CUInt)

pCAP_CHAR_ENC_UTF_8 :: FC.CUInt
pCAP_CHAR_ENC_UTF_8 = (1 :: FC.CUInt)

{-| __from C:__ @pcap_init@ -}
foreign import ccall safe "hs_bindgen_8e690804091c04ec" pcap_init
  :: FC.CUInt
  -> F.Ptr FC.CChar
  -> IO FC.CInt

{-| __from C:__ @pcap_lookupdev@ -}
foreign import ccall safe "hs_bindgen_46bc906cad541290" pcap_lookupdev
  :: F.Ptr FC.CChar
  -> IO (F.Ptr FC.CChar)

{-| __from C:__ @pcap_lookupnet@ -}
foreign import ccall safe "hs_bindgen_b19f8a50c4c1eed4" pcap_lookupnet
  :: F.Ptr FC.CChar
  -> F.Ptr Bpf_u_int32
  -> F.Ptr Bpf_u_int32
  -> F.Ptr FC.CChar
  -> IO FC.CInt

{-| __from C:__ @pcap_create@ -}
foreign import ccall safe "hs_bindgen_7b7834bca252cee9" pcap_create
  :: F.Ptr FC.CChar
  -> F.Ptr FC.CChar
  -> IO (F.Ptr Pcap_t)

{-| __from C:__ @pcap_set_snaplen@ -}
foreign import ccall safe "hs_bindgen_8553475d6a4d4fc1" pcap_set_snaplen
  :: F.Ptr Pcap_t
  -> FC.CInt
  -> IO FC.CInt

{-| __from C:__ @pcap_set_promisc@ -}
foreign import ccall safe "hs_bindgen_e20ccd92ea74bdd8" pcap_set_promisc
  :: F.Ptr Pcap_t
  -> FC.CInt
  -> IO FC.CInt

{-| __from C:__ @pcap_can_set_rfmon@ -}
foreign import ccall safe "hs_bindgen_cc2851957e1781b8" pcap_can_set_rfmon
  :: F.Ptr Pcap_t
  -> IO FC.CInt

{-| __from C:__ @pcap_set_rfmon@ -}
foreign import ccall safe "hs_bindgen_b840e978c312fc4b" pcap_set_rfmon
  :: F.Ptr Pcap_t
  -> FC.CInt
  -> IO FC.CInt

{-| __from C:__ @pcap_set_timeout@ -}
foreign import ccall safe "hs_bindgen_0b63cda6c932ebd4" pcap_set_timeout
  :: F.Ptr Pcap_t
  -> FC.CInt
  -> IO FC.CInt

{-| __from C:__ @pcap_set_tstamp_type@ -}
foreign import ccall safe "hs_bindgen_4aee540f1876beed" pcap_set_tstamp_type
  :: F.Ptr Pcap_t
  -> FC.CInt
  -> IO FC.CInt

{-| __from C:__ @pcap_set_immediate_mode@ -}
foreign import ccall safe "hs_bindgen_6198eda39caaee0b" pcap_set_immediate_mode
  :: F.Ptr Pcap_t
  -> FC.CInt
  -> IO FC.CInt

{-| __from C:__ @pcap_set_buffer_size@ -}
foreign import ccall safe "hs_bindgen_43e52f65308dc82e" pcap_set_buffer_size
  :: F.Ptr Pcap_t
  -> FC.CInt
  -> IO FC.CInt

{-| __from C:__ @pcap_set_tstamp_precision@ -}
foreign import ccall safe "hs_bindgen_bcb7b2d106e0cd52" pcap_set_tstamp_precision
  :: F.Ptr Pcap_t
  -> FC.CInt
  -> IO FC.CInt

{-| __from C:__ @pcap_get_tstamp_precision@ -}
foreign import ccall safe "hs_bindgen_79d362811d578175" pcap_get_tstamp_precision
  :: F.Ptr Pcap_t
  -> IO FC.CInt

{-| __from C:__ @pcap_activate@ -}
foreign import ccall safe "hs_bindgen_54ea2d199ac49ebc" pcap_activate
  :: F.Ptr Pcap_t
  -> IO FC.CInt

{-| __from C:__ @pcap_list_tstamp_types@ -}
foreign import ccall safe "hs_bindgen_43af2237985682b1" pcap_list_tstamp_types
  :: F.Ptr Pcap_t
  -> F.Ptr (F.Ptr FC.CInt)
  -> IO FC.CInt

{-| __from C:__ @pcap_free_tstamp_types@ -}
foreign import ccall safe "hs_bindgen_3c888668e9412a05" pcap_free_tstamp_types
  :: F.Ptr FC.CInt
  -> IO ()

{-| __from C:__ @pcap_tstamp_type_name_to_val@ -}
foreign import ccall safe "hs_bindgen_c8eb219382cf1b46" pcap_tstamp_type_name_to_val
  :: F.Ptr FC.CChar
  -> IO FC.CInt

{-| __from C:__ @pcap_tstamp_type_val_to_name@ -}
foreign import ccall safe "hs_bindgen_d4a57d99e333b885" pcap_tstamp_type_val_to_name
  :: FC.CInt
  -> IO (F.Ptr FC.CChar)

{-| __from C:__ @pcap_tstamp_type_val_to_description@ -}
foreign import ccall safe "hs_bindgen_9734f5e64e4f9bdc" pcap_tstamp_type_val_to_description
  :: FC.CInt
  -> IO (F.Ptr FC.CChar)

{-| __from C:__ @pcap_set_protocol_linux@ -}
foreign import ccall safe "hs_bindgen_23886cba4b92f2a2" pcap_set_protocol_linux
  :: F.Ptr Pcap_t
  -> FC.CInt
  -> IO FC.CInt

pCAP_TSTAMP_HOST_LOWPREC :: FC.CInt
pCAP_TSTAMP_HOST_LOWPREC = (1 :: FC.CInt)

pCAP_TSTAMP_HOST_HIPREC :: FC.CInt
pCAP_TSTAMP_HOST_HIPREC = (2 :: FC.CInt)

pCAP_TSTAMP_ADAPTER :: FC.CInt
pCAP_TSTAMP_ADAPTER = (3 :: FC.CInt)

pCAP_TSTAMP_ADAPTER_UNSYNCED :: FC.CInt
pCAP_TSTAMP_ADAPTER_UNSYNCED = (4 :: FC.CInt)

pCAP_TSTAMP_HOST_HIPREC_UNSYNCED :: FC.CInt
pCAP_TSTAMP_HOST_HIPREC_UNSYNCED = (5 :: FC.CInt)

pCAP_TSTAMP_PRECISION_NANO :: FC.CInt
pCAP_TSTAMP_PRECISION_NANO = (1 :: FC.CInt)

{-| __from C:__ @pcap_open_live@ -}
foreign import ccall safe "hs_bindgen_b7fb79264f562481" pcap_open_live
  :: F.Ptr FC.CChar
  -> FC.CInt
  -> FC.CInt
  -> FC.CInt
  -> F.Ptr FC.CChar
  -> IO (F.Ptr Pcap_t)

{-| __from C:__ @pcap_open_dead@ -}
foreign import ccall safe "hs_bindgen_76136638a41c6eeb" pcap_open_dead
  :: FC.CInt
  -> FC.CInt
  -> IO (F.Ptr Pcap_t)

{-| __from C:__ @pcap_open_dead_with_tstamp_precision@ -}
foreign import ccall safe "hs_bindgen_29a88236a4ecde00" pcap_open_dead_with_tstamp_precision
  :: FC.CInt
  -> FC.CInt
  -> U_int
  -> IO (F.Ptr Pcap_t)

{-| __from C:__ @pcap_open_offline_with_tstamp_precision@ -}
foreign import ccall safe "hs_bindgen_16814752302d61ee" pcap_open_offline_with_tstamp_precision
  :: F.Ptr FC.CChar
  -> U_int
  -> F.Ptr FC.CChar
  -> IO (F.Ptr Pcap_t)

{-| __from C:__ @pcap_open_offline@ -}
foreign import ccall safe "hs_bindgen_eb04b53463f10143" pcap_open_offline
  :: F.Ptr FC.CChar
  -> F.Ptr FC.CChar
  -> IO (F.Ptr Pcap_t)

{-| __from C:__ @pcap_fopen_offline_with_tstamp_precision@ -}
foreign import ccall safe "hs_bindgen_ff917f556333c221" pcap_fopen_offline_with_tstamp_precision
  :: F.Ptr HsBindgen.Runtime.Prelude.CFile
  -> U_int
  -> F.Ptr FC.CChar
  -> IO (F.Ptr Pcap_t)

{-| __from C:__ @pcap_fopen_offline@ -}
foreign import ccall safe "hs_bindgen_94640023a569342c" pcap_fopen_offline
  :: F.Ptr HsBindgen.Runtime.Prelude.CFile
  -> F.Ptr FC.CChar
  -> IO (F.Ptr Pcap_t)

{-| __from C:__ @pcap_close@ -}
foreign import ccall safe "hs_bindgen_399eb2733162cc22" pcap_close
  :: F.Ptr Pcap_t
  -> IO ()

{-| __from C:__ @pcap_loop@ -}
foreign import ccall safe "hs_bindgen_604fcacd649d777d" pcap_loop
  :: F.Ptr Pcap_t
  -> FC.CInt
  -> Pcap_handler
  -> F.Ptr U_char
  -> IO FC.CInt

{-| __from C:__ @pcap_dispatch@ -}
foreign import ccall safe "hs_bindgen_c74069ed8f91b583" pcap_dispatch
  :: F.Ptr Pcap_t
  -> FC.CInt
  -> Pcap_handler
  -> F.Ptr U_char
  -> IO FC.CInt

{-| __from C:__ @pcap_next@ -}
foreign import ccall safe "hs_bindgen_33a8bc05c5093742" pcap_next
  :: F.Ptr Pcap_t
  -> F.Ptr Pcap_pkthdr
  -> IO (F.Ptr U_char)

{-| __from C:__ @pcap_next_ex@ -}
foreign import ccall safe "hs_bindgen_9b4a988026a25a55" pcap_next_ex
  :: F.Ptr Pcap_t
  -> F.Ptr (F.Ptr Pcap_pkthdr)
  -> F.Ptr (F.Ptr U_char)
  -> IO FC.CInt

{-| __from C:__ @pcap_breakloop@ -}
foreign import ccall safe "hs_bindgen_8f560852ea6217b6" pcap_breakloop
  :: F.Ptr Pcap_t
  -> IO ()

{-| __from C:__ @pcap_stats@ -}
foreign import ccall safe "hs_bindgen_b3debcdebc5dcc97" pcap_stats
  :: F.Ptr Pcap_t
  -> F.Ptr Pcap_stat
  -> IO FC.CInt

{-| __from C:__ @pcap_setfilter@ -}
foreign import ccall safe "hs_bindgen_64b03a9edc688b6b" pcap_setfilter
  :: F.Ptr Pcap_t
  -> F.Ptr Bpf_program
  -> IO FC.CInt

{-| __from C:__ @pcap_setdirection@ -}
foreign import ccall safe "hs_bindgen_953baf14d7ce632e" pcap_setdirection
  :: F.Ptr Pcap_t
  -> Pcap_direction_t
  -> IO FC.CInt

{-| __from C:__ @pcap_getnonblock@ -}
foreign import ccall safe "hs_bindgen_5bd8498726f4d5d8" pcap_getnonblock
  :: F.Ptr Pcap_t
  -> F.Ptr FC.CChar
  -> IO FC.CInt

{-| __from C:__ @pcap_setnonblock@ -}
foreign import ccall safe "hs_bindgen_687c80d39a8640a8" pcap_setnonblock
  :: F.Ptr Pcap_t
  -> FC.CInt
  -> F.Ptr FC.CChar
  -> IO FC.CInt

{-| __from C:__ @pcap_inject@ -}
foreign import ccall safe "hs_bindgen_2b120405a2329bd9" pcap_inject
  :: F.Ptr Pcap_t
  -> F.Ptr Void
  -> HsBindgen.Runtime.Prelude.CSize
  -> IO FC.CInt

{-| __from C:__ @pcap_sendpacket@ -}
foreign import ccall safe "hs_bindgen_27b6362138d9d7f5" pcap_sendpacket
  :: F.Ptr Pcap_t
  -> F.Ptr U_char
  -> FC.CInt
  -> IO FC.CInt

{-| __from C:__ @pcap_statustostr@ -}
foreign import ccall safe "hs_bindgen_edb007bfd4fcc3a6" pcap_statustostr
  :: FC.CInt
  -> IO (F.Ptr FC.CChar)

{-| __from C:__ @pcap_strerror@ -}
foreign import ccall safe "hs_bindgen_eeef2be579076234" pcap_strerror
  :: FC.CInt
  -> IO (F.Ptr FC.CChar)

{-| __from C:__ @pcap_geterr@ -}
foreign import ccall safe "hs_bindgen_59d6290683549d60" pcap_geterr
  :: F.Ptr Pcap_t
  -> IO (F.Ptr FC.CChar)

{-| __from C:__ @pcap_perror@ -}
foreign import ccall safe "hs_bindgen_9c9c62590faa0950" pcap_perror
  :: F.Ptr Pcap_t
  -> F.Ptr FC.CChar
  -> IO ()

{-| __from C:__ @pcap_compile@ -}
foreign import ccall safe "hs_bindgen_24112b18ae5550c9" pcap_compile
  :: F.Ptr Pcap_t
  -> F.Ptr Bpf_program
  -> F.Ptr FC.CChar
  -> FC.CInt
  -> Bpf_u_int32
  -> IO FC.CInt

{-| __from C:__ @pcap_compile_nopcap@ -}
foreign import ccall safe "hs_bindgen_cc5b08bebf5cae34" pcap_compile_nopcap
  :: FC.CInt
  -> FC.CInt
  -> F.Ptr Bpf_program
  -> F.Ptr FC.CChar
  -> FC.CInt
  -> Bpf_u_int32
  -> IO FC.CInt

{-| __from C:__ @pcap_freecode@ -}
foreign import ccall safe "hs_bindgen_009c484b78096901" pcap_freecode
  :: F.Ptr Bpf_program
  -> IO ()

{-| __from C:__ @pcap_offline_filter@ -}
foreign import ccall safe "hs_bindgen_ca4bf9fbce12f46d" pcap_offline_filter
  :: F.Ptr Bpf_program
  -> F.Ptr Pcap_pkthdr
  -> F.Ptr U_char
  -> IO FC.CInt

{-| __from C:__ @pcap_datalink@ -}
foreign import ccall safe "hs_bindgen_6310f22b400cfbea" pcap_datalink
  :: F.Ptr Pcap_t
  -> IO FC.CInt

{-| __from C:__ @pcap_datalink_ext@ -}
foreign import ccall safe "hs_bindgen_8d263b0b0ccb525f" pcap_datalink_ext
  :: F.Ptr Pcap_t
  -> IO FC.CInt

{-| __from C:__ @pcap_list_datalinks@ -}
foreign import ccall safe "hs_bindgen_7a812b86d4676fa1" pcap_list_datalinks
  :: F.Ptr Pcap_t
  -> F.Ptr (F.Ptr FC.CInt)
  -> IO FC.CInt

{-| __from C:__ @pcap_set_datalink@ -}
foreign import ccall safe "hs_bindgen_3d961067e2512ee8" pcap_set_datalink
  :: F.Ptr Pcap_t
  -> FC.CInt
  -> IO FC.CInt

{-| __from C:__ @pcap_free_datalinks@ -}
foreign import ccall safe "hs_bindgen_2e77a3d02d2e4969" pcap_free_datalinks
  :: F.Ptr FC.CInt
  -> IO ()

{-| __from C:__ @pcap_datalink_name_to_val@ -}
foreign import ccall safe "hs_bindgen_e26516a9409e18d2" pcap_datalink_name_to_val
  :: F.Ptr FC.CChar
  -> IO FC.CInt

{-| __from C:__ @pcap_datalink_val_to_name@ -}
foreign import ccall safe "hs_bindgen_da0d5e544e7ffe16" pcap_datalink_val_to_name
  :: FC.CInt
  -> IO (F.Ptr FC.CChar)

{-| __from C:__ @pcap_datalink_val_to_description@ -}
foreign import ccall safe "hs_bindgen_ec025504038351d3" pcap_datalink_val_to_description
  :: FC.CInt
  -> IO (F.Ptr FC.CChar)

{-| __from C:__ @pcap_datalink_val_to_description_or_dlt@ -}
foreign import ccall safe "hs_bindgen_7018c9f6223919b0" pcap_datalink_val_to_description_or_dlt
  :: FC.CInt
  -> IO (F.Ptr FC.CChar)

{-| __from C:__ @pcap_snapshot@ -}
foreign import ccall safe "hs_bindgen_29c299a09deb8827" pcap_snapshot
  :: F.Ptr Pcap_t
  -> IO FC.CInt

{-| __from C:__ @pcap_is_swapped@ -}
foreign import ccall safe "hs_bindgen_d7d4aeb73315e298" pcap_is_swapped
  :: F.Ptr Pcap_t
  -> IO FC.CInt

{-| __from C:__ @pcap_major_version@ -}
foreign import ccall safe "hs_bindgen_957c445dfb0617cb" pcap_major_version
  :: F.Ptr Pcap_t
  -> IO FC.CInt

{-| __from C:__ @pcap_minor_version@ -}
foreign import ccall safe "hs_bindgen_f899c77267bc823b" pcap_minor_version
  :: F.Ptr Pcap_t
  -> IO FC.CInt

{-| __from C:__ @pcap_bufsize@ -}
foreign import ccall safe "hs_bindgen_8a687e5c2618f8ce" pcap_bufsize
  :: F.Ptr Pcap_t
  -> IO FC.CInt

{-| __from C:__ @pcap_file@ -}
foreign import ccall safe "hs_bindgen_465f70ee9f01bd6f" pcap_file
  :: F.Ptr Pcap_t
  -> IO (F.Ptr HsBindgen.Runtime.Prelude.CFile)

{-| __from C:__ @pcap_fileno@ -}
foreign import ccall safe "hs_bindgen_72bc897a01fd5c11" pcap_fileno
  :: F.Ptr Pcap_t
  -> IO FC.CInt

{-| __from C:__ @pcap_dump_open@ -}
foreign import ccall safe "hs_bindgen_82030f5f2650d881" pcap_dump_open
  :: F.Ptr Pcap_t
  -> F.Ptr FC.CChar
  -> IO (F.Ptr Pcap_dumper_t)

{-| __from C:__ @pcap_dump_fopen@ -}
foreign import ccall safe "hs_bindgen_35a8ef64632d544c" pcap_dump_fopen
  :: F.Ptr Pcap_t
  -> F.Ptr HsBindgen.Runtime.Prelude.CFile
     {- ^ __from C:__ @fp@ -}
  -> IO (F.Ptr Pcap_dumper_t)

{-| __from C:__ @pcap_dump_open_append@ -}
foreign import ccall safe "hs_bindgen_da36fd9375a43c56" pcap_dump_open_append
  :: F.Ptr Pcap_t
  -> F.Ptr FC.CChar
  -> IO (F.Ptr Pcap_dumper_t)

{-| __from C:__ @pcap_dump_file@ -}
foreign import ccall safe "hs_bindgen_32f2969fc086c0bf" pcap_dump_file
  :: F.Ptr Pcap_dumper_t
  -> IO (F.Ptr HsBindgen.Runtime.Prelude.CFile)

{-| __from C:__ @pcap_dump_ftell@ -}
foreign import ccall safe "hs_bindgen_cc64b0e3af70fdb5" pcap_dump_ftell
  :: F.Ptr Pcap_dumper_t
  -> IO FC.CLong

{-| __from C:__ @pcap_dump_ftell64@ -}
foreign import ccall safe "hs_bindgen_6e260398dc6bcf99" pcap_dump_ftell64
  :: F.Ptr Pcap_dumper_t
  -> IO HsBindgen.Runtime.Prelude.Int64

{-| __from C:__ @pcap_dump_flush@ -}
foreign import ccall safe "hs_bindgen_c04f066c730c44ba" pcap_dump_flush
  :: F.Ptr Pcap_dumper_t
  -> IO FC.CInt

{-| __from C:__ @pcap_dump_close@ -}
foreign import ccall safe "hs_bindgen_d20794d8ba8cb77f" pcap_dump_close
  :: F.Ptr Pcap_dumper_t
  -> IO ()

{-| __from C:__ @pcap_dump@ -}
foreign import ccall safe "hs_bindgen_e03b7a33ac92dda3" pcap_dump
  :: F.Ptr U_char
  -> F.Ptr Pcap_pkthdr
  -> F.Ptr U_char
  -> IO ()

{-| __from C:__ @pcap_findalldevs@ -}
foreign import ccall safe "hs_bindgen_cf38ad940dc3db2c" pcap_findalldevs
  :: F.Ptr (F.Ptr Pcap_if_t)
  -> F.Ptr FC.CChar
  -> IO FC.CInt

{-| __from C:__ @pcap_freealldevs@ -}
foreign import ccall safe "hs_bindgen_6ed42d89cc05b063" pcap_freealldevs
  :: F.Ptr Pcap_if_t
  -> IO ()

{-| __from C:__ @pcap_lib_version@ -}
foreign import ccall safe "hs_bindgen_5d098d3a885d036a" pcap_lib_version
  :: IO (F.Ptr FC.CChar)

{-| __from C:__ @pcap_get_selectable_fd@ -}
foreign import ccall safe "hs_bindgen_db4f32072d198ad8" pcap_get_selectable_fd
  :: F.Ptr Pcap_t
  -> IO FC.CInt

{-| __from C:__ @pcap_get_required_select_timeout@ -}
foreign import ccall safe "hs_bindgen_9e521ac37c1483c8" pcap_get_required_select_timeout
  :: F.Ptr Pcap_t
  -> IO (F.Ptr Timeval)

pCAP_BUF_SIZE :: FC.CInt
pCAP_BUF_SIZE = (1024 :: FC.CInt)

pCAP_SRC_FILE :: FC.CInt
pCAP_SRC_FILE = (2 :: FC.CInt)

pCAP_SRC_IFLOCAL :: FC.CInt
pCAP_SRC_IFLOCAL = (3 :: FC.CInt)

pCAP_SRC_IFREMOTE :: FC.CInt
pCAP_SRC_IFREMOTE = (4 :: FC.CInt)

-- pCAP_SRC_FILE_STRING :: ((,) (F.Ptr FC.CChar)) Int
-- pCAP_SRC_FILE_STRING =
--   (((F.Ptr "file://"#), 7) :: FC.CStringLen)

-- pCAP_SRC_IF_STRING :: ((,) (F.Ptr FC.CChar)) Int
-- pCAP_SRC_IF_STRING =
--   (((F.Ptr "rpcap://"#), 8) :: FC.CStringLen)

pCAP_OPENFLAG_PROMISCUOUS :: FC.CInt
pCAP_OPENFLAG_PROMISCUOUS = (1 :: FC.CInt)

pCAP_OPENFLAG_DATATX_UDP :: FC.CInt
pCAP_OPENFLAG_DATATX_UDP = (2 :: FC.CInt)

pCAP_OPENFLAG_NOCAPTURE_RPCAP :: FC.CInt
pCAP_OPENFLAG_NOCAPTURE_RPCAP = (4 :: FC.CInt)

pCAP_OPENFLAG_NOCAPTURE_LOCAL :: FC.CInt
pCAP_OPENFLAG_NOCAPTURE_LOCAL = (8 :: FC.CInt)

pCAP_OPENFLAG_MAX_RESPONSIVENESS :: FC.CInt
pCAP_OPENFLAG_MAX_RESPONSIVENESS = (16 :: FC.CInt)

rPCAP_RMTAUTH_PWD :: FC.CInt
rPCAP_RMTAUTH_PWD = (1 :: FC.CInt)

data Pcap_rmtauth = Pcap_rmtauth
  { pcap_rmtauth_type :: FC.CInt
  , pcap_rmtauth_username :: F.Ptr FC.CChar
  , pcap_rmtauth_password :: F.Ptr FC.CChar
  }
  deriving stock (Eq, Show)

instance F.Storable Pcap_rmtauth where

  sizeOf = \_ -> (24 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Pcap_rmtauth
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (16 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Pcap_rmtauth pcap_rmtauth_type2 pcap_rmtauth_username3 pcap_rmtauth_password4 ->
               F.pokeByteOff ptr0 (0 :: Int) pcap_rmtauth_type2
            >> F.pokeByteOff ptr0 (8 :: Int) pcap_rmtauth_username3
            >> F.pokeByteOff ptr0 (16 :: Int) pcap_rmtauth_password4

{-| __from C:__ @pcap_open@ -}
foreign import ccall safe "hs_bindgen_c2698a21a32b9dff" pcap_open
  :: F.Ptr FC.CChar
     {- ^ __from C:__ @source@ -}
  -> FC.CInt
     {- ^ __from C:__ @snaplen@ -}
  -> FC.CInt
     {- ^ __from C:__ @flags@ -}
  -> FC.CInt
     {- ^ __from C:__ @read_timeout@ -}
  -> F.Ptr Pcap_rmtauth
     {- ^ __from C:__ @auth@ -}
  -> F.Ptr FC.CChar
     {- ^ __from C:__ @errbuf@ -}
  -> IO (F.Ptr Pcap_t)

{-| __from C:__ @pcap_createsrcstr@ -}
foreign import ccall safe "hs_bindgen_8df98cacb3acc9dd" pcap_createsrcstr
  :: F.Ptr FC.CChar
     {- ^ __from C:__ @source@ -}
  -> FC.CInt
     {- ^ __from C:__ @type'@ -}
  -> F.Ptr FC.CChar
     {- ^ __from C:__ @host@ -}
  -> F.Ptr FC.CChar
     {- ^ __from C:__ @port@ -}
  -> F.Ptr FC.CChar
     {- ^ __from C:__ @name@ -}
  -> F.Ptr FC.CChar
     {- ^ __from C:__ @errbuf@ -}
  -> IO FC.CInt

{-| __from C:__ @pcap_parsesrcstr@ -}
foreign import ccall safe "hs_bindgen_7f18ff5dfbf6569c" pcap_parsesrcstr
  :: F.Ptr FC.CChar
     {- ^ __from C:__ @source@ -}
  -> F.Ptr FC.CInt
     {- ^ __from C:__ @type'@ -}
  -> F.Ptr FC.CChar
     {- ^ __from C:__ @host@ -}
  -> F.Ptr FC.CChar
     {- ^ __from C:__ @port@ -}
  -> F.Ptr FC.CChar
     {- ^ __from C:__ @name@ -}
  -> F.Ptr FC.CChar
     {- ^ __from C:__ @errbuf@ -}
  -> IO FC.CInt

{-| __from C:__ @pcap_findalldevs_ex@ -}
foreign import ccall safe "hs_bindgen_a8ed87fc59e6b669" pcap_findalldevs_ex
  :: F.Ptr FC.CChar
     {- ^ __from C:__ @source@ -}
  -> F.Ptr Pcap_rmtauth
     {- ^ __from C:__ @auth@ -}
  -> F.Ptr (F.Ptr Pcap_if_t)
     {- ^ __from C:__ @alldevs@ -}
  -> F.Ptr FC.CChar
     {- ^ __from C:__ @errbuf@ -}
  -> IO FC.CInt

pCAP_SAMP_1_EVERY_N :: FC.CInt
pCAP_SAMP_1_EVERY_N = (1 :: FC.CInt)

pCAP_SAMP_FIRST_AFTER_N_MS :: FC.CInt
pCAP_SAMP_FIRST_AFTER_N_MS = (2 :: FC.CInt)

data Pcap_samp = Pcap_samp
  { pcap_samp_method :: FC.CInt
  , pcap_samp_value :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable Pcap_samp where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Pcap_samp
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Pcap_samp pcap_samp_method2 pcap_samp_value3 ->
               F.pokeByteOff ptr0 (0 :: Int) pcap_samp_method2
            >> F.pokeByteOff ptr0 (4 :: Int) pcap_samp_value3

{-| __from C:__ @pcap_setsampling@ -}
foreign import ccall safe "hs_bindgen_842a3be6d3b00873" pcap_setsampling
  :: F.Ptr Pcap_t
     {- ^ __from C:__ @p@ -}
  -> IO (F.Ptr Pcap_samp)

rPCAP_HOSTLIST_SIZE :: FC.CInt
rPCAP_HOSTLIST_SIZE = (1024 :: FC.CInt)

{-| __from C:__ @pcap_remoteact_accept@ -}
foreign import ccall safe "hs_bindgen_d8464231f68f7e7e" pcap_remoteact_accept
  :: F.Ptr FC.CChar
     {- ^ __from C:__ @address@ -}
  -> F.Ptr FC.CChar
     {- ^ __from C:__ @port@ -}
  -> F.Ptr FC.CChar
     {- ^ __from C:__ @hostlist@ -}
  -> F.Ptr FC.CChar
     {- ^ __from C:__ @connectinghost@ -}
  -> F.Ptr Pcap_rmtauth
     {- ^ __from C:__ @auth@ -}
  -> F.Ptr FC.CChar
     {- ^ __from C:__ @errbuf@ -}
  -> IO FC.CInt

{-| __from C:__ @pcap_remoteact_accept_ex@ -}
foreign import ccall safe "hs_bindgen_b26cbce58c2eefaa" pcap_remoteact_accept_ex
  :: F.Ptr FC.CChar
     {- ^ __from C:__ @address@ -}
  -> F.Ptr FC.CChar
     {- ^ __from C:__ @port@ -}
  -> F.Ptr FC.CChar
     {- ^ __from C:__ @hostlist@ -}
  -> F.Ptr FC.CChar
     {- ^ __from C:__ @connectinghost@ -}
  -> F.Ptr Pcap_rmtauth
     {- ^ __from C:__ @auth@ -}
  -> FC.CInt
     {- ^ __from C:__ @uses_ssl@ -}
  -> F.Ptr FC.CChar
     {- ^ __from C:__ @errbuf@ -}
  -> IO FC.CInt

{-| __from C:__ @pcap_remoteact_list@ -}
foreign import ccall safe "hs_bindgen_aafbcc994f05a0c6" pcap_remoteact_list
  :: F.Ptr FC.CChar
     {- ^ __from C:__ @hostlist@ -}
  -> FC.CChar
     {- ^ __from C:__ @sep@ -}
  -> FC.CInt
     {- ^ __from C:__ @size@ -}
  -> F.Ptr FC.CChar
     {- ^ __from C:__ @errbuf@ -}
  -> IO FC.CInt

{-| __from C:__ @pcap_remoteact_close@ -}
foreign import ccall safe "hs_bindgen_d6e6b4d7523eb07b" pcap_remoteact_close
  :: F.Ptr FC.CChar
     {- ^ __from C:__ @host@ -}
  -> F.Ptr FC.CChar
     {- ^ __from C:__ @errbuf@ -}
  -> IO FC.CInt

{-| __from C:__ @pcap_remoteact_cleanup@ -}
foreign import ccall safe "hs_bindgen_722fd158dbfe4678" pcap_remoteact_cleanup
  :: IO ()
