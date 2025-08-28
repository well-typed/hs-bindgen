{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Generated.Pcap where

import qualified C.Expr.HostPlatform as C
import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty
import Data.Void (Void)
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.Prelude
import Prelude ((<*>), (>>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, showsPrec)
import qualified Text.Read

$(CAPI.addCSource "#include <pcap.h>\nsigned int hs_bindgen_5a74b0412e8f8c8c (unsigned int arg1, char *arg2) { return pcap_init(arg1, arg2); }\n/* get_pcap_init_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_c9f9e9c9e4f8892d (void)) (unsigned int arg1, char *arg2) { return &pcap_init; } \nsigned int hs_bindgen_dd0412a256181b94 (char const *arg1, bpf_u_int32 *arg2, bpf_u_int32 *arg3, char *arg4) { return pcap_lookupnet(arg1, arg2, arg3, arg4); }\n/* get_pcap_lookupnet_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_7d836eead1796585 (void)) (char const *arg1, bpf_u_int32 *arg2, bpf_u_int32 *arg3, char *arg4) { return &pcap_lookupnet; } \npcap_t *hs_bindgen_c67404749d715e0f (char const *arg1, char *arg2) { return pcap_create(arg1, arg2); }\n/* get_pcap_create_ptr */ __attribute__ ((const)) pcap_t *(*hs_bindgen_b4a4a216f0fbdba5 (void)) (char const *arg1, char *arg2) { return &pcap_create; } \nsigned int hs_bindgen_899f506320c671ee (pcap_t *arg1, signed int arg2) { return pcap_set_snaplen(arg1, arg2); }\n/* get_pcap_set_snaplen_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_88236eb94df0f75c (void)) (pcap_t *arg1, signed int arg2) { return &pcap_set_snaplen; } \nsigned int hs_bindgen_0129cbee2211ca4c (pcap_t *arg1, signed int arg2) { return pcap_set_promisc(arg1, arg2); }\n/* get_pcap_set_promisc_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_b3d4fa8f98e045d9 (void)) (pcap_t *arg1, signed int arg2) { return &pcap_set_promisc; } \nsigned int hs_bindgen_a8c2eaf79b0eed7b (pcap_t *arg1) { return pcap_can_set_rfmon(arg1); }\n/* get_pcap_can_set_rfmon_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_66a4060db5c6a44e (void)) (pcap_t *arg1) { return &pcap_can_set_rfmon; } \nsigned int hs_bindgen_7b8f0fc59a2bd7ef (pcap_t *arg1, signed int arg2) { return pcap_set_rfmon(arg1, arg2); }\n/* get_pcap_set_rfmon_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_71a0b736624c8c6a (void)) (pcap_t *arg1, signed int arg2) { return &pcap_set_rfmon; } \nsigned int hs_bindgen_49ffc77ef9b4faae (pcap_t *arg1, signed int arg2) { return pcap_set_timeout(arg1, arg2); }\n/* get_pcap_set_timeout_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_983c6131b48b1059 (void)) (pcap_t *arg1, signed int arg2) { return &pcap_set_timeout; } \nsigned int hs_bindgen_305b3c1ba436bff5 (pcap_t *arg1, signed int arg2) { return pcap_set_tstamp_type(arg1, arg2); }\n/* get_pcap_set_tstamp_type_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_2ceae335926feccd (void)) (pcap_t *arg1, signed int arg2) { return &pcap_set_tstamp_type; } \nsigned int hs_bindgen_e69c67afe679fa14 (pcap_t *arg1, signed int arg2) { return pcap_set_immediate_mode(arg1, arg2); }\n/* get_pcap_set_immediate_mode_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_90f1071272b80b48 (void)) (pcap_t *arg1, signed int arg2) { return &pcap_set_immediate_mode; } \nsigned int hs_bindgen_729b652c3f9e3786 (pcap_t *arg1, signed int arg2) { return pcap_set_buffer_size(arg1, arg2); }\n/* get_pcap_set_buffer_size_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_256a4f684a37f304 (void)) (pcap_t *arg1, signed int arg2) { return &pcap_set_buffer_size; } \nsigned int hs_bindgen_67e5b4f07de57ed3 (pcap_t *arg1, signed int arg2) { return pcap_set_tstamp_precision(arg1, arg2); }\n/* get_pcap_set_tstamp_precision_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_b6796c07f0083fb2 (void)) (pcap_t *arg1, signed int arg2) { return &pcap_set_tstamp_precision; } \nsigned int hs_bindgen_e160411dc9acb593 (pcap_t *arg1) { return pcap_get_tstamp_precision(arg1); }\n/* get_pcap_get_tstamp_precision_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_40c019e03991b952 (void)) (pcap_t *arg1) { return &pcap_get_tstamp_precision; } \nsigned int hs_bindgen_ca4ffae84de4ef9c (pcap_t *arg1) { return pcap_activate(arg1); }\n/* get_pcap_activate_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_c14f86ce5e8ca2b9 (void)) (pcap_t *arg1) { return &pcap_activate; } \nsigned int hs_bindgen_a5d9732ca735493a (pcap_t *arg1, signed int **arg2) { return pcap_list_tstamp_types(arg1, arg2); }\n/* get_pcap_list_tstamp_types_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_112a71c30aef74b7 (void)) (pcap_t *arg1, signed int **arg2) { return &pcap_list_tstamp_types; } \nvoid hs_bindgen_80c4798a4663bce0 (signed int *arg1) { pcap_free_tstamp_types(arg1); }\n/* get_pcap_free_tstamp_types_ptr */ __attribute__ ((const)) void (*hs_bindgen_6049744ecd937e78 (void)) (signed int *arg1) { return &pcap_free_tstamp_types; } \nsigned int hs_bindgen_1b88c6740aa68840 (char const *arg1) { return pcap_tstamp_type_name_to_val(arg1); }\n/* get_pcap_tstamp_type_name_to_val_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_8cd46287f05ed2c9 (void)) (char const *arg1) { return &pcap_tstamp_type_name_to_val; } \nchar const *hs_bindgen_dbb311000616a945 (signed int arg1) { return pcap_tstamp_type_val_to_name(arg1); }\n/* get_pcap_tstamp_type_val_to_name_ptr */ __attribute__ ((const)) char const *(*hs_bindgen_3abd8556be46d8d1 (void)) (signed int arg1) { return &pcap_tstamp_type_val_to_name; } \nchar const *hs_bindgen_a2c55f5ff16c095f (signed int arg1) { return pcap_tstamp_type_val_to_description(arg1); }\n/* get_pcap_tstamp_type_val_to_description_ptr */ __attribute__ ((const)) char const *(*hs_bindgen_ff9d6e66b3a16bd8 (void)) (signed int arg1) { return &pcap_tstamp_type_val_to_description; } \nsigned int hs_bindgen_183f6ad6ac6deac6 (pcap_t *arg1, signed int arg2) { return pcap_set_protocol_linux(arg1, arg2); }\n/* get_pcap_set_protocol_linux_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_616654327e926a08 (void)) (pcap_t *arg1, signed int arg2) { return &pcap_set_protocol_linux; } \npcap_t *hs_bindgen_73c31b44a2469172 (FILE *arg1, u_int arg2, char *arg3) { return pcap_fopen_offline_with_tstamp_precision(arg1, arg2, arg3); }\n/* get_pcap_fopen_offline_with_tstamp_precision_ptr */ __attribute__ ((const)) pcap_t *(*hs_bindgen_983878c28d277db8 (void)) (FILE *arg1, u_int arg2, char *arg3) { return &pcap_fopen_offline_with_tstamp_precision; } \npcap_t *hs_bindgen_c11d352c68ff9eaf (FILE *arg1, char *arg2) { return pcap_fopen_offline(arg1, arg2); }\n/* get_pcap_fopen_offline_ptr */ __attribute__ ((const)) pcap_t *(*hs_bindgen_a24d511fb8c0ae8e (void)) (FILE *arg1, char *arg2) { return &pcap_fopen_offline; } \nvoid hs_bindgen_a00c19cb75a4068e (pcap_t *arg1) { pcap_close(arg1); }\n/* get_pcap_close_ptr */ __attribute__ ((const)) void (*hs_bindgen_03b9258ebb5aab3a (void)) (pcap_t *arg1) { return &pcap_close; } \nsigned int hs_bindgen_f721a2623c4e6b72 (pcap_t *arg1, signed int arg2, pcap_handler arg3, u_char *arg4) { return pcap_loop(arg1, arg2, arg3, arg4); }\n/* get_pcap_loop_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_98b43666decaee1e (void)) (pcap_t *arg1, signed int arg2, pcap_handler arg3, u_char *arg4) { return &pcap_loop; } \nsigned int hs_bindgen_31243165d72e1fc1 (pcap_t *arg1, signed int arg2, pcap_handler arg3, u_char *arg4) { return pcap_dispatch(arg1, arg2, arg3, arg4); }\n/* get_pcap_dispatch_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_c5ae44d99e7e4b73 (void)) (pcap_t *arg1, signed int arg2, pcap_handler arg3, u_char *arg4) { return &pcap_dispatch; } \nu_char const *hs_bindgen_b2f81d61b38cbbea (pcap_t *arg1, struct pcap_pkthdr *arg2) { return pcap_next(arg1, arg2); }\n/* get_pcap_next_ptr */ __attribute__ ((const)) u_char const *(*hs_bindgen_0b42e18b7963cd98 (void)) (pcap_t *arg1, struct pcap_pkthdr *arg2) { return &pcap_next; } \nsigned int hs_bindgen_9bfbdc8d5b6faeb4 (pcap_t *arg1, struct pcap_pkthdr **arg2, u_char const **arg3) { return pcap_next_ex(arg1, arg2, arg3); }\n/* get_pcap_next_ex_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_d16b7e5def9aaf08 (void)) (pcap_t *arg1, struct pcap_pkthdr **arg2, u_char const **arg3) { return &pcap_next_ex; } \nvoid hs_bindgen_8be607287be591ca (pcap_t *arg1) { pcap_breakloop(arg1); }\n/* get_pcap_breakloop_ptr */ __attribute__ ((const)) void (*hs_bindgen_3ff2347bf458f166 (void)) (pcap_t *arg1) { return &pcap_breakloop; } \nsigned int hs_bindgen_8b85c22d16b68b74 (pcap_t *arg1, struct pcap_stat *arg2) { return pcap_stats(arg1, arg2); }\n/* get_pcap_stats_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_57436a2f7dd3f348 (void)) (pcap_t *arg1, struct pcap_stat *arg2) { return &pcap_stats; } \nsigned int hs_bindgen_8dd18013405d45c4 (pcap_t *arg1, struct bpf_program *arg2) { return pcap_setfilter(arg1, arg2); }\n/* get_pcap_setfilter_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_e584b88cdcd8cef3 (void)) (pcap_t *arg1, struct bpf_program *arg2) { return &pcap_setfilter; } \nsigned int hs_bindgen_a89a45dd24ab274a (pcap_t *arg1, pcap_direction_t arg2) { return pcap_setdirection(arg1, arg2); }\n/* get_pcap_setdirection_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_9947109428276b0a (void)) (pcap_t *arg1, pcap_direction_t arg2) { return &pcap_setdirection; } \nsigned int hs_bindgen_448afe8cd2792dfc (pcap_t *arg1, char *arg2) { return pcap_getnonblock(arg1, arg2); }\n/* get_pcap_getnonblock_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_df5d01df419a5d0c (void)) (pcap_t *arg1, char *arg2) { return &pcap_getnonblock; } \nsigned int hs_bindgen_ae2fd5189870569a (pcap_t *arg1, signed int arg2, char *arg3) { return pcap_setnonblock(arg1, arg2, arg3); }\n/* get_pcap_setnonblock_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_7da24a20470a8056 (void)) (pcap_t *arg1, signed int arg2, char *arg3) { return &pcap_setnonblock; } \nsigned int hs_bindgen_5909d70f9932fc67 (pcap_t *arg1, void const *arg2, size_t arg3) { return pcap_inject(arg1, arg2, arg3); }\n/* get_pcap_inject_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_ddd66b69cc87ba56 (void)) (pcap_t *arg1, void const *arg2, size_t arg3) { return &pcap_inject; } \nsigned int hs_bindgen_ea9a87f52c0bb98a (pcap_t *arg1, u_char const *arg2, signed int arg3) { return pcap_sendpacket(arg1, arg2, arg3); }\n/* get_pcap_sendpacket_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_d12a0775f081904e (void)) (pcap_t *arg1, u_char const *arg2, signed int arg3) { return &pcap_sendpacket; } \nchar const *hs_bindgen_92b0c0c43c37ffba (signed int arg1) { return pcap_statustostr(arg1); }\n/* get_pcap_statustostr_ptr */ __attribute__ ((const)) char const *(*hs_bindgen_c0bb64ef3e24ca49 (void)) (signed int arg1) { return &pcap_statustostr; } \nchar const *hs_bindgen_ee0b68336360b03f (signed int arg1) { return pcap_strerror(arg1); }\n/* get_pcap_strerror_ptr */ __attribute__ ((const)) char const *(*hs_bindgen_172e573733c559fa (void)) (signed int arg1) { return &pcap_strerror; } \nchar *hs_bindgen_a32beca7c8760813 (pcap_t *arg1) { return pcap_geterr(arg1); }\n/* get_pcap_geterr_ptr */ __attribute__ ((const)) char *(*hs_bindgen_81791e56d8b1fefb (void)) (pcap_t *arg1) { return &pcap_geterr; } \nvoid hs_bindgen_2208cc248d0f8d87 (pcap_t *arg1, char const *arg2) { pcap_perror(arg1, arg2); }\n/* get_pcap_perror_ptr */ __attribute__ ((const)) void (*hs_bindgen_309f2a9e5d17c18c (void)) (pcap_t *arg1, char const *arg2) { return &pcap_perror; } \nsigned int hs_bindgen_6b34a8490102b9a7 (pcap_t *arg1, struct bpf_program *arg2, char const *arg3, signed int arg4, bpf_u_int32 arg5) { return pcap_compile(arg1, arg2, arg3, arg4, arg5); }\n/* get_pcap_compile_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_63a79dc54502c80d (void)) (pcap_t *arg1, struct bpf_program *arg2, char const *arg3, signed int arg4, bpf_u_int32 arg5) { return &pcap_compile; } \nvoid hs_bindgen_e833df3c5ebc9855 (struct bpf_program *arg1) { pcap_freecode(arg1); }\n/* get_pcap_freecode_ptr */ __attribute__ ((const)) void (*hs_bindgen_fdf71a125e627818 (void)) (struct bpf_program *arg1) { return &pcap_freecode; } \nsigned int hs_bindgen_ef995d1dca55f24e (struct bpf_program const *arg1, struct pcap_pkthdr const *arg2, u_char const *arg3) { return pcap_offline_filter(arg1, arg2, arg3); }\n/* get_pcap_offline_filter_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_3a072b87bf68590b (void)) (struct bpf_program const *arg1, struct pcap_pkthdr const *arg2, u_char const *arg3) { return &pcap_offline_filter; } \nsigned int hs_bindgen_2691de33c9de0eac (pcap_t *arg1) { return pcap_datalink(arg1); }\n/* get_pcap_datalink_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_cdd86d767366ccb6 (void)) (pcap_t *arg1) { return &pcap_datalink; } \nsigned int hs_bindgen_7c355b0a96f34fba (pcap_t *arg1) { return pcap_datalink_ext(arg1); }\n/* get_pcap_datalink_ext_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_c48e00b1d1a53873 (void)) (pcap_t *arg1) { return &pcap_datalink_ext; } \nsigned int hs_bindgen_18cf0ac774b799e6 (pcap_t *arg1, signed int **arg2) { return pcap_list_datalinks(arg1, arg2); }\n/* get_pcap_list_datalinks_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_baba6950f2676f33 (void)) (pcap_t *arg1, signed int **arg2) { return &pcap_list_datalinks; } \nsigned int hs_bindgen_caff20c615fbaeea (pcap_t *arg1, signed int arg2) { return pcap_set_datalink(arg1, arg2); }\n/* get_pcap_set_datalink_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_cb33ddf9d0e412f3 (void)) (pcap_t *arg1, signed int arg2) { return &pcap_set_datalink; } \nvoid hs_bindgen_919509339b068fa5 (signed int *arg1) { pcap_free_datalinks(arg1); }\n/* get_pcap_free_datalinks_ptr */ __attribute__ ((const)) void (*hs_bindgen_085e11dc4c8d3c50 (void)) (signed int *arg1) { return &pcap_free_datalinks; } \nsigned int hs_bindgen_dee608d79c9e03f4 (char const *arg1) { return pcap_datalink_name_to_val(arg1); }\n/* get_pcap_datalink_name_to_val_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_0cb538c6a717e1a1 (void)) (char const *arg1) { return &pcap_datalink_name_to_val; } \nchar const *hs_bindgen_b28a5e20602167c9 (signed int arg1) { return pcap_datalink_val_to_name(arg1); }\n/* get_pcap_datalink_val_to_name_ptr */ __attribute__ ((const)) char const *(*hs_bindgen_9bd0bd37dc9a5a12 (void)) (signed int arg1) { return &pcap_datalink_val_to_name; } \nchar const *hs_bindgen_955733491140d435 (signed int arg1) { return pcap_datalink_val_to_description(arg1); }\n/* get_pcap_datalink_val_to_description_ptr */ __attribute__ ((const)) char const *(*hs_bindgen_ac477b87d0044147 (void)) (signed int arg1) { return &pcap_datalink_val_to_description; } \nchar const *hs_bindgen_6dc343be469a9e3e (signed int arg1) { return pcap_datalink_val_to_description_or_dlt(arg1); }\n/* get_pcap_datalink_val_to_description_or_dlt_ptr */ __attribute__ ((const)) char const *(*hs_bindgen_cb775158ce004f8a (void)) (signed int arg1) { return &pcap_datalink_val_to_description_or_dlt; } \nsigned int hs_bindgen_c26318f47f502103 (pcap_t *arg1) { return pcap_snapshot(arg1); }\n/* get_pcap_snapshot_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_383e73406aa189ab (void)) (pcap_t *arg1) { return &pcap_snapshot; } \nsigned int hs_bindgen_9138a5d27c5ed968 (pcap_t *arg1) { return pcap_is_swapped(arg1); }\n/* get_pcap_is_swapped_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_3c88b8a57bc4aa03 (void)) (pcap_t *arg1) { return &pcap_is_swapped; } \nsigned int hs_bindgen_4b895511c31cf718 (pcap_t *arg1) { return pcap_major_version(arg1); }\n/* get_pcap_major_version_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_79a5564d5c6d6808 (void)) (pcap_t *arg1) { return &pcap_major_version; } \nsigned int hs_bindgen_5454bab4d77c4e08 (pcap_t *arg1) { return pcap_minor_version(arg1); }\n/* get_pcap_minor_version_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_8f3c0c9f62bf5ca6 (void)) (pcap_t *arg1) { return &pcap_minor_version; } \nsigned int hs_bindgen_29137dffca5b71d2 (pcap_t *arg1) { return pcap_bufsize(arg1); }\n/* get_pcap_bufsize_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_3a2cafa5203f6a27 (void)) (pcap_t *arg1) { return &pcap_bufsize; } \nFILE *hs_bindgen_95d1f46f0cd2b89a (pcap_t *arg1) { return pcap_file(arg1); }\n/* get_pcap_file_ptr */ __attribute__ ((const)) FILE *(*hs_bindgen_6102cb9744b09841 (void)) (pcap_t *arg1) { return &pcap_file; } \nsigned int hs_bindgen_e4ec82d954c0cda3 (pcap_t *arg1) { return pcap_fileno(arg1); }\n/* get_pcap_fileno_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_4a8e9c546e7e739c (void)) (pcap_t *arg1) { return &pcap_fileno; } \npcap_dumper_t *hs_bindgen_679f873b0c503d7e (pcap_t *arg1, char const *arg2) { return pcap_dump_open(arg1, arg2); }\n/* get_pcap_dump_open_ptr */ __attribute__ ((const)) pcap_dumper_t *(*hs_bindgen_ce88e3dd67a87f4e (void)) (pcap_t *arg1, char const *arg2) { return &pcap_dump_open; } \npcap_dumper_t *hs_bindgen_2c61c28b1a0dcab0 (pcap_t *arg1, FILE *arg2) { return pcap_dump_fopen(arg1, arg2); }\n/* get_pcap_dump_fopen_ptr */ __attribute__ ((const)) pcap_dumper_t *(*hs_bindgen_56b877cb62ae0119 (void)) (pcap_t *arg1, FILE *arg2) { return &pcap_dump_fopen; } \npcap_dumper_t *hs_bindgen_d7bf02b04a26700b (pcap_t *arg1, char const *arg2) { return pcap_dump_open_append(arg1, arg2); }\n/* get_pcap_dump_open_append_ptr */ __attribute__ ((const)) pcap_dumper_t *(*hs_bindgen_c6d9e001ffcc5db2 (void)) (pcap_t *arg1, char const *arg2) { return &pcap_dump_open_append; } \nFILE *hs_bindgen_b3f87367e0d0a8c1 (pcap_dumper_t *arg1) { return pcap_dump_file(arg1); }\n/* get_pcap_dump_file_ptr */ __attribute__ ((const)) FILE *(*hs_bindgen_d6e5cd781d179052 (void)) (pcap_dumper_t *arg1) { return &pcap_dump_file; } \nsigned long hs_bindgen_d4e39b27a7f4faae (pcap_dumper_t *arg1) { return pcap_dump_ftell(arg1); }\n/* get_pcap_dump_ftell_ptr */ __attribute__ ((const)) signed long (*hs_bindgen_acc6be05c7d92fdd (void)) (pcap_dumper_t *arg1) { return &pcap_dump_ftell; } \nint64_t hs_bindgen_0596fdb074f580de (pcap_dumper_t *arg1) { return pcap_dump_ftell64(arg1); }\n/* get_pcap_dump_ftell64_ptr */ __attribute__ ((const)) int64_t (*hs_bindgen_dd07417aca5bb8e5 (void)) (pcap_dumper_t *arg1) { return &pcap_dump_ftell64; } \nsigned int hs_bindgen_ce95212b95850737 (pcap_dumper_t *arg1) { return pcap_dump_flush(arg1); }\n/* get_pcap_dump_flush_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_1db9ebaec193f507 (void)) (pcap_dumper_t *arg1) { return &pcap_dump_flush; } \nvoid hs_bindgen_8737bfddc526b3d6 (pcap_dumper_t *arg1) { pcap_dump_close(arg1); }\n/* get_pcap_dump_close_ptr */ __attribute__ ((const)) void (*hs_bindgen_c68e8746b85b3e95 (void)) (pcap_dumper_t *arg1) { return &pcap_dump_close; } \nvoid hs_bindgen_5f2d744bd850aa8c (u_char *arg1, struct pcap_pkthdr const *arg2, u_char const *arg3) { pcap_dump(arg1, arg2, arg3); }\n/* get_pcap_dump_ptr */ __attribute__ ((const)) void (*hs_bindgen_1c463311f00b98d4 (void)) (u_char *arg1, struct pcap_pkthdr const *arg2, u_char const *arg3) { return &pcap_dump; } \nsigned int hs_bindgen_0c97f860802cd1b6 (pcap_if_t **arg1, char *arg2) { return pcap_findalldevs(arg1, arg2); }\n/* get_pcap_findalldevs_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_b8927abb07d90d4e (void)) (pcap_if_t **arg1, char *arg2) { return &pcap_findalldevs; } \nvoid hs_bindgen_93cdcd7040db1082 (pcap_if_t *arg1) { pcap_freealldevs(arg1); }\n/* get_pcap_freealldevs_ptr */ __attribute__ ((const)) void (*hs_bindgen_22ffe8543386e2bc (void)) (pcap_if_t *arg1) { return &pcap_freealldevs; } \nchar const *hs_bindgen_d867d6999ff695c0 (void) { return pcap_lib_version(); }\n/* get_pcap_lib_version_ptr */ __attribute__ ((const)) char const *(*hs_bindgen_0236695bf43b2f36 (void)) (void) { return &pcap_lib_version; } \nsigned int hs_bindgen_9c54b5c23c22c8bb (pcap_t *arg1) { return pcap_get_selectable_fd(arg1); }\n/* get_pcap_get_selectable_fd_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_3623f6e8d90e160a (void)) (pcap_t *arg1) { return &pcap_get_selectable_fd; } \nstruct timeval const *hs_bindgen_a1534f381d527aa1 (pcap_t *arg1) { return pcap_get_required_select_timeout(arg1); }\n/* get_pcap_get_required_select_timeout_ptr */ __attribute__ ((const)) struct timeval const *(*hs_bindgen_529cdf3e96af4a58 (void)) (pcap_t *arg1) { return &pcap_get_required_select_timeout; } \n")

{-| __C declaration:__ @sa_family_t@

    __defined at:__ @sockaddr.h:28:28@

    __exported by:__ @pcap.h@
-}
newtype Sa_family_t = Sa_family_t
  { un_Sa_family_t :: FC.CUShort
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @__u_char@

    __defined at:__ @types.h:31:23@

    __exported by:__ @pcap.h@
-}
newtype C__U_char = C__U_char
  { un_C__U_char :: FC.CUChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @__u_short@

    __defined at:__ @types.h:32:28@

    __exported by:__ @pcap.h@
-}
newtype C__U_short = C__U_short
  { un_C__U_short :: FC.CUShort
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @__u_int@

    __defined at:__ @types.h:33:22@

    __exported by:__ @pcap.h@
-}
newtype C__U_int = C__U_int
  { un_C__U_int :: FC.CUInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @__time_t@

    __defined at:__ @types.h:160:26@

    __exported by:__ @pcap.h@
-}
newtype C__Time_t = C__Time_t
  { un_C__Time_t :: FC.CLong
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @__suseconds_t@

    __defined at:__ @types.h:162:31@

    __exported by:__ @pcap.h@
-}
newtype C__Suseconds_t = C__Suseconds_t
  { un_C__Suseconds_t :: FC.CLong
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @timeval@

    __defined at:__ @struct_timeval.h:8:8@

    __exported by:__ @pcap.h@
-}
data Timeval = Timeval
  { timeval_tv_sec :: C__Time_t
    {- ^ __C declaration:__ @tv_sec@

         __defined at:__ @struct_timeval.h:14:12@

         __exported by:__ @pcap.h@
    -}
  , timeval_tv_usec :: C__Suseconds_t
    {- ^ __C declaration:__ @tv_usec@

         __defined at:__ @struct_timeval.h:15:17@

         __exported by:__ @pcap.h@
    -}
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

{-| __C declaration:__ @u_char@

    __defined at:__ @types.h:33:18@

    __exported by:__ @pcap.h@
-}
newtype U_char = U_char
  { un_U_char :: C__U_char
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @u_short@

    __defined at:__ @types.h:34:19@

    __exported by:__ @pcap.h@
-}
newtype U_short = U_short
  { un_U_short :: C__U_short
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @u_int@

    __defined at:__ @types.h:35:17@

    __exported by:__ @pcap.h@
-}
newtype U_int = U_int
  { un_U_int :: C__U_int
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @sockaddr@

    __defined at:__ @socket.h:184:39@

    __exported by:__ @pcap.h@
-}
data Sockaddr = Sockaddr
  { sockaddr_sa_family :: Sa_family_t
    {- ^ __C declaration:__ @sa_family@

         __defined at:__ @socket.h:186:5@

         __exported by:__ @pcap.h@
    -}
  , sockaddr_sa_data :: (HsBindgen.Runtime.ConstantArray.ConstantArray 14) FC.CChar
    {- ^ __C declaration:__ @sa_data@

         __defined at:__ @socket.h:187:10@

         __exported by:__ @pcap.h@
    -}
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

{-| __C declaration:__ @bpf_int32@

    __defined at:__ @bpf.h:96:13@

    __exported by:__ @pcap.h@
-}
newtype Bpf_int32 = Bpf_int32
  { un_Bpf_int32 :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @bpf_u_int32@

    __defined at:__ @bpf.h:97:15@

    __exported by:__ @pcap.h@
-}
newtype Bpf_u_int32 = Bpf_u_int32
  { un_Bpf_u_int32 :: U_int
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @bpf_insn@

    __defined at:__ @bpf.h:244:8@

    __exported by:__ @pcap.h@
-}
data Bpf_insn = Bpf_insn
  { bpf_insn_code :: U_short
    {- ^ __C declaration:__ @code@

         __defined at:__ @bpf.h:245:10@

         __exported by:__ @pcap.h@
    -}
  , bpf_insn_jt :: U_char
    {- ^ __C declaration:__ @jt@

         __defined at:__ @bpf.h:246:9@

         __exported by:__ @pcap.h@
    -}
  , bpf_insn_jf :: U_char
    {- ^ __C declaration:__ @jf@

         __defined at:__ @bpf.h:247:9@

         __exported by:__ @pcap.h@
    -}
  , bpf_insn_k :: Bpf_u_int32
    {- ^ __C declaration:__ @k@

         __defined at:__ @bpf.h:248:14@

         __exported by:__ @pcap.h@
    -}
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

{-| __C declaration:__ @bpf_program@

    __defined at:__ @bpf.h:116:8@

    __exported by:__ @pcap.h@
-}
data Bpf_program = Bpf_program
  { bpf_program_bf_len :: U_int
    {- ^ __C declaration:__ @bf_len@

         __defined at:__ @bpf.h:117:8@

         __exported by:__ @pcap.h@
    -}
  , bpf_program_bf_insns :: Ptr.Ptr Bpf_insn
    {- ^ __C declaration:__ @bf_insns@

         __defined at:__ @bpf.h:118:19@

         __exported by:__ @pcap.h@
    -}
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

{-| __C declaration:__ @PCAP_VERSION_MAJOR@

    __defined at:__ @pcap.h:149:9@

    __exported by:__ @pcap.h@
-}
pCAP_VERSION_MAJOR :: FC.CInt
pCAP_VERSION_MAJOR = (2 :: FC.CInt)

{-| __C declaration:__ @PCAP_VERSION_MINOR@

    __defined at:__ @pcap.h:150:9@

    __exported by:__ @pcap.h@
-}
pCAP_VERSION_MINOR :: FC.CInt
pCAP_VERSION_MINOR = (4 :: FC.CInt)

{-| __C declaration:__ @PCAP_ERRBUF_SIZE@

    __defined at:__ @pcap.h:152:9@

    __exported by:__ @pcap.h@
-}
pCAP_ERRBUF_SIZE :: FC.CInt
pCAP_ERRBUF_SIZE = (256 :: FC.CInt)

{-| __C declaration:__ @pcap_t@

    __defined at:__ @pcap.h:163:16@

    __exported by:__ @pcap.h@
-}
data Pcap_t

{-| __C declaration:__ @pcap_dumper_t@

    __defined at:__ @pcap.h:164:16@

    __exported by:__ @pcap.h@
-}
data Pcap_dumper_t

{-| __C declaration:__ @pcap_addr@

    __defined at:__ @pcap.h:370:8@

    __exported by:__ @pcap.h@
-}
data Pcap_addr = Pcap_addr
  { pcap_addr_next :: Ptr.Ptr Pcap_addr
    {- ^ __C declaration:__ @next@

         __defined at:__ @pcap.h:371:20@

         __exported by:__ @pcap.h@
    -}
  , pcap_addr_addr :: Ptr.Ptr Sockaddr
    {- ^ __C declaration:__ @addr@

         __defined at:__ @pcap.h:372:19@

         __exported by:__ @pcap.h@
    -}
  , pcap_addr_netmask :: Ptr.Ptr Sockaddr
    {- ^ __C declaration:__ @netmask@

         __defined at:__ @pcap.h:373:19@

         __exported by:__ @pcap.h@
    -}
  , pcap_addr_broadaddr :: Ptr.Ptr Sockaddr
    {- ^ __C declaration:__ @broadaddr@

         __defined at:__ @pcap.h:374:19@

         __exported by:__ @pcap.h@
    -}
  , pcap_addr_dstaddr :: Ptr.Ptr Sockaddr
    {- ^ __C declaration:__ @dstaddr@

         __defined at:__ @pcap.h:375:19@

         __exported by:__ @pcap.h@
    -}
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

{-| __C declaration:__ @pcap_if_t@

    __defined at:__ @pcap.h:349:8@

    __exported by:__ @pcap.h@
-}
data Pcap_if_t = Pcap_if_t
  { pcap_if_t_next :: Ptr.Ptr Pcap_if_t
    {- ^ __C declaration:__ @next@

         __defined at:__ @pcap.h:350:18@

         __exported by:__ @pcap.h@
    -}
  , pcap_if_t_name :: Ptr.Ptr FC.CChar
    {- ^ __C declaration:__ @name@

         __defined at:__ @pcap.h:351:8@

         __exported by:__ @pcap.h@
    -}
  , pcap_if_t_description :: Ptr.Ptr FC.CChar
    {- ^ __C declaration:__ @description@

         __defined at:__ @pcap.h:352:8@

         __exported by:__ @pcap.h@
    -}
  , pcap_if_t_addresses :: Ptr.Ptr Pcap_addr
    {- ^ __C declaration:__ @addresses@

         __defined at:__ @pcap.h:353:20@

         __exported by:__ @pcap.h@
    -}
  , pcap_if_t_flags :: Bpf_u_int32
    {- ^ __C declaration:__ @flags@

         __defined at:__ @pcap.h:354:14@

         __exported by:__ @pcap.h@
    -}
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

{-| __C declaration:__ @pcap_addr_t@

    __defined at:__ @pcap.h:166:26@

    __exported by:__ @pcap.h@
-}
newtype Pcap_addr_t = Pcap_addr_t
  { un_Pcap_addr_t :: Pcap_addr
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @pcap_file_header@

    __defined at:__ @pcap.h:207:8@

    __exported by:__ @pcap.h@
-}
data Pcap_file_header = Pcap_file_header
  { pcap_file_header_magic :: Bpf_u_int32
    {- ^ __C declaration:__ @magic@

         __defined at:__ @pcap.h:208:14@

         __exported by:__ @pcap.h@
    -}
  , pcap_file_header_version_major :: U_short
    {- ^ __C declaration:__ @version_major@

         __defined at:__ @pcap.h:209:10@

         __exported by:__ @pcap.h@
    -}
  , pcap_file_header_version_minor :: U_short
    {- ^ __C declaration:__ @version_minor@

         __defined at:__ @pcap.h:210:10@

         __exported by:__ @pcap.h@
    -}
  , pcap_file_header_thiszone :: Bpf_int32
    {- ^ __C declaration:__ @thiszone@

         __defined at:__ @pcap.h:211:12@

         __exported by:__ @pcap.h@
    -}
  , pcap_file_header_sigfigs :: Bpf_u_int32
    {- ^ __C declaration:__ @sigfigs@

         __defined at:__ @pcap.h:212:14@

         __exported by:__ @pcap.h@
    -}
  , pcap_file_header_snaplen :: Bpf_u_int32
    {- ^ __C declaration:__ @snaplen@

         __defined at:__ @pcap.h:213:14@

         __exported by:__ @pcap.h@
    -}
  , pcap_file_header_linktype :: Bpf_u_int32
    {- ^ __C declaration:__ @linktype@

         __defined at:__ @pcap.h:214:14@

         __exported by:__ @pcap.h@
    -}
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

{-| __C declaration:__ @LT_LINKTYPE@

    __defined at:__ @pcap.h:268:9@

    __exported by:__ @pcap.h@
-}
lT_LINKTYPE :: forall a0. (C.Bitwise a0) FC.CInt => a0 -> (C.BitsRes a0) FC.CInt
lT_LINKTYPE = \x0 -> (C..&.) x0 (65535 :: FC.CInt)

{-| __C declaration:__ @LT_LINKTYPE_EXT@

    __defined at:__ @pcap.h:269:9@

    __exported by:__ @pcap.h@
-}
lT_LINKTYPE_EXT :: forall a0. (C.Bitwise a0) FC.CInt => a0 -> (C.BitsRes a0) FC.CInt
lT_LINKTYPE_EXT =
  \x0 -> (C..&.) x0 (4294901760 :: FC.CInt)

{-| __C declaration:__ @LT_RESERVED1@

    __defined at:__ @pcap.h:270:9@

    __exported by:__ @pcap.h@
-}
lT_RESERVED1 :: forall a0. (C.Bitwise a0) FC.CInt => a0 -> (C.BitsRes a0) FC.CInt
lT_RESERVED1 =
  \x0 -> (C..&.) x0 (67043328 :: FC.CInt)

{-| __C declaration:__ @LT_FCS_LENGTH_PRESENT@

    __defined at:__ @pcap.h:271:9@

    __exported by:__ @pcap.h@
-}
lT_FCS_LENGTH_PRESENT :: forall a0. (C.Bitwise a0) FC.CInt => a0 -> (C.BitsRes a0) FC.CInt
lT_FCS_LENGTH_PRESENT =
  \x0 -> (C..&.) x0 (67108864 :: FC.CInt)

{-| __C declaration:__ @LT_FCS_LENGTH@

    __defined at:__ @pcap.h:272:9@

    __exported by:__ @pcap.h@
-}
lT_FCS_LENGTH :: forall a0. (C.Bitwise a0) FC.CInt => (C.Shift ((C.BitsRes a0) FC.CInt)) FC.CInt => a0 -> C.ShiftRes ((C.BitsRes a0) FC.CInt)
lT_FCS_LENGTH =
  \x0 ->
    (C.>>) ((C..&.) x0 (4026531840 :: FC.CInt)) (28 :: FC.CInt)

{-| __C declaration:__ @LT_FCS_DATALINK_EXT@

    __defined at:__ @pcap.h:273:9@

    __exported by:__ @pcap.h@
-}
lT_FCS_DATALINK_EXT :: forall a0. (C.Bitwise a0) FC.CInt => (C.Bitwise (C.ShiftRes ((C.BitsRes a0) FC.CInt))) FC.CInt => (C.Shift ((C.BitsRes a0) FC.CInt)) FC.CInt => a0 -> (C.BitsRes (C.ShiftRes ((C.BitsRes a0) FC.CInt))) FC.CInt
lT_FCS_DATALINK_EXT =
  \x0 ->
    (C..|.) ((C.<<) ((C..&.) x0 (15 :: FC.CInt)) (28 :: FC.CInt)) (67108864 :: FC.CInt)

{-| __C declaration:__ @pcap_direction_t@

    __defined at:__ @pcap.h:275:9@

    __exported by:__ @pcap.h@
-}
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

{-| __C declaration:__ @PCAP_D_INOUT@

    __defined at:__ @pcap.h:276:8@

    __exported by:__ @pcap.h@
-}
pattern PCAP_D_INOUT :: Pcap_direction_t
pattern PCAP_D_INOUT = Pcap_direction_t 0

{-| __C declaration:__ @PCAP_D_IN@

    __defined at:__ @pcap.h:277:8@

    __exported by:__ @pcap.h@
-}
pattern PCAP_D_IN :: Pcap_direction_t
pattern PCAP_D_IN = Pcap_direction_t 1

{-| __C declaration:__ @PCAP_D_OUT@

    __defined at:__ @pcap.h:278:8@

    __exported by:__ @pcap.h@
-}
pattern PCAP_D_OUT :: Pcap_direction_t
pattern PCAP_D_OUT = Pcap_direction_t 2

{-| __C declaration:__ @pcap_pkthdr@

    __defined at:__ @pcap.h:293:8@

    __exported by:__ @pcap.h@
-}
data Pcap_pkthdr = Pcap_pkthdr
  { pcap_pkthdr_ts :: Timeval
    {- ^ __C declaration:__ @ts@

         __defined at:__ @pcap.h:294:17@

         __exported by:__ @pcap.h@
    -}
  , pcap_pkthdr_caplen :: Bpf_u_int32
    {- ^ __C declaration:__ @caplen@

         __defined at:__ @pcap.h:295:14@

         __exported by:__ @pcap.h@
    -}
  , pcap_pkthdr_len :: Bpf_u_int32
    {- ^ __C declaration:__ @len@

         __defined at:__ @pcap.h:296:14@

         __exported by:__ @pcap.h@
    -}
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

{-| __C declaration:__ @pcap_stat@

    __defined at:__ @pcap.h:302:8@

    __exported by:__ @pcap.h@
-}
data Pcap_stat = Pcap_stat
  { pcap_stat_ps_recv :: U_int
    {- ^ __C declaration:__ @ps_recv@

         __defined at:__ @pcap.h:303:8@

         __exported by:__ @pcap.h@
    -}
  , pcap_stat_ps_drop :: U_int
    {- ^ __C declaration:__ @ps_drop@

         __defined at:__ @pcap.h:304:8@

         __exported by:__ @pcap.h@
    -}
  , pcap_stat_ps_ifdrop :: U_int
    {- ^ __C declaration:__ @ps_ifdrop@

         __defined at:__ @pcap.h:305:8@

         __exported by:__ @pcap.h@
    -}
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

{-| __C declaration:__ @PCAP_IF_LOOPBACK@

    __defined at:__ @pcap.h:357:9@

    __exported by:__ @pcap.h@
-}
pCAP_IF_LOOPBACK :: FC.CInt
pCAP_IF_LOOPBACK = (1 :: FC.CInt)

{-| __C declaration:__ @PCAP_IF_UP@

    __defined at:__ @pcap.h:358:9@

    __exported by:__ @pcap.h@
-}
pCAP_IF_UP :: FC.CInt
pCAP_IF_UP = (2 :: FC.CInt)

{-| __C declaration:__ @PCAP_IF_RUNNING@

    __defined at:__ @pcap.h:359:9@

    __exported by:__ @pcap.h@
-}
pCAP_IF_RUNNING :: FC.CInt
pCAP_IF_RUNNING = (4 :: FC.CInt)

{-| __C declaration:__ @PCAP_IF_WIRELESS@

    __defined at:__ @pcap.h:360:9@

    __exported by:__ @pcap.h@
-}
pCAP_IF_WIRELESS :: FC.CInt
pCAP_IF_WIRELESS = (8 :: FC.CInt)

{-| __C declaration:__ @PCAP_IF_CONNECTION_STATUS@

    __defined at:__ @pcap.h:361:9@

    __exported by:__ @pcap.h@
-}
pCAP_IF_CONNECTION_STATUS :: FC.CInt
pCAP_IF_CONNECTION_STATUS = (48 :: FC.CInt)

{-| __C declaration:__ @PCAP_IF_CONNECTION_STATUS_UNKNOWN@

    __defined at:__ @pcap.h:362:9@

    __exported by:__ @pcap.h@
-}
pCAP_IF_CONNECTION_STATUS_UNKNOWN :: FC.CInt
pCAP_IF_CONNECTION_STATUS_UNKNOWN = (0 :: FC.CInt)

{-| __C declaration:__ @PCAP_IF_CONNECTION_STATUS_CONNECTED@

    __defined at:__ @pcap.h:363:9@

    __exported by:__ @pcap.h@
-}
pCAP_IF_CONNECTION_STATUS_CONNECTED :: FC.CInt
pCAP_IF_CONNECTION_STATUS_CONNECTED = (16 :: FC.CInt)

{-| __C declaration:__ @PCAP_IF_CONNECTION_STATUS_DISCONNECTED@

    __defined at:__ @pcap.h:364:9@

    __exported by:__ @pcap.h@
-}
pCAP_IF_CONNECTION_STATUS_DISCONNECTED :: FC.CInt
pCAP_IF_CONNECTION_STATUS_DISCONNECTED =
  (32 :: FC.CInt)

{-| __C declaration:__ @PCAP_IF_CONNECTION_STATUS_NOT_APPLICABLE@

    __defined at:__ @pcap.h:365:9@

    __exported by:__ @pcap.h@
-}
pCAP_IF_CONNECTION_STATUS_NOT_APPLICABLE :: FC.CInt
pCAP_IF_CONNECTION_STATUS_NOT_APPLICABLE =
  (48 :: FC.CInt)

{-| __C declaration:__ @pcap_handler@

    __defined at:__ @pcap.h:378:16@

    __exported by:__ @pcap.h@
-}
newtype Pcap_handler = Pcap_handler
  { un_Pcap_handler :: Ptr.FunPtr ((Ptr.Ptr U_char) -> (Ptr.Ptr Pcap_pkthdr) -> (Ptr.Ptr U_char) -> IO ())
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @PCAP_ERROR@

    __defined at:__ @pcap.h:387:9@

    __exported by:__ @pcap.h@
-}
pCAP_ERROR :: FC.CInt
pCAP_ERROR = C.negate (1 :: FC.CInt)

{-| __C declaration:__ @PCAP_ERROR_BREAK@

    __defined at:__ @pcap.h:388:9@

    __exported by:__ @pcap.h@
-}
pCAP_ERROR_BREAK :: FC.CInt
pCAP_ERROR_BREAK = C.negate (2 :: FC.CInt)

{-| __C declaration:__ @PCAP_ERROR_NOT_ACTIVATED@

    __defined at:__ @pcap.h:389:9@

    __exported by:__ @pcap.h@
-}
pCAP_ERROR_NOT_ACTIVATED :: FC.CInt
pCAP_ERROR_NOT_ACTIVATED = C.negate (3 :: FC.CInt)

{-| __C declaration:__ @PCAP_ERROR_ACTIVATED@

    __defined at:__ @pcap.h:390:9@

    __exported by:__ @pcap.h@
-}
pCAP_ERROR_ACTIVATED :: FC.CInt
pCAP_ERROR_ACTIVATED = C.negate (4 :: FC.CInt)

{-| __C declaration:__ @PCAP_ERROR_NO_SUCH_DEVICE@

    __defined at:__ @pcap.h:391:9@

    __exported by:__ @pcap.h@
-}
pCAP_ERROR_NO_SUCH_DEVICE :: FC.CInt
pCAP_ERROR_NO_SUCH_DEVICE = C.negate (5 :: FC.CInt)

{-| __C declaration:__ @PCAP_ERROR_RFMON_NOTSUP@

    __defined at:__ @pcap.h:392:9@

    __exported by:__ @pcap.h@
-}
pCAP_ERROR_RFMON_NOTSUP :: FC.CInt
pCAP_ERROR_RFMON_NOTSUP = C.negate (6 :: FC.CInt)

{-| __C declaration:__ @PCAP_ERROR_NOT_RFMON@

    __defined at:__ @pcap.h:393:9@

    __exported by:__ @pcap.h@
-}
pCAP_ERROR_NOT_RFMON :: FC.CInt
pCAP_ERROR_NOT_RFMON = C.negate (7 :: FC.CInt)

{-| __C declaration:__ @PCAP_ERROR_PERM_DENIED@

    __defined at:__ @pcap.h:394:9@

    __exported by:__ @pcap.h@
-}
pCAP_ERROR_PERM_DENIED :: FC.CInt
pCAP_ERROR_PERM_DENIED = C.negate (8 :: FC.CInt)

{-| __C declaration:__ @PCAP_ERROR_IFACE_NOT_UP@

    __defined at:__ @pcap.h:395:9@

    __exported by:__ @pcap.h@
-}
pCAP_ERROR_IFACE_NOT_UP :: FC.CInt
pCAP_ERROR_IFACE_NOT_UP = C.negate (9 :: FC.CInt)

{-| __C declaration:__ @PCAP_ERROR_CANTSET_TSTAMP_TYPE@

    __defined at:__ @pcap.h:396:9@

    __exported by:__ @pcap.h@
-}
pCAP_ERROR_CANTSET_TSTAMP_TYPE :: FC.CInt
pCAP_ERROR_CANTSET_TSTAMP_TYPE =
  C.negate (10 :: FC.CInt)

{-| __C declaration:__ @PCAP_ERROR_PROMISC_PERM_DENIED@

    __defined at:__ @pcap.h:397:9@

    __exported by:__ @pcap.h@
-}
pCAP_ERROR_PROMISC_PERM_DENIED :: FC.CInt
pCAP_ERROR_PROMISC_PERM_DENIED =
  C.negate (11 :: FC.CInt)

{-| __C declaration:__ @PCAP_ERROR_TSTAMP_PRECISION_NOTSUP@

    __defined at:__ @pcap.h:398:9@

    __exported by:__ @pcap.h@
-}
pCAP_ERROR_TSTAMP_PRECISION_NOTSUP :: FC.CInt
pCAP_ERROR_TSTAMP_PRECISION_NOTSUP =
  C.negate (12 :: FC.CInt)

{-| __C declaration:__ @PCAP_ERROR_CAPTURE_NOTSUP@

    __defined at:__ @pcap.h:399:9@

    __exported by:__ @pcap.h@
-}
pCAP_ERROR_CAPTURE_NOTSUP :: FC.CInt
pCAP_ERROR_CAPTURE_NOTSUP = C.negate (13 :: FC.CInt)

{-| __C declaration:__ @PCAP_WARNING@

    __defined at:__ @pcap.h:406:9@

    __exported by:__ @pcap.h@
-}
pCAP_WARNING :: FC.CInt
pCAP_WARNING = (1 :: FC.CInt)

{-| __C declaration:__ @PCAP_WARNING_PROMISC_NOTSUP@

    __defined at:__ @pcap.h:407:9@

    __exported by:__ @pcap.h@
-}
pCAP_WARNING_PROMISC_NOTSUP :: FC.CInt
pCAP_WARNING_PROMISC_NOTSUP = (2 :: FC.CInt)

{-| __C declaration:__ @PCAP_WARNING_TSTAMP_TYPE_NOTSUP@

    __defined at:__ @pcap.h:408:9@

    __exported by:__ @pcap.h@
-}
pCAP_WARNING_TSTAMP_TYPE_NOTSUP :: FC.CInt
pCAP_WARNING_TSTAMP_TYPE_NOTSUP = (3 :: FC.CInt)

{-| __C declaration:__ @PCAP_NETMASK_UNKNOWN@

    __defined at:__ @pcap.h:414:9@

    __exported by:__ @pcap.h@
-}
pCAP_NETMASK_UNKNOWN :: FC.CInt
pCAP_NETMASK_UNKNOWN = (4294967295 :: FC.CInt)

{-| __C declaration:__ @PCAP_CHAR_ENC_LOCAL@

    __defined at:__ @pcap.h:431:9@

    __exported by:__ @pcap.h@
-}
pCAP_CHAR_ENC_LOCAL :: FC.CUInt
pCAP_CHAR_ENC_LOCAL = (0 :: FC.CUInt)

{-| __C declaration:__ @PCAP_CHAR_ENC_UTF_8@

    __defined at:__ @pcap.h:432:9@

    __exported by:__ @pcap.h@
-}
pCAP_CHAR_ENC_UTF_8 :: FC.CUInt
pCAP_CHAR_ENC_UTF_8 = (1 :: FC.CUInt)

{-| __C declaration:__ @pcap_init@

    __defined at:__ @pcap.h:435:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_5a74b0412e8f8c8c" pcap_init
  :: FC.CUInt
  -> Ptr.Ptr FC.CChar
  -> IO FC.CInt

{-| __C declaration:__ @pcap_init@

    __defined at:__ @pcap.h:435:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_c9f9e9c9e4f8892d" hs_bindgen_c9f9e9c9e4f8892d
  :: IO (Ptr.FunPtr (FC.CUInt -> (Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE pcap_init_ptr #-}

pcap_init_ptr :: Ptr.FunPtr (FC.CUInt -> (Ptr.Ptr FC.CChar) -> IO FC.CInt)
pcap_init_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c9f9e9c9e4f8892d

{-| __C declaration:__ @pcap_lookupnet@

    __defined at:__ @pcap.h:447:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_dd0412a256181b94" pcap_lookupnet
  :: Ptr.Ptr FC.CChar
  -> Ptr.Ptr Bpf_u_int32
  -> Ptr.Ptr Bpf_u_int32
  -> Ptr.Ptr FC.CChar
  -> IO FC.CInt

{-| __C declaration:__ @pcap_lookupnet@

    __defined at:__ @pcap.h:447:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_7d836eead1796585" hs_bindgen_7d836eead1796585
  :: IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> (Ptr.Ptr Bpf_u_int32) -> (Ptr.Ptr Bpf_u_int32) -> (Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE pcap_lookupnet_ptr #-}

pcap_lookupnet_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> (Ptr.Ptr Bpf_u_int32) -> (Ptr.Ptr Bpf_u_int32) -> (Ptr.Ptr FC.CChar) -> IO FC.CInt)
pcap_lookupnet_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7d836eead1796585

{-| __C declaration:__ @pcap_create@

    __defined at:__ @pcap.h:450:18@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_c67404749d715e0f" pcap_create
  :: Ptr.Ptr FC.CChar
  -> Ptr.Ptr FC.CChar
  -> IO (Ptr.Ptr Pcap_t)

{-| __C declaration:__ @pcap_create@

    __defined at:__ @pcap.h:450:18@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_b4a4a216f0fbdba5" hs_bindgen_b4a4a216f0fbdba5
  :: IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr Pcap_t)))

{-# NOINLINE pcap_create_ptr #-}

pcap_create_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr Pcap_t))
pcap_create_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b4a4a216f0fbdba5

{-| __C declaration:__ @pcap_set_snaplen@

    __defined at:__ @pcap.h:453:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_899f506320c671ee" pcap_set_snaplen
  :: Ptr.Ptr Pcap_t
  -> FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @pcap_set_snaplen@

    __defined at:__ @pcap.h:453:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_88236eb94df0f75c" hs_bindgen_88236eb94df0f75c
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE pcap_set_snaplen_ptr #-}

pcap_set_snaplen_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> IO FC.CInt)
pcap_set_snaplen_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_88236eb94df0f75c

{-| __C declaration:__ @pcap_set_promisc@

    __defined at:__ @pcap.h:456:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_0129cbee2211ca4c" pcap_set_promisc
  :: Ptr.Ptr Pcap_t
  -> FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @pcap_set_promisc@

    __defined at:__ @pcap.h:456:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_b3d4fa8f98e045d9" hs_bindgen_b3d4fa8f98e045d9
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE pcap_set_promisc_ptr #-}

pcap_set_promisc_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> IO FC.CInt)
pcap_set_promisc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b3d4fa8f98e045d9

{-| __C declaration:__ @pcap_can_set_rfmon@

    __defined at:__ @pcap.h:459:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_a8c2eaf79b0eed7b" pcap_can_set_rfmon
  :: Ptr.Ptr Pcap_t
  -> IO FC.CInt

{-| __C declaration:__ @pcap_can_set_rfmon@

    __defined at:__ @pcap.h:459:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_66a4060db5c6a44e" hs_bindgen_66a4060db5c6a44e
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt))

{-# NOINLINE pcap_can_set_rfmon_ptr #-}

pcap_can_set_rfmon_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt)
pcap_can_set_rfmon_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_66a4060db5c6a44e

{-| __C declaration:__ @pcap_set_rfmon@

    __defined at:__ @pcap.h:462:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_7b8f0fc59a2bd7ef" pcap_set_rfmon
  :: Ptr.Ptr Pcap_t
  -> FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @pcap_set_rfmon@

    __defined at:__ @pcap.h:462:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_71a0b736624c8c6a" hs_bindgen_71a0b736624c8c6a
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE pcap_set_rfmon_ptr #-}

pcap_set_rfmon_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> IO FC.CInt)
pcap_set_rfmon_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_71a0b736624c8c6a

{-| __C declaration:__ @pcap_set_timeout@

    __defined at:__ @pcap.h:465:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_49ffc77ef9b4faae" pcap_set_timeout
  :: Ptr.Ptr Pcap_t
  -> FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @pcap_set_timeout@

    __defined at:__ @pcap.h:465:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_983c6131b48b1059" hs_bindgen_983c6131b48b1059
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE pcap_set_timeout_ptr #-}

pcap_set_timeout_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> IO FC.CInt)
pcap_set_timeout_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_983c6131b48b1059

{-| __C declaration:__ @pcap_set_tstamp_type@

    __defined at:__ @pcap.h:468:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_305b3c1ba436bff5" pcap_set_tstamp_type
  :: Ptr.Ptr Pcap_t
  -> FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @pcap_set_tstamp_type@

    __defined at:__ @pcap.h:468:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_2ceae335926feccd" hs_bindgen_2ceae335926feccd
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE pcap_set_tstamp_type_ptr #-}

pcap_set_tstamp_type_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> IO FC.CInt)
pcap_set_tstamp_type_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2ceae335926feccd

{-| __C declaration:__ @pcap_set_immediate_mode@

    __defined at:__ @pcap.h:471:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_e69c67afe679fa14" pcap_set_immediate_mode
  :: Ptr.Ptr Pcap_t
  -> FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @pcap_set_immediate_mode@

    __defined at:__ @pcap.h:471:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_90f1071272b80b48" hs_bindgen_90f1071272b80b48
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE pcap_set_immediate_mode_ptr #-}

pcap_set_immediate_mode_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> IO FC.CInt)
pcap_set_immediate_mode_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_90f1071272b80b48

{-| __C declaration:__ @pcap_set_buffer_size@

    __defined at:__ @pcap.h:474:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_729b652c3f9e3786" pcap_set_buffer_size
  :: Ptr.Ptr Pcap_t
  -> FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @pcap_set_buffer_size@

    __defined at:__ @pcap.h:474:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_256a4f684a37f304" hs_bindgen_256a4f684a37f304
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE pcap_set_buffer_size_ptr #-}

pcap_set_buffer_size_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> IO FC.CInt)
pcap_set_buffer_size_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_256a4f684a37f304

{-| __C declaration:__ @pcap_set_tstamp_precision@

    __defined at:__ @pcap.h:477:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_67e5b4f07de57ed3" pcap_set_tstamp_precision
  :: Ptr.Ptr Pcap_t
  -> FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @pcap_set_tstamp_precision@

    __defined at:__ @pcap.h:477:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_b6796c07f0083fb2" hs_bindgen_b6796c07f0083fb2
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE pcap_set_tstamp_precision_ptr #-}

pcap_set_tstamp_precision_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> IO FC.CInt)
pcap_set_tstamp_precision_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b6796c07f0083fb2

{-| __C declaration:__ @pcap_get_tstamp_precision@

    __defined at:__ @pcap.h:480:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_e160411dc9acb593" pcap_get_tstamp_precision
  :: Ptr.Ptr Pcap_t
  -> IO FC.CInt

{-| __C declaration:__ @pcap_get_tstamp_precision@

    __defined at:__ @pcap.h:480:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_40c019e03991b952" hs_bindgen_40c019e03991b952
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt))

{-# NOINLINE pcap_get_tstamp_precision_ptr #-}

pcap_get_tstamp_precision_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt)
pcap_get_tstamp_precision_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_40c019e03991b952

{-| __C declaration:__ @pcap_activate@

    __defined at:__ @pcap.h:483:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_ca4ffae84de4ef9c" pcap_activate
  :: Ptr.Ptr Pcap_t
  -> IO FC.CInt

{-| __C declaration:__ @pcap_activate@

    __defined at:__ @pcap.h:483:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_c14f86ce5e8ca2b9" hs_bindgen_c14f86ce5e8ca2b9
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt))

{-# NOINLINE pcap_activate_ptr #-}

pcap_activate_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt)
pcap_activate_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c14f86ce5e8ca2b9

{-| __C declaration:__ @pcap_list_tstamp_types@

    __defined at:__ @pcap.h:486:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_a5d9732ca735493a" pcap_list_tstamp_types
  :: Ptr.Ptr Pcap_t
  -> Ptr.Ptr (Ptr.Ptr FC.CInt)
  -> IO FC.CInt

{-| __C declaration:__ @pcap_list_tstamp_types@

    __defined at:__ @pcap.h:486:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_112a71c30aef74b7" hs_bindgen_112a71c30aef74b7
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr (Ptr.Ptr FC.CInt)) -> IO FC.CInt))

{-# NOINLINE pcap_list_tstamp_types_ptr #-}

pcap_list_tstamp_types_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr (Ptr.Ptr FC.CInt)) -> IO FC.CInt)
pcap_list_tstamp_types_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_112a71c30aef74b7

{-| __C declaration:__ @pcap_free_tstamp_types@

    __defined at:__ @pcap.h:489:15@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_80c4798a4663bce0" pcap_free_tstamp_types
  :: Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @pcap_free_tstamp_types@

    __defined at:__ @pcap.h:489:15@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_6049744ecd937e78" hs_bindgen_6049744ecd937e78
  :: IO (Ptr.FunPtr ((Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE pcap_free_tstamp_types_ptr #-}

pcap_free_tstamp_types_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CInt) -> IO ())
pcap_free_tstamp_types_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6049744ecd937e78

{-| __C declaration:__ @pcap_tstamp_type_name_to_val@

    __defined at:__ @pcap.h:492:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_1b88c6740aa68840" pcap_tstamp_type_name_to_val
  :: Ptr.Ptr FC.CChar
  -> IO FC.CInt

{-| __C declaration:__ @pcap_tstamp_type_name_to_val@

    __defined at:__ @pcap.h:492:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_8cd46287f05ed2c9" hs_bindgen_8cd46287f05ed2c9
  :: IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE pcap_tstamp_type_name_to_val_ptr #-}

pcap_tstamp_type_name_to_val_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt)
pcap_tstamp_type_name_to_val_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8cd46287f05ed2c9

{-| __C declaration:__ @pcap_tstamp_type_val_to_name@

    __defined at:__ @pcap.h:495:22@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_dbb311000616a945" pcap_tstamp_type_val_to_name
  :: FC.CInt
  -> IO (Ptr.Ptr FC.CChar)

{-| __C declaration:__ @pcap_tstamp_type_val_to_name@

    __defined at:__ @pcap.h:495:22@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_3abd8556be46d8d1" hs_bindgen_3abd8556be46d8d1
  :: IO (Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE pcap_tstamp_type_val_to_name_ptr #-}

pcap_tstamp_type_val_to_name_ptr :: Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr FC.CChar))
pcap_tstamp_type_val_to_name_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3abd8556be46d8d1

{-| __C declaration:__ @pcap_tstamp_type_val_to_description@

    __defined at:__ @pcap.h:498:22@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_a2c55f5ff16c095f" pcap_tstamp_type_val_to_description
  :: FC.CInt
  -> IO (Ptr.Ptr FC.CChar)

{-| __C declaration:__ @pcap_tstamp_type_val_to_description@

    __defined at:__ @pcap.h:498:22@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_ff9d6e66b3a16bd8" hs_bindgen_ff9d6e66b3a16bd8
  :: IO (Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE pcap_tstamp_type_val_to_description_ptr #-}

pcap_tstamp_type_val_to_description_ptr :: Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr FC.CChar))
pcap_tstamp_type_val_to_description_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ff9d6e66b3a16bd8

{-| __C declaration:__ @pcap_set_protocol_linux@

    __defined at:__ @pcap.h:502:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_183f6ad6ac6deac6" pcap_set_protocol_linux
  :: Ptr.Ptr Pcap_t
  -> FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @pcap_set_protocol_linux@

    __defined at:__ @pcap.h:502:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_616654327e926a08" hs_bindgen_616654327e926a08
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE pcap_set_protocol_linux_ptr #-}

pcap_set_protocol_linux_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> IO FC.CInt)
pcap_set_protocol_linux_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_616654327e926a08

{-| __C declaration:__ @PCAP_TSTAMP_HOST_LOWPREC@

    __defined at:__ @pcap.h:548:9@

    __exported by:__ @pcap.h@
-}
pCAP_TSTAMP_HOST_LOWPREC :: FC.CInt
pCAP_TSTAMP_HOST_LOWPREC = (1 :: FC.CInt)

{-| __C declaration:__ @PCAP_TSTAMP_HOST_HIPREC@

    __defined at:__ @pcap.h:549:9@

    __exported by:__ @pcap.h@
-}
pCAP_TSTAMP_HOST_HIPREC :: FC.CInt
pCAP_TSTAMP_HOST_HIPREC = (2 :: FC.CInt)

{-| __C declaration:__ @PCAP_TSTAMP_ADAPTER@

    __defined at:__ @pcap.h:550:9@

    __exported by:__ @pcap.h@
-}
pCAP_TSTAMP_ADAPTER :: FC.CInt
pCAP_TSTAMP_ADAPTER = (3 :: FC.CInt)

{-| __C declaration:__ @PCAP_TSTAMP_ADAPTER_UNSYNCED@

    __defined at:__ @pcap.h:551:9@

    __exported by:__ @pcap.h@
-}
pCAP_TSTAMP_ADAPTER_UNSYNCED :: FC.CInt
pCAP_TSTAMP_ADAPTER_UNSYNCED = (4 :: FC.CInt)

{-| __C declaration:__ @PCAP_TSTAMP_HOST_HIPREC_UNSYNCED@

    __defined at:__ @pcap.h:552:9@

    __exported by:__ @pcap.h@
-}
pCAP_TSTAMP_HOST_HIPREC_UNSYNCED :: FC.CInt
pCAP_TSTAMP_HOST_HIPREC_UNSYNCED = (5 :: FC.CInt)

{-| __C declaration:__ @PCAP_TSTAMP_PRECISION_NANO@

    __defined at:__ @pcap.h:561:9@

    __exported by:__ @pcap.h@
-}
pCAP_TSTAMP_PRECISION_NANO :: FC.CInt
pCAP_TSTAMP_PRECISION_NANO = (1 :: FC.CInt)

{-| __C declaration:__ @pcap_fopen_offline_with_tstamp_precision@

    __defined at:__ @pcap.h:604:20@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_73c31b44a2469172" pcap_fopen_offline_with_tstamp_precision
  :: Ptr.Ptr HsBindgen.Runtime.Prelude.CFile
  -> U_int
  -> Ptr.Ptr FC.CChar
  -> IO (Ptr.Ptr Pcap_t)

{-| __C declaration:__ @pcap_fopen_offline_with_tstamp_precision@

    __defined at:__ @pcap.h:604:20@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_983878c28d277db8" hs_bindgen_983878c28d277db8
  :: IO (Ptr.FunPtr ((Ptr.Ptr HsBindgen.Runtime.Prelude.CFile) -> U_int -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr Pcap_t)))

{-# NOINLINE pcap_fopen_offline_with_tstamp_precision_ptr #-}

pcap_fopen_offline_with_tstamp_precision_ptr :: Ptr.FunPtr ((Ptr.Ptr HsBindgen.Runtime.Prelude.CFile) -> U_int -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr Pcap_t))
pcap_fopen_offline_with_tstamp_precision_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_983878c28d277db8

{-| __C declaration:__ @pcap_fopen_offline@

    __defined at:__ @pcap.h:607:20@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_c11d352c68ff9eaf" pcap_fopen_offline
  :: Ptr.Ptr HsBindgen.Runtime.Prelude.CFile
  -> Ptr.Ptr FC.CChar
  -> IO (Ptr.Ptr Pcap_t)

{-| __C declaration:__ @pcap_fopen_offline@

    __defined at:__ @pcap.h:607:20@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_a24d511fb8c0ae8e" hs_bindgen_a24d511fb8c0ae8e
  :: IO (Ptr.FunPtr ((Ptr.Ptr HsBindgen.Runtime.Prelude.CFile) -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr Pcap_t)))

{-# NOINLINE pcap_fopen_offline_ptr #-}

pcap_fopen_offline_ptr :: Ptr.FunPtr ((Ptr.Ptr HsBindgen.Runtime.Prelude.CFile) -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr Pcap_t))
pcap_fopen_offline_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a24d511fb8c0ae8e

{-| __C declaration:__ @pcap_close@

    __defined at:__ @pcap.h:611:15@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_a00c19cb75a4068e" pcap_close
  :: Ptr.Ptr Pcap_t
  -> IO ()

{-| __C declaration:__ @pcap_close@

    __defined at:__ @pcap.h:611:15@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_03b9258ebb5aab3a" hs_bindgen_03b9258ebb5aab3a
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO ()))

{-# NOINLINE pcap_close_ptr #-}

pcap_close_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO ())
pcap_close_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_03b9258ebb5aab3a

{-| __C declaration:__ @pcap_loop@

    __defined at:__ @pcap.h:614:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_f721a2623c4e6b72" pcap_loop
  :: Ptr.Ptr Pcap_t
  -> FC.CInt
  -> Pcap_handler
  -> Ptr.Ptr U_char
  -> IO FC.CInt

{-| __C declaration:__ @pcap_loop@

    __defined at:__ @pcap.h:614:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_98b43666decaee1e" hs_bindgen_98b43666decaee1e
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> Pcap_handler -> (Ptr.Ptr U_char) -> IO FC.CInt))

{-# NOINLINE pcap_loop_ptr #-}

pcap_loop_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> Pcap_handler -> (Ptr.Ptr U_char) -> IO FC.CInt)
pcap_loop_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_98b43666decaee1e

{-| __C declaration:__ @pcap_dispatch@

    __defined at:__ @pcap.h:617:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_31243165d72e1fc1" pcap_dispatch
  :: Ptr.Ptr Pcap_t
  -> FC.CInt
  -> Pcap_handler
  -> Ptr.Ptr U_char
  -> IO FC.CInt

{-| __C declaration:__ @pcap_dispatch@

    __defined at:__ @pcap.h:617:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_c5ae44d99e7e4b73" hs_bindgen_c5ae44d99e7e4b73
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> Pcap_handler -> (Ptr.Ptr U_char) -> IO FC.CInt))

{-# NOINLINE pcap_dispatch_ptr #-}

pcap_dispatch_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> Pcap_handler -> (Ptr.Ptr U_char) -> IO FC.CInt)
pcap_dispatch_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c5ae44d99e7e4b73

{-| __C declaration:__ @pcap_next@

    __defined at:__ @pcap.h:620:24@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_b2f81d61b38cbbea" pcap_next
  :: Ptr.Ptr Pcap_t
  -> Ptr.Ptr Pcap_pkthdr
  -> IO (Ptr.Ptr U_char)

{-| __C declaration:__ @pcap_next@

    __defined at:__ @pcap.h:620:24@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_0b42e18b7963cd98" hs_bindgen_0b42e18b7963cd98
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr Pcap_pkthdr) -> IO (Ptr.Ptr U_char)))

{-# NOINLINE pcap_next_ptr #-}

pcap_next_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr Pcap_pkthdr) -> IO (Ptr.Ptr U_char))
pcap_next_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0b42e18b7963cd98

{-| __C declaration:__ @pcap_next_ex@

    __defined at:__ @pcap.h:623:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_9bfbdc8d5b6faeb4" pcap_next_ex
  :: Ptr.Ptr Pcap_t
  -> Ptr.Ptr (Ptr.Ptr Pcap_pkthdr)
  -> Ptr.Ptr (Ptr.Ptr U_char)
  -> IO FC.CInt

{-| __C declaration:__ @pcap_next_ex@

    __defined at:__ @pcap.h:623:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_d16b7e5def9aaf08" hs_bindgen_d16b7e5def9aaf08
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr (Ptr.Ptr Pcap_pkthdr)) -> (Ptr.Ptr (Ptr.Ptr U_char)) -> IO FC.CInt))

{-# NOINLINE pcap_next_ex_ptr #-}

pcap_next_ex_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr (Ptr.Ptr Pcap_pkthdr)) -> (Ptr.Ptr (Ptr.Ptr U_char)) -> IO FC.CInt)
pcap_next_ex_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d16b7e5def9aaf08

{-| __C declaration:__ @pcap_breakloop@

    __defined at:__ @pcap.h:626:15@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_8be607287be591ca" pcap_breakloop
  :: Ptr.Ptr Pcap_t
  -> IO ()

{-| __C declaration:__ @pcap_breakloop@

    __defined at:__ @pcap.h:626:15@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_3ff2347bf458f166" hs_bindgen_3ff2347bf458f166
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO ()))

{-# NOINLINE pcap_breakloop_ptr #-}

pcap_breakloop_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO ())
pcap_breakloop_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3ff2347bf458f166

{-| __C declaration:__ @pcap_stats@

    __defined at:__ @pcap.h:629:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_8b85c22d16b68b74" pcap_stats
  :: Ptr.Ptr Pcap_t
  -> Ptr.Ptr Pcap_stat
  -> IO FC.CInt

{-| __C declaration:__ @pcap_stats@

    __defined at:__ @pcap.h:629:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_57436a2f7dd3f348" hs_bindgen_57436a2f7dd3f348
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr Pcap_stat) -> IO FC.CInt))

{-# NOINLINE pcap_stats_ptr #-}

pcap_stats_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr Pcap_stat) -> IO FC.CInt)
pcap_stats_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_57436a2f7dd3f348

{-| __C declaration:__ @pcap_setfilter@

    __defined at:__ @pcap.h:632:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_8dd18013405d45c4" pcap_setfilter
  :: Ptr.Ptr Pcap_t
  -> Ptr.Ptr Bpf_program
  -> IO FC.CInt

{-| __C declaration:__ @pcap_setfilter@

    __defined at:__ @pcap.h:632:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_e584b88cdcd8cef3" hs_bindgen_e584b88cdcd8cef3
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr Bpf_program) -> IO FC.CInt))

{-# NOINLINE pcap_setfilter_ptr #-}

pcap_setfilter_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr Bpf_program) -> IO FC.CInt)
pcap_setfilter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e584b88cdcd8cef3

{-| __C declaration:__ @pcap_setdirection@

    __defined at:__ @pcap.h:635:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_a89a45dd24ab274a" pcap_setdirection
  :: Ptr.Ptr Pcap_t
  -> Pcap_direction_t
  -> IO FC.CInt

{-| __C declaration:__ @pcap_setdirection@

    __defined at:__ @pcap.h:635:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_9947109428276b0a" hs_bindgen_9947109428276b0a
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> Pcap_direction_t -> IO FC.CInt))

{-# NOINLINE pcap_setdirection_ptr #-}

pcap_setdirection_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> Pcap_direction_t -> IO FC.CInt)
pcap_setdirection_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9947109428276b0a

{-| __C declaration:__ @pcap_getnonblock@

    __defined at:__ @pcap.h:638:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_448afe8cd2792dfc" pcap_getnonblock
  :: Ptr.Ptr Pcap_t
  -> Ptr.Ptr FC.CChar
  -> IO FC.CInt

{-| __C declaration:__ @pcap_getnonblock@

    __defined at:__ @pcap.h:638:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_df5d01df419a5d0c" hs_bindgen_df5d01df419a5d0c
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE pcap_getnonblock_ptr #-}

pcap_getnonblock_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr FC.CChar) -> IO FC.CInt)
pcap_getnonblock_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_df5d01df419a5d0c

{-| __C declaration:__ @pcap_setnonblock@

    __defined at:__ @pcap.h:641:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_ae2fd5189870569a" pcap_setnonblock
  :: Ptr.Ptr Pcap_t
  -> FC.CInt
  -> Ptr.Ptr FC.CChar
  -> IO FC.CInt

{-| __C declaration:__ @pcap_setnonblock@

    __defined at:__ @pcap.h:641:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_7da24a20470a8056" hs_bindgen_7da24a20470a8056
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> (Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE pcap_setnonblock_ptr #-}

pcap_setnonblock_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> (Ptr.Ptr FC.CChar) -> IO FC.CInt)
pcap_setnonblock_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7da24a20470a8056

{-| __C declaration:__ @pcap_inject@

    __defined at:__ @pcap.h:644:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_5909d70f9932fc67" pcap_inject
  :: Ptr.Ptr Pcap_t
  -> Ptr.Ptr Void
  -> HsBindgen.Runtime.Prelude.CSize
  -> IO FC.CInt

{-| __C declaration:__ @pcap_inject@

    __defined at:__ @pcap.h:644:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_ddd66b69cc87ba56" hs_bindgen_ddd66b69cc87ba56
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CInt))

{-# NOINLINE pcap_inject_ptr #-}

pcap_inject_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CInt)
pcap_inject_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ddd66b69cc87ba56

{-| __C declaration:__ @pcap_sendpacket@

    __defined at:__ @pcap.h:647:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_ea9a87f52c0bb98a" pcap_sendpacket
  :: Ptr.Ptr Pcap_t
  -> Ptr.Ptr U_char
  -> FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @pcap_sendpacket@

    __defined at:__ @pcap.h:647:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_d12a0775f081904e" hs_bindgen_d12a0775f081904e
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr U_char) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE pcap_sendpacket_ptr #-}

pcap_sendpacket_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr U_char) -> FC.CInt -> IO FC.CInt)
pcap_sendpacket_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d12a0775f081904e

{-| __C declaration:__ @pcap_statustostr@

    __defined at:__ @pcap.h:650:22@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_92b0c0c43c37ffba" pcap_statustostr
  :: FC.CInt
  -> IO (Ptr.Ptr FC.CChar)

{-| __C declaration:__ @pcap_statustostr@

    __defined at:__ @pcap.h:650:22@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_c0bb64ef3e24ca49" hs_bindgen_c0bb64ef3e24ca49
  :: IO (Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE pcap_statustostr_ptr #-}

pcap_statustostr_ptr :: Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr FC.CChar))
pcap_statustostr_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c0bb64ef3e24ca49

{-| __C declaration:__ @pcap_strerror@

    __defined at:__ @pcap.h:653:22@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_ee0b68336360b03f" pcap_strerror
  :: FC.CInt
  -> IO (Ptr.Ptr FC.CChar)

{-| __C declaration:__ @pcap_strerror@

    __defined at:__ @pcap.h:653:22@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_172e573733c559fa" hs_bindgen_172e573733c559fa
  :: IO (Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE pcap_strerror_ptr #-}

pcap_strerror_ptr :: Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr FC.CChar))
pcap_strerror_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_172e573733c559fa

{-| __C declaration:__ @pcap_geterr@

    __defined at:__ @pcap.h:656:16@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_a32beca7c8760813" pcap_geterr
  :: Ptr.Ptr Pcap_t
  -> IO (Ptr.Ptr FC.CChar)

{-| __C declaration:__ @pcap_geterr@

    __defined at:__ @pcap.h:656:16@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_81791e56d8b1fefb" hs_bindgen_81791e56d8b1fefb
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE pcap_geterr_ptr #-}

pcap_geterr_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO (Ptr.Ptr FC.CChar))
pcap_geterr_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_81791e56d8b1fefb

{-| __C declaration:__ @pcap_perror@

    __defined at:__ @pcap.h:659:15@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_2208cc248d0f8d87" pcap_perror
  :: Ptr.Ptr Pcap_t
  -> Ptr.Ptr FC.CChar
  -> IO ()

{-| __C declaration:__ @pcap_perror@

    __defined at:__ @pcap.h:659:15@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_309f2a9e5d17c18c" hs_bindgen_309f2a9e5d17c18c
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr FC.CChar) -> IO ()))

{-# NOINLINE pcap_perror_ptr #-}

pcap_perror_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr FC.CChar) -> IO ())
pcap_perror_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_309f2a9e5d17c18c

{-| __C declaration:__ @pcap_compile@

    __defined at:__ @pcap.h:662:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_6b34a8490102b9a7" pcap_compile
  :: Ptr.Ptr Pcap_t
  -> Ptr.Ptr Bpf_program
  -> Ptr.Ptr FC.CChar
  -> FC.CInt
  -> Bpf_u_int32
  -> IO FC.CInt

{-| __C declaration:__ @pcap_compile@

    __defined at:__ @pcap.h:662:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_63a79dc54502c80d" hs_bindgen_63a79dc54502c80d
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr Bpf_program) -> (Ptr.Ptr FC.CChar) -> FC.CInt -> Bpf_u_int32 -> IO FC.CInt))

{-# NOINLINE pcap_compile_ptr #-}

pcap_compile_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr Bpf_program) -> (Ptr.Ptr FC.CChar) -> FC.CInt -> Bpf_u_int32 -> IO FC.CInt)
pcap_compile_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_63a79dc54502c80d

{-| __C declaration:__ @pcap_freecode@

    __defined at:__ @pcap.h:672:15@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_e833df3c5ebc9855" pcap_freecode
  :: Ptr.Ptr Bpf_program
  -> IO ()

{-| __C declaration:__ @pcap_freecode@

    __defined at:__ @pcap.h:672:15@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_fdf71a125e627818" hs_bindgen_fdf71a125e627818
  :: IO (Ptr.FunPtr ((Ptr.Ptr Bpf_program) -> IO ()))

{-# NOINLINE pcap_freecode_ptr #-}

pcap_freecode_ptr :: Ptr.FunPtr ((Ptr.Ptr Bpf_program) -> IO ())
pcap_freecode_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_fdf71a125e627818

{-| __C declaration:__ @pcap_offline_filter@

    __defined at:__ @pcap.h:675:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_ef995d1dca55f24e" pcap_offline_filter
  :: Ptr.Ptr Bpf_program
  -> Ptr.Ptr Pcap_pkthdr
  -> Ptr.Ptr U_char
  -> IO FC.CInt

{-| __C declaration:__ @pcap_offline_filter@

    __defined at:__ @pcap.h:675:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_3a072b87bf68590b" hs_bindgen_3a072b87bf68590b
  :: IO (Ptr.FunPtr ((Ptr.Ptr Bpf_program) -> (Ptr.Ptr Pcap_pkthdr) -> (Ptr.Ptr U_char) -> IO FC.CInt))

{-# NOINLINE pcap_offline_filter_ptr #-}

pcap_offline_filter_ptr :: Ptr.FunPtr ((Ptr.Ptr Bpf_program) -> (Ptr.Ptr Pcap_pkthdr) -> (Ptr.Ptr U_char) -> IO FC.CInt)
pcap_offline_filter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3a072b87bf68590b

{-| __C declaration:__ @pcap_datalink@

    __defined at:__ @pcap.h:679:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_2691de33c9de0eac" pcap_datalink
  :: Ptr.Ptr Pcap_t
  -> IO FC.CInt

{-| __C declaration:__ @pcap_datalink@

    __defined at:__ @pcap.h:679:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_cdd86d767366ccb6" hs_bindgen_cdd86d767366ccb6
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt))

{-# NOINLINE pcap_datalink_ptr #-}

pcap_datalink_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt)
pcap_datalink_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cdd86d767366ccb6

{-| __C declaration:__ @pcap_datalink_ext@

    __defined at:__ @pcap.h:682:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_7c355b0a96f34fba" pcap_datalink_ext
  :: Ptr.Ptr Pcap_t
  -> IO FC.CInt

{-| __C declaration:__ @pcap_datalink_ext@

    __defined at:__ @pcap.h:682:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_c48e00b1d1a53873" hs_bindgen_c48e00b1d1a53873
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt))

{-# NOINLINE pcap_datalink_ext_ptr #-}

pcap_datalink_ext_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt)
pcap_datalink_ext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c48e00b1d1a53873

{-| __C declaration:__ @pcap_list_datalinks@

    __defined at:__ @pcap.h:685:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_18cf0ac774b799e6" pcap_list_datalinks
  :: Ptr.Ptr Pcap_t
  -> Ptr.Ptr (Ptr.Ptr FC.CInt)
  -> IO FC.CInt

{-| __C declaration:__ @pcap_list_datalinks@

    __defined at:__ @pcap.h:685:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_baba6950f2676f33" hs_bindgen_baba6950f2676f33
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr (Ptr.Ptr FC.CInt)) -> IO FC.CInt))

{-# NOINLINE pcap_list_datalinks_ptr #-}

pcap_list_datalinks_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr (Ptr.Ptr FC.CInt)) -> IO FC.CInt)
pcap_list_datalinks_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_baba6950f2676f33

{-| __C declaration:__ @pcap_set_datalink@

    __defined at:__ @pcap.h:688:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_caff20c615fbaeea" pcap_set_datalink
  :: Ptr.Ptr Pcap_t
  -> FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @pcap_set_datalink@

    __defined at:__ @pcap.h:688:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_cb33ddf9d0e412f3" hs_bindgen_cb33ddf9d0e412f3
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE pcap_set_datalink_ptr #-}

pcap_set_datalink_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> FC.CInt -> IO FC.CInt)
pcap_set_datalink_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cb33ddf9d0e412f3

{-| __C declaration:__ @pcap_free_datalinks@

    __defined at:__ @pcap.h:691:15@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_919509339b068fa5" pcap_free_datalinks
  :: Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @pcap_free_datalinks@

    __defined at:__ @pcap.h:691:15@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_085e11dc4c8d3c50" hs_bindgen_085e11dc4c8d3c50
  :: IO (Ptr.FunPtr ((Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE pcap_free_datalinks_ptr #-}

pcap_free_datalinks_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CInt) -> IO ())
pcap_free_datalinks_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_085e11dc4c8d3c50

{-| __C declaration:__ @pcap_datalink_name_to_val@

    __defined at:__ @pcap.h:694:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_dee608d79c9e03f4" pcap_datalink_name_to_val
  :: Ptr.Ptr FC.CChar
  -> IO FC.CInt

{-| __C declaration:__ @pcap_datalink_name_to_val@

    __defined at:__ @pcap.h:694:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_0cb538c6a717e1a1" hs_bindgen_0cb538c6a717e1a1
  :: IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE pcap_datalink_name_to_val_ptr #-}

pcap_datalink_name_to_val_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt)
pcap_datalink_name_to_val_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0cb538c6a717e1a1

{-| __C declaration:__ @pcap_datalink_val_to_name@

    __defined at:__ @pcap.h:697:22@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_b28a5e20602167c9" pcap_datalink_val_to_name
  :: FC.CInt
  -> IO (Ptr.Ptr FC.CChar)

{-| __C declaration:__ @pcap_datalink_val_to_name@

    __defined at:__ @pcap.h:697:22@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_9bd0bd37dc9a5a12" hs_bindgen_9bd0bd37dc9a5a12
  :: IO (Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE pcap_datalink_val_to_name_ptr #-}

pcap_datalink_val_to_name_ptr :: Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr FC.CChar))
pcap_datalink_val_to_name_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9bd0bd37dc9a5a12

{-| __C declaration:__ @pcap_datalink_val_to_description@

    __defined at:__ @pcap.h:700:22@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_955733491140d435" pcap_datalink_val_to_description
  :: FC.CInt
  -> IO (Ptr.Ptr FC.CChar)

{-| __C declaration:__ @pcap_datalink_val_to_description@

    __defined at:__ @pcap.h:700:22@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_ac477b87d0044147" hs_bindgen_ac477b87d0044147
  :: IO (Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE pcap_datalink_val_to_description_ptr #-}

pcap_datalink_val_to_description_ptr :: Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr FC.CChar))
pcap_datalink_val_to_description_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ac477b87d0044147

{-| __C declaration:__ @pcap_datalink_val_to_description_or_dlt@

    __defined at:__ @pcap.h:703:22@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_6dc343be469a9e3e" pcap_datalink_val_to_description_or_dlt
  :: FC.CInt
  -> IO (Ptr.Ptr FC.CChar)

{-| __C declaration:__ @pcap_datalink_val_to_description_or_dlt@

    __defined at:__ @pcap.h:703:22@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_cb775158ce004f8a" hs_bindgen_cb775158ce004f8a
  :: IO (Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE pcap_datalink_val_to_description_or_dlt_ptr #-}

pcap_datalink_val_to_description_or_dlt_ptr :: Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr FC.CChar))
pcap_datalink_val_to_description_or_dlt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cb775158ce004f8a

{-| __C declaration:__ @pcap_snapshot@

    __defined at:__ @pcap.h:706:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_c26318f47f502103" pcap_snapshot
  :: Ptr.Ptr Pcap_t
  -> IO FC.CInt

{-| __C declaration:__ @pcap_snapshot@

    __defined at:__ @pcap.h:706:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_383e73406aa189ab" hs_bindgen_383e73406aa189ab
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt))

{-# NOINLINE pcap_snapshot_ptr #-}

pcap_snapshot_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt)
pcap_snapshot_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_383e73406aa189ab

{-| __C declaration:__ @pcap_is_swapped@

    __defined at:__ @pcap.h:709:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_9138a5d27c5ed968" pcap_is_swapped
  :: Ptr.Ptr Pcap_t
  -> IO FC.CInt

{-| __C declaration:__ @pcap_is_swapped@

    __defined at:__ @pcap.h:709:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_3c88b8a57bc4aa03" hs_bindgen_3c88b8a57bc4aa03
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt))

{-# NOINLINE pcap_is_swapped_ptr #-}

pcap_is_swapped_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt)
pcap_is_swapped_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3c88b8a57bc4aa03

{-| __C declaration:__ @pcap_major_version@

    __defined at:__ @pcap.h:712:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_4b895511c31cf718" pcap_major_version
  :: Ptr.Ptr Pcap_t
  -> IO FC.CInt

{-| __C declaration:__ @pcap_major_version@

    __defined at:__ @pcap.h:712:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_79a5564d5c6d6808" hs_bindgen_79a5564d5c6d6808
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt))

{-# NOINLINE pcap_major_version_ptr #-}

pcap_major_version_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt)
pcap_major_version_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_79a5564d5c6d6808

{-| __C declaration:__ @pcap_minor_version@

    __defined at:__ @pcap.h:715:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_5454bab4d77c4e08" pcap_minor_version
  :: Ptr.Ptr Pcap_t
  -> IO FC.CInt

{-| __C declaration:__ @pcap_minor_version@

    __defined at:__ @pcap.h:715:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_8f3c0c9f62bf5ca6" hs_bindgen_8f3c0c9f62bf5ca6
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt))

{-# NOINLINE pcap_minor_version_ptr #-}

pcap_minor_version_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt)
pcap_minor_version_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8f3c0c9f62bf5ca6

{-| __C declaration:__ @pcap_bufsize@

    __defined at:__ @pcap.h:718:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_29137dffca5b71d2" pcap_bufsize
  :: Ptr.Ptr Pcap_t
  -> IO FC.CInt

{-| __C declaration:__ @pcap_bufsize@

    __defined at:__ @pcap.h:718:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_3a2cafa5203f6a27" hs_bindgen_3a2cafa5203f6a27
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt))

{-# NOINLINE pcap_bufsize_ptr #-}

pcap_bufsize_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt)
pcap_bufsize_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3a2cafa5203f6a27

{-| __C declaration:__ @pcap_file@

    __defined at:__ @pcap.h:722:16@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_95d1f46f0cd2b89a" pcap_file
  :: Ptr.Ptr Pcap_t
  -> IO (Ptr.Ptr HsBindgen.Runtime.Prelude.CFile)

{-| __C declaration:__ @pcap_file@

    __defined at:__ @pcap.h:722:16@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_6102cb9744b09841" hs_bindgen_6102cb9744b09841
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO (Ptr.Ptr HsBindgen.Runtime.Prelude.CFile)))

{-# NOINLINE pcap_file_ptr #-}

pcap_file_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO (Ptr.Ptr HsBindgen.Runtime.Prelude.CFile))
pcap_file_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6102cb9744b09841

{-| __C declaration:__ @pcap_fileno@

    __defined at:__ @pcap.h:737:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_e4ec82d954c0cda3" pcap_fileno
  :: Ptr.Ptr Pcap_t
  -> IO FC.CInt

{-| __C declaration:__ @pcap_fileno@

    __defined at:__ @pcap.h:737:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_4a8e9c546e7e739c" hs_bindgen_4a8e9c546e7e739c
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt))

{-# NOINLINE pcap_fileno_ptr #-}

pcap_fileno_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt)
pcap_fileno_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4a8e9c546e7e739c

{-| __C declaration:__ @pcap_dump_open@

    __defined at:__ @pcap.h:745:25@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_679f873b0c503d7e" pcap_dump_open
  :: Ptr.Ptr Pcap_t
  -> Ptr.Ptr FC.CChar
  -> IO (Ptr.Ptr Pcap_dumper_t)

{-| __C declaration:__ @pcap_dump_open@

    __defined at:__ @pcap.h:745:25@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_ce88e3dd67a87f4e" hs_bindgen_ce88e3dd67a87f4e
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr Pcap_dumper_t)))

{-# NOINLINE pcap_dump_open_ptr #-}

pcap_dump_open_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr Pcap_dumper_t))
pcap_dump_open_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ce88e3dd67a87f4e

{-| __C declaration:__ @pcap_dump_fopen@

    __defined at:__ @pcap.h:770:27@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_2c61c28b1a0dcab0" pcap_dump_fopen
  :: Ptr.Ptr Pcap_t
  -> Ptr.Ptr HsBindgen.Runtime.Prelude.CFile
     {- ^ __C declaration:__ @fp@
     -}
  -> IO (Ptr.Ptr Pcap_dumper_t)

{-| __C declaration:__ @pcap_dump_fopen@

    __defined at:__ @pcap.h:770:27@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_56b877cb62ae0119" hs_bindgen_56b877cb62ae0119
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr HsBindgen.Runtime.Prelude.CFile) -> IO (Ptr.Ptr Pcap_dumper_t)))

{-# NOINLINE pcap_dump_fopen_ptr #-}

pcap_dump_fopen_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr HsBindgen.Runtime.Prelude.CFile) -> IO (Ptr.Ptr Pcap_dumper_t))
pcap_dump_fopen_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_56b877cb62ae0119

{-| __C declaration:__ @pcap_dump_open_append@

    __defined at:__ @pcap.h:774:25@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_d7bf02b04a26700b" pcap_dump_open_append
  :: Ptr.Ptr Pcap_t
  -> Ptr.Ptr FC.CChar
  -> IO (Ptr.Ptr Pcap_dumper_t)

{-| __C declaration:__ @pcap_dump_open_append@

    __defined at:__ @pcap.h:774:25@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_c6d9e001ffcc5db2" hs_bindgen_c6d9e001ffcc5db2
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr Pcap_dumper_t)))

{-# NOINLINE pcap_dump_open_append_ptr #-}

pcap_dump_open_append_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr Pcap_dumper_t))
pcap_dump_open_append_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c6d9e001ffcc5db2

{-| __C declaration:__ @pcap_dump_file@

    __defined at:__ @pcap.h:777:16@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_b3f87367e0d0a8c1" pcap_dump_file
  :: Ptr.Ptr Pcap_dumper_t
  -> IO (Ptr.Ptr HsBindgen.Runtime.Prelude.CFile)

{-| __C declaration:__ @pcap_dump_file@

    __defined at:__ @pcap.h:777:16@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_d6e5cd781d179052" hs_bindgen_d6e5cd781d179052
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_dumper_t) -> IO (Ptr.Ptr HsBindgen.Runtime.Prelude.CFile)))

{-# NOINLINE pcap_dump_file_ptr #-}

pcap_dump_file_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_dumper_t) -> IO (Ptr.Ptr HsBindgen.Runtime.Prelude.CFile))
pcap_dump_file_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d6e5cd781d179052

{-| __C declaration:__ @pcap_dump_ftell@

    __defined at:__ @pcap.h:780:15@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_d4e39b27a7f4faae" pcap_dump_ftell
  :: Ptr.Ptr Pcap_dumper_t
  -> IO FC.CLong

{-| __C declaration:__ @pcap_dump_ftell@

    __defined at:__ @pcap.h:780:15@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_acc6be05c7d92fdd" hs_bindgen_acc6be05c7d92fdd
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_dumper_t) -> IO FC.CLong))

{-# NOINLINE pcap_dump_ftell_ptr #-}

pcap_dump_ftell_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_dumper_t) -> IO FC.CLong)
pcap_dump_ftell_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_acc6be05c7d92fdd

{-| __C declaration:__ @pcap_dump_ftell64@

    __defined at:__ @pcap.h:783:18@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_0596fdb074f580de" pcap_dump_ftell64
  :: Ptr.Ptr Pcap_dumper_t
  -> IO HsBindgen.Runtime.Prelude.Int64

{-| __C declaration:__ @pcap_dump_ftell64@

    __defined at:__ @pcap.h:783:18@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_dd07417aca5bb8e5" hs_bindgen_dd07417aca5bb8e5
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_dumper_t) -> IO HsBindgen.Runtime.Prelude.Int64))

{-# NOINLINE pcap_dump_ftell64_ptr #-}

pcap_dump_ftell64_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_dumper_t) -> IO HsBindgen.Runtime.Prelude.Int64)
pcap_dump_ftell64_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_dd07417aca5bb8e5

{-| __C declaration:__ @pcap_dump_flush@

    __defined at:__ @pcap.h:786:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_ce95212b95850737" pcap_dump_flush
  :: Ptr.Ptr Pcap_dumper_t
  -> IO FC.CInt

{-| __C declaration:__ @pcap_dump_flush@

    __defined at:__ @pcap.h:786:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_1db9ebaec193f507" hs_bindgen_1db9ebaec193f507
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_dumper_t) -> IO FC.CInt))

{-# NOINLINE pcap_dump_flush_ptr #-}

pcap_dump_flush_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_dumper_t) -> IO FC.CInt)
pcap_dump_flush_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1db9ebaec193f507

{-| __C declaration:__ @pcap_dump_close@

    __defined at:__ @pcap.h:789:15@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_8737bfddc526b3d6" pcap_dump_close
  :: Ptr.Ptr Pcap_dumper_t
  -> IO ()

{-| __C declaration:__ @pcap_dump_close@

    __defined at:__ @pcap.h:789:15@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_c68e8746b85b3e95" hs_bindgen_c68e8746b85b3e95
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_dumper_t) -> IO ()))

{-# NOINLINE pcap_dump_close_ptr #-}

pcap_dump_close_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_dumper_t) -> IO ())
pcap_dump_close_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c68e8746b85b3e95

{-| __C declaration:__ @pcap_dump@

    __defined at:__ @pcap.h:792:15@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_5f2d744bd850aa8c" pcap_dump
  :: Ptr.Ptr U_char
  -> Ptr.Ptr Pcap_pkthdr
  -> Ptr.Ptr U_char
  -> IO ()

{-| __C declaration:__ @pcap_dump@

    __defined at:__ @pcap.h:792:15@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_1c463311f00b98d4" hs_bindgen_1c463311f00b98d4
  :: IO (Ptr.FunPtr ((Ptr.Ptr U_char) -> (Ptr.Ptr Pcap_pkthdr) -> (Ptr.Ptr U_char) -> IO ()))

{-# NOINLINE pcap_dump_ptr #-}

pcap_dump_ptr :: Ptr.FunPtr ((Ptr.Ptr U_char) -> (Ptr.Ptr Pcap_pkthdr) -> (Ptr.Ptr U_char) -> IO ())
pcap_dump_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1c463311f00b98d4

{-| __C declaration:__ @pcap_findalldevs@

    __defined at:__ @pcap.h:795:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_0c97f860802cd1b6" pcap_findalldevs
  :: Ptr.Ptr (Ptr.Ptr Pcap_if_t)
  -> Ptr.Ptr FC.CChar
  -> IO FC.CInt

{-| __C declaration:__ @pcap_findalldevs@

    __defined at:__ @pcap.h:795:14@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_b8927abb07d90d4e" hs_bindgen_b8927abb07d90d4e
  :: IO (Ptr.FunPtr ((Ptr.Ptr (Ptr.Ptr Pcap_if_t)) -> (Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE pcap_findalldevs_ptr #-}

pcap_findalldevs_ptr :: Ptr.FunPtr ((Ptr.Ptr (Ptr.Ptr Pcap_if_t)) -> (Ptr.Ptr FC.CChar) -> IO FC.CInt)
pcap_findalldevs_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b8927abb07d90d4e

{-| __C declaration:__ @pcap_freealldevs@

    __defined at:__ @pcap.h:798:15@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_93cdcd7040db1082" pcap_freealldevs
  :: Ptr.Ptr Pcap_if_t
  -> IO ()

{-| __C declaration:__ @pcap_freealldevs@

    __defined at:__ @pcap.h:798:15@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_22ffe8543386e2bc" hs_bindgen_22ffe8543386e2bc
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_if_t) -> IO ()))

{-# NOINLINE pcap_freealldevs_ptr #-}

pcap_freealldevs_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_if_t) -> IO ())
pcap_freealldevs_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_22ffe8543386e2bc

{-| __C declaration:__ @pcap_lib_version@

    __defined at:__ @pcap.h:816:22@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_d867d6999ff695c0" pcap_lib_version
  :: IO (Ptr.Ptr FC.CChar)

{-| __C declaration:__ @pcap_lib_version@

    __defined at:__ @pcap.h:816:22@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_0236695bf43b2f36" hs_bindgen_0236695bf43b2f36
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE pcap_lib_version_ptr #-}

pcap_lib_version_ptr :: Ptr.FunPtr (IO (Ptr.Ptr FC.CChar))
pcap_lib_version_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0236695bf43b2f36

{-| __C declaration:__ @pcap_get_selectable_fd@

    __defined at:__ @pcap.h:898:16@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_9c54b5c23c22c8bb" pcap_get_selectable_fd
  :: Ptr.Ptr Pcap_t
  -> IO FC.CInt

{-| __C declaration:__ @pcap_get_selectable_fd@

    __defined at:__ @pcap.h:898:16@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_3623f6e8d90e160a" hs_bindgen_3623f6e8d90e160a
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt))

{-# NOINLINE pcap_get_selectable_fd_ptr #-}

pcap_get_selectable_fd_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO FC.CInt)
pcap_get_selectable_fd_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3623f6e8d90e160a

{-| __C declaration:__ @pcap_get_required_select_timeout@

    __defined at:__ @pcap.h:901:34@

    __exported by:__ @pcap.h@
-}
foreign import ccall safe "hs_bindgen_a1534f381d527aa1" pcap_get_required_select_timeout
  :: Ptr.Ptr Pcap_t
  -> IO (Ptr.Ptr Timeval)

{-| __C declaration:__ @pcap_get_required_select_timeout@

    __defined at:__ @pcap.h:901:34@

    __exported by:__ @pcap.h@
-}
foreign import ccall unsafe "hs_bindgen_529cdf3e96af4a58" hs_bindgen_529cdf3e96af4a58
  :: IO (Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO (Ptr.Ptr Timeval)))

{-# NOINLINE pcap_get_required_select_timeout_ptr #-}

pcap_get_required_select_timeout_ptr :: Ptr.FunPtr ((Ptr.Ptr Pcap_t) -> IO (Ptr.Ptr Timeval))
pcap_get_required_select_timeout_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_529cdf3e96af4a58

{-| __C declaration:__ @PCAP_BUF_SIZE@

    __defined at:__ @pcap.h:919:9@

    __exported by:__ @pcap.h@
-}
pCAP_BUF_SIZE :: FC.CInt
pCAP_BUF_SIZE = (1024 :: FC.CInt)

{-| __C declaration:__ @PCAP_SRC_FILE@

    __defined at:__ @pcap.h:924:9@

    __exported by:__ @pcap.h@
-}
pCAP_SRC_FILE :: FC.CInt
pCAP_SRC_FILE = (2 :: FC.CInt)

{-| __C declaration:__ @PCAP_SRC_IFLOCAL@

    __defined at:__ @pcap.h:925:9@

    __exported by:__ @pcap.h@
-}
pCAP_SRC_IFLOCAL :: FC.CInt
pCAP_SRC_IFLOCAL = (3 :: FC.CInt)

{-| __C declaration:__ @PCAP_SRC_IFREMOTE@

    __defined at:__ @pcap.h:926:9@

    __exported by:__ @pcap.h@
-}
pCAP_SRC_IFREMOTE :: FC.CInt
pCAP_SRC_IFREMOTE = (4 :: FC.CInt)

{-| __C declaration:__ @PCAP_SRC_FILE_STRING@

    __defined at:__ @pcap.h:972:9@

    __exported by:__ @pcap.h@
-}
pCAP_SRC_FILE_STRING :: ((,) (Ptr.Ptr FC.CChar)) Int
pCAP_SRC_FILE_STRING =
  ((Ptr.Ptr "file://"#, 7) :: FC.CStringLen)

{-| __C declaration:__ @PCAP_SRC_IF_STRING@

    __defined at:__ @pcap.h:979:9@

    __exported by:__ @pcap.h@
-}
pCAP_SRC_IF_STRING :: ((,) (Ptr.Ptr FC.CChar)) Int
pCAP_SRC_IF_STRING =
  ((Ptr.Ptr "rpcap://"#, 8) :: FC.CStringLen)

{-| __C declaration:__ @PCAP_OPENFLAG_PROMISCUOUS@

    __defined at:__ @pcap.h:988:9@

    __exported by:__ @pcap.h@
-}
pCAP_OPENFLAG_PROMISCUOUS :: FC.CInt
pCAP_OPENFLAG_PROMISCUOUS = (1 :: FC.CInt)

{-| __C declaration:__ @PCAP_OPENFLAG_DATATX_UDP@

    __defined at:__ @pcap.h:1002:9@

    __exported by:__ @pcap.h@
-}
pCAP_OPENFLAG_DATATX_UDP :: FC.CInt
pCAP_OPENFLAG_DATATX_UDP = (2 :: FC.CInt)

{-| __C declaration:__ @PCAP_OPENFLAG_NOCAPTURE_RPCAP@

    __defined at:__ @pcap.h:1016:9@

    __exported by:__ @pcap.h@
-}
pCAP_OPENFLAG_NOCAPTURE_RPCAP :: FC.CInt
pCAP_OPENFLAG_NOCAPTURE_RPCAP = (4 :: FC.CInt)

{-| __C declaration:__ @PCAP_OPENFLAG_NOCAPTURE_LOCAL@

    __defined at:__ @pcap.h:1027:9@

    __exported by:__ @pcap.h@
-}
pCAP_OPENFLAG_NOCAPTURE_LOCAL :: FC.CInt
pCAP_OPENFLAG_NOCAPTURE_LOCAL = (8 :: FC.CInt)

{-| __C declaration:__ @PCAP_OPENFLAG_MAX_RESPONSIVENESS@

    __defined at:__ @pcap.h:1043:9@

    __exported by:__ @pcap.h@
-}
pCAP_OPENFLAG_MAX_RESPONSIVENESS :: FC.CInt
pCAP_OPENFLAG_MAX_RESPONSIVENESS = (16 :: FC.CInt)

{-| __C declaration:__ @RPCAP_RMTAUTH_PWD@

    __defined at:__ @pcap.h:1072:9@

    __exported by:__ @pcap.h@
-}
rPCAP_RMTAUTH_PWD :: FC.CInt
rPCAP_RMTAUTH_PWD = (1 :: FC.CInt)

{-| __C declaration:__ @pcap_rmtauth@

    __defined at:__ @pcap.h:1087:8@

    __exported by:__ @pcap.h@
-}
data Pcap_rmtauth = Pcap_rmtauth
  { pcap_rmtauth_type :: FC.CInt
    {- ^ __C declaration:__ @type@

         __defined at:__ @pcap.h:1097:6@

         __exported by:__ @pcap.h@
    -}
  , pcap_rmtauth_username :: Ptr.Ptr FC.CChar
    {- ^ __C declaration:__ @username@

         __defined at:__ @pcap.h:1105:8@

         __exported by:__ @pcap.h@
    -}
  , pcap_rmtauth_password :: Ptr.Ptr FC.CChar
    {- ^ __C declaration:__ @password@

         __defined at:__ @pcap.h:1113:8@

         __exported by:__ @pcap.h@
    -}
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

{-| __C declaration:__ @PCAP_SAMP_1_EVERY_N@

    __defined at:__ @pcap.h:1194:9@

    __exported by:__ @pcap.h@
-}
pCAP_SAMP_1_EVERY_N :: FC.CInt
pCAP_SAMP_1_EVERY_N = (1 :: FC.CInt)

{-| __C declaration:__ @PCAP_SAMP_FIRST_AFTER_N_MS@

    __defined at:__ @pcap.h:1205:9@

    __exported by:__ @pcap.h@
-}
pCAP_SAMP_FIRST_AFTER_N_MS :: FC.CInt
pCAP_SAMP_FIRST_AFTER_N_MS = (2 :: FC.CInt)

{-| __C declaration:__ @pcap_samp@

    __defined at:__ @pcap.h:1219:8@

    __exported by:__ @pcap.h@
-}
data Pcap_samp = Pcap_samp
  { pcap_samp_method :: FC.CInt
    {- ^ __C declaration:__ @method@

         __defined at:__ @pcap.h:1224:6@

         __exported by:__ @pcap.h@
    -}
  , pcap_samp_value :: FC.CInt
    {- ^ __C declaration:__ @value@

         __defined at:__ @pcap.h:1230:6@

         __exported by:__ @pcap.h@
    -}
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

{-| __C declaration:__ @RPCAP_HOSTLIST_SIZE@

    __defined at:__ @pcap.h:1244:9@

    __exported by:__ @pcap.h@
-}
rPCAP_HOSTLIST_SIZE :: FC.CInt
rPCAP_HOSTLIST_SIZE = (1024 :: FC.CInt)
