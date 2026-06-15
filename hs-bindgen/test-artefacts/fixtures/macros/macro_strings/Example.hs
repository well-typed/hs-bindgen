{-# LANGUAGE ExplicitForAll #-}

module Example
    ( Example.c1
    , Example.c2
    , Example.c3
    , Example.c4
    , Example.c5
    , Example.c6
    , Example.c7
    , Example.c8
    , Example.s1
    , Example.s2
    , Example.s3
    , Example.s4
    , Example.s5
    , Example.s6
    , Example.s7
    , Example.s8
    , Example.t1
    , Example.t2
    , Example.t3
    , Example.u
    , Example.v
    , Example.w1
    , Example.w2
    )
  where

import qualified Data.ByteString as BS
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

{-| __C declaration:__ @macro C1@

    __C literal:__ @\'a\'@

    __defined at:__ @macros\/macro_strings.h 4:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c1 :: RIP.CChar
c1 = 97

{-| __C declaration:__ @macro C2@

    __C literal:__ @\'\"\'@

    __defined at:__ @macros\/macro_strings.h 5:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c2 :: RIP.CChar
c2 = 34

{-| __C declaration:__ @macro C3@

    __C literal:__ @\'\\t\'@

    __defined at:__ @macros\/macro_strings.h 6:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c3 :: RIP.CChar
c3 = 9

{-| __C declaration:__ @macro C4@

    __C literal:__ @\'\\NUL\'@

    __defined at:__ @macros\/macro_strings.h 7:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c4 :: RIP.CChar
c4 = 0

{-| __C declaration:__ @macro C5@

    __C literal:__ @\'\\\'\'@

    __defined at:__ @macros\/macro_strings.h 8:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c5 :: RIP.CChar
c5 = 39

{-| __C declaration:__ @macro C6@

    __C literal:__ @\'?\'@

    __defined at:__ @macros\/macro_strings.h 9:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c6 :: RIP.CChar
c6 = 63

{-| __C declaration:__ @macro C7@

    __C literal:__ @\'S\'@

    __defined at:__ @macros\/macro_strings.h 10:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c7 :: RIP.CChar
c7 = 83

{-| __C declaration:__ @macro C8@

    __C literal:__ @\'S\'@

    __defined at:__ @macros\/macro_strings.h 11:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c8 :: RIP.CChar
c8 = 83

{-| __C declaration:__ @macro S1@

    __C literal:__ @\"a\"@

    __defined at:__ @macros\/macro_strings.h 20:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s1 :: BS.ByteString
s1 = BS.pack [0x61]

{-| __C declaration:__ @macro S2@

    __C literal:__ @\"\'\"@

    __defined at:__ @macros\/macro_strings.h 21:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s2 :: BS.ByteString
s2 = BS.pack [0x27]

{-| __C declaration:__ @macro S3@

    __C literal:__ @\"\\t\"@

    __defined at:__ @macros\/macro_strings.h 22:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s3 :: BS.ByteString
s3 = BS.pack [0x09]

{-| __C declaration:__ @macro S4@

    __C literal:__ @\"\\NUL\"@

    __defined at:__ @macros\/macro_strings.h 23:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s4 :: BS.ByteString
s4 = BS.pack [0x00]

{-| __C declaration:__ @macro S5@

    __C literal:__ @\"\'\"@

    __defined at:__ @macros\/macro_strings.h 24:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s5 :: BS.ByteString
s5 = BS.pack [0x27]

{-| __C declaration:__ @macro S6@

    __C literal:__ @\"?\"@

    __defined at:__ @macros\/macro_strings.h 25:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s6 :: BS.ByteString
s6 = BS.pack [0x3F]

{-| __C declaration:__ @macro S7@

    __C literal:__ @\"S\"@

    __defined at:__ @macros\/macro_strings.h 26:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s7 :: BS.ByteString
s7 = BS.pack [0x53]

{-| __C declaration:__ @macro S8@

    __C literal:__ @\"S\"@

    __defined at:__ @macros\/macro_strings.h 27:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s8 :: BS.ByteString
s8 = BS.pack [0x53]

{-| __C declaration:__ @macro T1@

    __C literal:__ @\"\\227\\129\\130\"@

    __defined at:__ @macros\/macro_strings.h 29:9@

    __exported by:__ @macros\/macro_strings.h@
-}
t1 :: BS.ByteString
t1 = BS.pack [0xE3, 0x81, 0x82]

{-| __C declaration:__ @macro T2@

    __C literal:__ @\"\\227\\129\\130\"@

    __defined at:__ @macros\/macro_strings.h 30:9@

    __exported by:__ @macros\/macro_strings.h@
-}
t2 :: BS.ByteString
t2 = BS.pack [0xE3, 0x81, 0x82]

{-| __C declaration:__ @macro T3@

    __C literal:__ @\"\\227\\129\\130\"@

    __defined at:__ @macros\/macro_strings.h 31:9@

    __exported by:__ @macros\/macro_strings.h@
-}
t3 :: BS.ByteString
t3 = BS.pack [0xE3, 0x81, 0x82]

{-| __C declaration:__ @macro U@

    __C literal:__ @\"\\SOH\\255\\SOH\\255\\SOH\\255\\SOH\\255\"@

    __defined at:__ @macros\/macro_strings.h 33:9@

    __exported by:__ @macros\/macro_strings.h@
-}
u :: BS.ByteString
u =
  BS.pack [0x01, 0xFF, 0x01, 0xFF, 0x01, 0xFF, 0x01, 0xFF]

{-| __C declaration:__ @macro V@

    __C literal:__ @\"\\SOH\\STX\\ETX\\EOT\\ENQ\\ACK\"@

    __defined at:__ @macros\/macro_strings.h 34:9@

    __exported by:__ @macros\/macro_strings.h@
-}
v :: BS.ByteString
v = BS.pack [0x01, 0x02, 0x03, 0x04, 0x05, 0x06]

{-| __C declaration:__ @macro W1@

    __C literal:__ @\"hij\\NUL\"@

    __defined at:__ @macros\/macro_strings.h 36:9@

    __exported by:__ @macros\/macro_strings.h@
-}
w1 :: BS.ByteString
w1 = BS.pack [0x68, 0x69, 0x6A, 0x00]

{-| __C declaration:__ @macro W2@

    __C literal:__ @\"abc\\NULdef\\NULg\"@

    __defined at:__ @macros\/macro_strings.h 37:9@

    __exported by:__ @macros\/macro_strings.h@
-}
w2 :: BS.ByteString
w2 =
  BS.pack [0x61, 0x62, 0x63, 0x00, 0x64, 0x65, 0x66, 0x00, 0x67]
