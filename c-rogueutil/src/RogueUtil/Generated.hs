{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module RogueUtil.Generated where

import qualified Data.List.NonEmpty
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.CEnum
import Prelude ((<*>), Eq, IO, Int, Ord, Read, Show, pure, showsPrec)
import qualified Text.Read

$(CAPI.addCSource "#include \"rogueutil.h\"\nvoid RayUtil_locate (signed int arg1, signed int arg2) { locate(arg1, arg2); }\nsigned int RayUtil_getch (void) { return getch(); }\nsigned int RayUtil_kbhit (void) { return kbhit(); }\nvoid RayUtil_gotoxy (signed int arg1, signed int arg2) { gotoxy(arg1, arg2); }\nvoid RayUtil_rutil_print (RUTIL_STRING arg1) { rutil_print(arg1); }\nsigned int RayUtil_getkey (void) { return getkey(); }\nsigned int RayUtil_nb_getch (void) { return nb_getch(); }\nRUTIL_STRING RayUtil_getANSIColor (signed int arg1) { return getANSIColor(arg1); }\nRUTIL_STRING RayUtil_getANSIBgColor (signed int arg1) { return getANSIBgColor(arg1); }\nvoid RayUtil_setColor (signed int arg1) { setColor(arg1); }\nvoid RayUtil_setBackgroundColor (signed int arg1) { setBackgroundColor(arg1); }\nsigned int RayUtil_saveDefaultColor (void) { return saveDefaultColor(); }\nvoid RayUtil_resetColor (void) { resetColor(); }\nvoid RayUtil_cls (void) { cls(); }\nvoid RayUtil_setString (RUTIL_STRING arg1) { setString(arg1); }\nvoid RayUtil_setChar (char arg1) { setChar(arg1); }\nvoid RayUtil_setCursorVisibility (char arg1) { setCursorVisibility(arg1); }\nvoid RayUtil_hidecursor (void) { hidecursor(); }\nvoid RayUtil_showcursor (void) { showcursor(); }\nvoid RayUtil_msleep (unsigned int arg1) { msleep(arg1); }\nsigned int RayUtil_trows (void) { return trows(); }\nsigned int RayUtil_tcols (void) { return tcols(); }\nvoid RayUtil_anykey (RUTIL_STRING arg1) { anykey(arg1); }\nvoid RayUtil_setConsoleTitle (RUTIL_STRING arg1) { setConsoleTitle(arg1); }\nchar *RayUtil_getUsername (void) { return getUsername(); }\nvoid RayUtil_printXY (signed int arg1, signed int arg2, RUTIL_STRING arg3) { printXY(arg1, arg2, arg3); }\n")

foreign import ccall safe "RayUtil_locate" locate :: FC.CInt -> FC.CInt -> IO ()

foreign import ccall safe "RayUtil_getch" getch :: IO FC.CInt

foreign import ccall safe "RayUtil_kbhit" kbhit :: IO FC.CInt

foreign import ccall safe "RayUtil_gotoxy" gotoxy :: FC.CInt -> FC.CInt -> IO ()

newtype RUTIL_STRING = RUTIL_STRING
  { un_RUTIL_STRING :: F.Ptr FC.CChar
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

newtype Color_code = Color_code
  { un_Color_code :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable Color_code where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Color_code
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Color_code un_Color_code2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_Color_code2

instance HsBindgen.Runtime.CEnum.CEnum Color_code where

  type CEnumZ Color_code = FC.CUInt

  toCEnum = Color_code

  fromCEnum = un_Color_code

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "BLACK")
                                                     , (1, Data.List.NonEmpty.singleton "BLUE")
                                                     , (2, Data.List.NonEmpty.singleton "GREEN")
                                                     , (3, Data.List.NonEmpty.singleton "CYAN")
                                                     , (4, Data.List.NonEmpty.singleton "RED")
                                                     , (5, Data.List.NonEmpty.singleton "MAGENTA")
                                                     , (6, Data.List.NonEmpty.singleton "BROWN")
                                                     , (7, Data.List.NonEmpty.singleton "GREY")
                                                     , (8, Data.List.NonEmpty.singleton "DARKGREY")
                                                     , (9, Data.List.NonEmpty.singleton "LIGHTBLUE")
                                                     , (10, Data.List.NonEmpty.singleton "LIGHTGREEN")
                                                     , (11, Data.List.NonEmpty.singleton "LIGHTCYAN")
                                                     , (12, Data.List.NonEmpty.singleton "LIGHTRED")
                                                     , (13, Data.List.NonEmpty.singleton "LIGHTMAGENTA")
                                                     , (14, Data.List.NonEmpty.singleton "YELLOW")
                                                     , (15, Data.List.NonEmpty.singleton "WHITE")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Color_code"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Color_code"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Color_code where

  minDeclaredValue = BLACK

  maxDeclaredValue = WHITE

instance Show Color_code where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Color_code where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern BLACK :: Color_code
pattern BLACK = Color_code 0

pattern BLUE :: Color_code
pattern BLUE = Color_code 1

pattern GREEN :: Color_code
pattern GREEN = Color_code 2

pattern CYAN :: Color_code
pattern CYAN = Color_code 3

pattern RED :: Color_code
pattern RED = Color_code 4

pattern MAGENTA :: Color_code
pattern MAGENTA = Color_code 5

pattern BROWN :: Color_code
pattern BROWN = Color_code 6

pattern GREY :: Color_code
pattern GREY = Color_code 7

pattern DARKGREY :: Color_code
pattern DARKGREY = Color_code 8

pattern LIGHTBLUE :: Color_code
pattern LIGHTBLUE = Color_code 9

pattern LIGHTGREEN :: Color_code
pattern LIGHTGREEN = Color_code 10

pattern LIGHTCYAN :: Color_code
pattern LIGHTCYAN = Color_code 11

pattern LIGHTRED :: Color_code
pattern LIGHTRED = Color_code 12

pattern LIGHTMAGENTA :: Color_code
pattern LIGHTMAGENTA = Color_code 13

pattern YELLOW :: Color_code
pattern YELLOW = Color_code 14

pattern WHITE :: Color_code
pattern WHITE = Color_code 15

newtype Key_code = Key_code
  { un_Key_code :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable Key_code where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Key_code
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Key_code un_Key_code2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_Key_code2

instance HsBindgen.Runtime.CEnum.CEnum Key_code where

  type CEnumZ Key_code = FC.CUInt

  toCEnum = Key_code

  fromCEnum = un_Key_code

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "KEY_ESCAPE")
                                                     , (1, Data.List.NonEmpty.singleton "KEY_ENTER")
                                                     , (2, Data.List.NonEmpty.singleton "KEY_INSERT")
                                                     , (3, Data.List.NonEmpty.singleton "KEY_HOME")
                                                     , (4, Data.List.NonEmpty.singleton "KEY_PGUP")
                                                     , (5, Data.List.NonEmpty.singleton "KEY_DELETE")
                                                     , (6, Data.List.NonEmpty.singleton "KEY_END")
                                                     , (7, Data.List.NonEmpty.singleton "KEY_PGDOWN")
                                                     , (14, Data.List.NonEmpty.singleton "KEY_UP")
                                                     , (15, Data.List.NonEmpty.singleton "KEY_DOWN")
                                                     , (16, Data.List.NonEmpty.singleton "KEY_LEFT")
                                                     , (17, Data.List.NonEmpty.singleton "KEY_RIGHT")
                                                     , (18, Data.List.NonEmpty.singleton "KEY_F1")
                                                     , (19, Data.List.NonEmpty.singleton "KEY_F2")
                                                     , (20, Data.List.NonEmpty.singleton "KEY_F3")
                                                     , (21, Data.List.NonEmpty.singleton "KEY_F4")
                                                     , (22, Data.List.NonEmpty.singleton "KEY_F5")
                                                     , (23, Data.List.NonEmpty.singleton "KEY_F6")
                                                     , (24, Data.List.NonEmpty.singleton "KEY_F7")
                                                     , (25, Data.List.NonEmpty.singleton "KEY_F8")
                                                     , (26, Data.List.NonEmpty.singleton "KEY_F9")
                                                     , (27, Data.List.NonEmpty.singleton "KEY_F10")
                                                     , (28, Data.List.NonEmpty.singleton "KEY_F11")
                                                     , (29, Data.List.NonEmpty.singleton "KEY_F12")
                                                     , (30, Data.List.NonEmpty.singleton "KEY_NUMDEL")
                                                     , (31, Data.List.NonEmpty.singleton "KEY_NUMPAD0")
                                                     , (32, Data.List.NonEmpty.singleton "KEY_SPACE")
                                                     , (127, Data.List.NonEmpty.singleton "KEY_NUMPAD1")
                                                     , (128, Data.List.NonEmpty.singleton "KEY_NUMPAD2")
                                                     , (129, Data.List.NonEmpty.singleton "KEY_NUMPAD3")
                                                     , (130, Data.List.NonEmpty.singleton "KEY_NUMPAD4")
                                                     , (131, Data.List.NonEmpty.singleton "KEY_NUMPAD5")
                                                     , (132, Data.List.NonEmpty.singleton "KEY_NUMPAD6")
                                                     , (133, Data.List.NonEmpty.singleton "KEY_NUMPAD7")
                                                     , (134, Data.List.NonEmpty.singleton "KEY_NUMPAD8")
                                                     , (135, Data.List.NonEmpty.singleton "KEY_NUMPAD9")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Key_code"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Key_code"

instance Show Key_code where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Key_code where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern KEY_ESCAPE :: Key_code
pattern KEY_ESCAPE = Key_code 0

pattern KEY_ENTER :: Key_code
pattern KEY_ENTER = Key_code 1

pattern KEY_SPACE :: Key_code
pattern KEY_SPACE = Key_code 32

pattern KEY_INSERT :: Key_code
pattern KEY_INSERT = Key_code 2

pattern KEY_HOME :: Key_code
pattern KEY_HOME = Key_code 3

pattern KEY_PGUP :: Key_code
pattern KEY_PGUP = Key_code 4

pattern KEY_DELETE :: Key_code
pattern KEY_DELETE = Key_code 5

pattern KEY_END :: Key_code
pattern KEY_END = Key_code 6

pattern KEY_PGDOWN :: Key_code
pattern KEY_PGDOWN = Key_code 7

pattern KEY_UP :: Key_code
pattern KEY_UP = Key_code 14

pattern KEY_DOWN :: Key_code
pattern KEY_DOWN = Key_code 15

pattern KEY_LEFT :: Key_code
pattern KEY_LEFT = Key_code 16

pattern KEY_RIGHT :: Key_code
pattern KEY_RIGHT = Key_code 17

pattern KEY_F1 :: Key_code
pattern KEY_F1 = Key_code 18

pattern KEY_F2 :: Key_code
pattern KEY_F2 = Key_code 19

pattern KEY_F3 :: Key_code
pattern KEY_F3 = Key_code 20

pattern KEY_F4 :: Key_code
pattern KEY_F4 = Key_code 21

pattern KEY_F5 :: Key_code
pattern KEY_F5 = Key_code 22

pattern KEY_F6 :: Key_code
pattern KEY_F6 = Key_code 23

pattern KEY_F7 :: Key_code
pattern KEY_F7 = Key_code 24

pattern KEY_F8 :: Key_code
pattern KEY_F8 = Key_code 25

pattern KEY_F9 :: Key_code
pattern KEY_F9 = Key_code 26

pattern KEY_F10 :: Key_code
pattern KEY_F10 = Key_code 27

pattern KEY_F11 :: Key_code
pattern KEY_F11 = Key_code 28

pattern KEY_F12 :: Key_code
pattern KEY_F12 = Key_code 29

pattern KEY_NUMDEL :: Key_code
pattern KEY_NUMDEL = Key_code 30

pattern KEY_NUMPAD0 :: Key_code
pattern KEY_NUMPAD0 = Key_code 31

pattern KEY_NUMPAD1 :: Key_code
pattern KEY_NUMPAD1 = Key_code 127

pattern KEY_NUMPAD2 :: Key_code
pattern KEY_NUMPAD2 = Key_code 128

pattern KEY_NUMPAD3 :: Key_code
pattern KEY_NUMPAD3 = Key_code 129

pattern KEY_NUMPAD4 :: Key_code
pattern KEY_NUMPAD4 = Key_code 130

pattern KEY_NUMPAD5 :: Key_code
pattern KEY_NUMPAD5 = Key_code 131

pattern KEY_NUMPAD6 :: Key_code
pattern KEY_NUMPAD6 = Key_code 132

pattern KEY_NUMPAD7 :: Key_code
pattern KEY_NUMPAD7 = Key_code 133

pattern KEY_NUMPAD8 :: Key_code
pattern KEY_NUMPAD8 = Key_code 134

pattern KEY_NUMPAD9 :: Key_code
pattern KEY_NUMPAD9 = Key_code 135

foreign import ccall safe "RayUtil_rutil_print" rutil_print :: RUTIL_STRING -> IO ()

foreign import ccall safe "RayUtil_getkey" getkey :: IO FC.CInt

foreign import ccall safe "RayUtil_nb_getch" nb_getch :: IO FC.CInt

foreign import ccall safe "RayUtil_getANSIColor" getANSIColor :: FC.CInt -> IO RUTIL_STRING

foreign import ccall safe "RayUtil_getANSIBgColor" getANSIBgColor :: FC.CInt -> IO RUTIL_STRING

foreign import ccall safe "RayUtil_setColor" setColor :: FC.CInt -> IO ()

foreign import ccall safe "RayUtil_setBackgroundColor" setBackgroundColor :: FC.CInt -> IO ()

foreign import ccall safe "RayUtil_saveDefaultColor" saveDefaultColor :: IO FC.CInt

foreign import ccall safe "RayUtil_resetColor" resetColor :: IO ()

foreign import ccall safe "RayUtil_cls" cls :: IO ()

foreign import ccall safe "RayUtil_setString" setString :: RUTIL_STRING -> IO ()

foreign import ccall safe "RayUtil_setChar" setChar :: FC.CChar -> IO ()

foreign import ccall safe "RayUtil_setCursorVisibility" setCursorVisibility :: FC.CChar -> IO ()

foreign import ccall safe "RayUtil_hidecursor" hidecursor :: IO ()

foreign import ccall safe "RayUtil_showcursor" showcursor :: IO ()

foreign import ccall safe "RayUtil_msleep" msleep :: FC.CUInt -> IO ()

foreign import ccall safe "RayUtil_trows" trows :: IO FC.CInt

foreign import ccall safe "RayUtil_tcols" tcols :: IO FC.CInt

foreign import ccall safe "RayUtil_anykey" anykey :: RUTIL_STRING -> IO ()

foreign import ccall safe "RayUtil_setConsoleTitle" setConsoleTitle :: RUTIL_STRING -> IO ()

foreign import ccall safe "RayUtil_getUsername" getUsername :: IO (F.Ptr FC.CChar)

foreign import ccall safe "RayUtil_printXY" printXY :: FC.CInt -> FC.CInt -> RUTIL_STRING -> IO ()
