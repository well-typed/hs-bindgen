[
  DeclVar
    MacroExpr {
      macroExprName = Name
        "@NsVar"
        "iNCR",
      macroExprBody =
      CheckedMacroExpr {
        macroExprArgs = [Name "x"],
        macroExprBody = MApp
          MAdd
          [
            MTerm
              (MVar NoXVar (Name "x") []),
            MTerm
              (MInt
                IntegerLiteral {
                  integerLiteralText = "1",
                  integerLiteralType = Int Signed,
                  integerLiteralValue = 1})],
        macroExprType =
        "(forall a. Add a (IntLike (CIntegralType (IntLike (Int Signed)))) => (a -> AddRes a (IntLike (CIntegralType (IntLike (Int Signed))))))"},
      macroExprComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "INCR",
          commentLocation = Just
            "macro_functions.h:1:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_functions.h"],
              headerInclude =
              "macro_functions.h"},
          commentChildren = []}},
  DeclVar
    MacroExpr {
      macroExprName = Name
        "@NsVar"
        "aDD",
      macroExprBody =
      CheckedMacroExpr {
        macroExprArgs = [
          Name "x",
          Name "y"],
        macroExprBody = MApp
          MAdd
          [
            MTerm
              (MVar NoXVar (Name "x") []),
            MTerm
              (MVar NoXVar (Name "y") [])],
        macroExprType =
        "(forall a b. Add a b => (a -> b -> AddRes a b))"},
      macroExprComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "ADD",
          commentLocation = Just
            "macro_functions.h:2:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_functions.h"],
              headerInclude =
              "macro_functions.h"},
          commentChildren = []}},
  DeclVar
    MacroExpr {
      macroExprName = Name
        "@NsVar"
        "iD",
      macroExprBody =
      CheckedMacroExpr {
        macroExprArgs = [Name "X"],
        macroExprBody = MTerm
          (MVar NoXVar (Name "X") []),
        macroExprType =
        "(forall a. (a -> a))"},
      macroExprComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "ID",
          commentLocation = Just
            "macro_functions.h:4:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_functions.h"],
              headerInclude =
              "macro_functions.h"},
          commentChildren = []}},
  DeclVar
    MacroExpr {
      macroExprName = Name
        "@NsVar"
        "cONST",
      macroExprBody =
      CheckedMacroExpr {
        macroExprArgs = [
          Name "X",
          Name "Y"],
        macroExprBody = MTerm
          (MVar NoXVar (Name "X") []),
        macroExprType =
        "(forall a b. (a -> b -> a))"},
      macroExprComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "CONST",
          commentLocation = Just
            "macro_functions.h:5:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_functions.h"],
              headerInclude =
              "macro_functions.h"},
          commentChildren = []}},
  DeclVar
    MacroExpr {
      macroExprName = Name
        "@NsVar"
        "cMP",
      macroExprBody =
      CheckedMacroExpr {
        macroExprArgs = [
          Name "X",
          Name "Y"],
        macroExprBody = MApp
          MRelLT
          [
            MTerm
              (MVar NoXVar (Name "X") []),
            MTerm
              (MVar NoXVar (Name "Y") [])],
        macroExprType =
        "(forall a b. RelOrd a b => (a -> b -> IntLike (CIntegralType (IntLike (Int Signed)))))"},
      macroExprComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "CMP",
          commentLocation = Just
            "macro_functions.h:7:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_functions.h"],
              headerInclude =
              "macro_functions.h"},
          commentChildren = []}},
  DeclVar
    MacroExpr {
      macroExprName = Name
        "@NsVar"
        "fUN1",
      macroExprBody =
      CheckedMacroExpr {
        macroExprArgs = [
          Name "X",
          Name "Y"],
        macroExprBody = MApp
          MAdd
          [
            MTerm
              (MVar NoXVar (Name "X") []),
            MApp
              MMult
              [
                MTerm
                  (MInt
                    IntegerLiteral {
                      integerLiteralText = "12ull",
                      integerLiteralType = LongLong
                        Unsigned,
                      integerLiteralValue = 12}),
                MTerm
                  (MVar NoXVar (Name "Y") [])]],
        macroExprType =
        "(forall a b. Add a (MultRes (IntLike (CIntegralType (IntLike (LongLong Unsigned)))) b) => Mult (IntLike (CIntegralType (IntLike (LongLong Unsigned)))) b => (a -> b -> AddRes a (MultRes (IntLike (CIntegralType (IntLike (LongLong Unsigned)))) b)))"},
      macroExprComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "FUN1",
          commentLocation = Just
            "macro_functions.h:8:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_functions.h"],
              headerInclude =
              "macro_functions.h"},
          commentChildren = []}},
  DeclVar
    MacroExpr {
      macroExprName = Name
        "@NsVar"
        "fUN2",
      macroExprBody =
      CheckedMacroExpr {
        macroExprArgs = [
          Name "X",
          Name "Y"],
        macroExprBody = MApp
          MShiftLeft
          [
            MTerm
              (MVar NoXVar (Name "X") []),
            MApp
              MMult
              [
                MTerm
                  (MInt
                    IntegerLiteral {
                      integerLiteralText = "3ull",
                      integerLiteralType = LongLong
                        Unsigned,
                      integerLiteralValue = 3}),
                MTerm
                  (MVar NoXVar (Name "Y") [])]],
        macroExprType =
        "(forall a b. Mult (IntLike (CIntegralType (IntLike (LongLong Unsigned)))) b => Shift a (MultRes (IntLike (CIntegralType (IntLike (LongLong Unsigned)))) b) => (a -> b -> ShiftRes a))"},
      macroExprComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "FUN2",
          commentLocation = Just
            "macro_functions.h:9:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_functions.h"],
              headerInclude =
              "macro_functions.h"},
          commentChildren = []}},
  DeclVar
    MacroExpr {
      macroExprName = Name
        "@NsVar"
        "g",
      macroExprBody =
      CheckedMacroExpr {
        macroExprArgs = [
          Name "X",
          Name "Y"],
        macroExprBody = MTerm
          (MVar
            NoXVar
            (Name "CONST")
            [
              MTerm
                (MVar
                  NoXVar
                  (Name "INCR")
                  [
                    MTerm
                      (MVar NoXVar (Name "Y") [])]),
              MTerm
                (MVar
                  NoXVar
                  (Name "ID")
                  [
                    MTerm
                      (MVar NoXVar (Name "X") [])])]),
        macroExprType =
        "(forall a b. Add a (IntLike (CIntegralType (IntLike (Int Signed)))) => (b -> a -> AddRes a (IntLike (CIntegralType (IntLike (Int Signed))))))"},
      macroExprComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "G",
          commentLocation = Just
            "macro_functions.h:11:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_functions.h"],
              headerInclude =
              "macro_functions.h"},
          commentChildren = []}},
  DeclVar
    MacroExpr {
      macroExprName = Name
        "@NsVar"
        "dIV1",
      macroExprBody =
      CheckedMacroExpr {
        macroExprArgs = [
          Name "X",
          Name "Y"],
        macroExprBody = MApp
          MDiv
          [
            MTerm
              (MVar NoXVar (Name "X") []),
            MApp
              MAdd
              [
                MTerm
                  (MVar NoXVar (Name "Y") []),
                MTerm
                  (MInt
                    IntegerLiteral {
                      integerLiteralText = "12u",
                      integerLiteralType = Int
                        Unsigned,
                      integerLiteralValue = 12})]],
        macroExprType =
        "(forall a b. Add b (IntLike (CIntegralType (IntLike (Int Unsigned)))) => Div a (AddRes b (IntLike (CIntegralType (IntLike (Int Unsigned))))) => (a -> b -> DivRes a (AddRes b (IntLike (CIntegralType (IntLike (Int Unsigned)))))))"},
      macroExprComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "DIV1",
          commentLocation = Just
            "macro_functions.h:13:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_functions.h"],
              headerInclude =
              "macro_functions.h"},
          commentChildren = []}},
  DeclVar
    MacroExpr {
      macroExprName = Name
        "@NsVar"
        "dIV2",
      macroExprBody =
      CheckedMacroExpr {
        macroExprArgs = [
          Name "X",
          Name "Y"],
        macroExprBody = MApp
          MDiv
          [
            MApp
              MMult
              [
                MTerm
                  (MFloat
                    FloatingLiteral {
                      floatingLiteralText = "10.0f",
                      floatingLiteralType = FloatType,
                      floatingLiteralFloatValue =
                      10.0,
                      floatingLiteralDoubleValue =
                      10.0}),
                MTerm
                  (MVar NoXVar (Name "X") [])],
            MTerm
              (MVar NoXVar (Name "Y") [])],
        macroExprType =
        "(forall a b. Mult (FloatLike FloatType) a => Div (MultRes (FloatLike FloatType) a) b => (a -> b -> DivRes (MultRes (FloatLike FloatType) a) b))"},
      macroExprComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "DIV2",
          commentLocation = Just
            "macro_functions.h:14:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_functions.h"],
              headerInclude =
              "macro_functions.h"},
          commentChildren = []}},
  DeclVar
    MacroExpr {
      macroExprName = Name
        "@NsVar"
        "sWAP32",
      macroExprBody =
      CheckedMacroExpr {
        macroExprArgs = [Name "w"],
        macroExprBody = MApp
          MBitwiseOr
          [
            MApp
              MBitwiseAnd
              [
                MApp
                  MShiftRight
                  [
                    MTerm
                      (MVar NoXVar (Name "w") []),
                    MTerm
                      (MInt
                        IntegerLiteral {
                          integerLiteralText = "24",
                          integerLiteralType = Int Signed,
                          integerLiteralValue = 24})],
                MTerm
                  (MInt
                    IntegerLiteral {
                      integerLiteralText = "0xff",
                      integerLiteralType = Int Signed,
                      integerLiteralValue = 255})],
            MApp
              MBitwiseAnd
              [
                MApp
                  MShiftLeft
                  [
                    MTerm
                      (MVar NoXVar (Name "w") []),
                    MTerm
                      (MInt
                        IntegerLiteral {
                          integerLiteralText = "8",
                          integerLiteralType = Int Signed,
                          integerLiteralValue = 8})],
                MTerm
                  (MInt
                    IntegerLiteral {
                      integerLiteralText = "0xff0000",
                      integerLiteralType = Int Signed,
                      integerLiteralValue =
                      16711680})]],
        macroExprType =
        "(forall a. Bitwise (BitsRes (ShiftRes a) (IntLike (CIntegralType (IntLike (Int Signed))))) (BitsRes (ShiftRes a) (IntLike (CIntegralType (IntLike (Int Signed))))) => Bitwise (ShiftRes a) (IntLike (CIntegralType (IntLike (Int Signed)))) => Shift a (IntLike (CIntegralType (IntLike (Int Signed)))) => (a -> BitsRes (BitsRes (ShiftRes a) (IntLike (CIntegralType (IntLike (Int Signed))))) (BitsRes (ShiftRes a) (IntLike (CIntegralType (IntLike (Int Signed)))))))"},
      macroExprComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "SWAP32",
          commentLocation = Just
            "macro_functions.h:18:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_functions.h"],
              headerInclude =
              "macro_functions.h"},
          commentChildren = []}},
  DeclVar
    MacroExpr {
      macroExprName = Name
        "@NsVar"
        "aV_VERSION_INT",
      macroExprBody =
      CheckedMacroExpr {
        macroExprArgs = [
          Name "a",
          Name "b",
          Name "c"],
        macroExprBody = MApp
          MBitwiseOr
          [
            MApp
              MBitwiseOr
              [
                MApp
                  MShiftLeft
                  [
                    MTerm
                      (MVar NoXVar (Name "a") []),
                    MTerm
                      (MInt
                        IntegerLiteral {
                          integerLiteralText = "16",
                          integerLiteralType = Int Signed,
                          integerLiteralValue = 16})],
                MApp
                  MShiftLeft
                  [
                    MTerm
                      (MVar NoXVar (Name "b") []),
                    MTerm
                      (MInt
                        IntegerLiteral {
                          integerLiteralText = "8",
                          integerLiteralType = Int Signed,
                          integerLiteralValue = 8})]],
            MTerm
              (MVar NoXVar (Name "c") [])],
        macroExprType =
        "(forall a b c. Bitwise (ShiftRes a) (ShiftRes b) => Bitwise (BitsRes (ShiftRes a) (ShiftRes b)) c => Shift b (IntLike (CIntegralType (IntLike (Int Signed)))) => Shift a (IntLike (CIntegralType (IntLike (Int Signed)))) => (a -> b -> c -> BitsRes (BitsRes (ShiftRes a) (ShiftRes b)) c))"},
      macroExprComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "AV_VERSION_INT",
          commentLocation = Just
            "macro_functions.h:19:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_functions.h"],
              headerInclude =
              "macro_functions.h"},
          commentChildren = []}}]
