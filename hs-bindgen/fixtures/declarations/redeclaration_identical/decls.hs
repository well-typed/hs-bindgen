[
  DeclVar
    MacroExpr {
      macroExprName = Name
        "@NsVar"
        "a",
      macroExprBody =
      CheckedMacroExpr {
        macroExprArgs = [],
        macroExprBody = MTerm
          (MInt
            IntegerLiteral {
              integerLiteralText = "5",
              integerLiteralType = Int Signed,
              integerLiteralValue = 5}),
        macroExprType =
        "IntLike (CIntegralType (IntLike (Int Signed)))"},
      macroExprComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "A",
          commentLocation = Just
            "redeclaration_identical.h:3:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "declarations/redeclaration_identical.h"],
              headerInclude =
              "declarations/redeclaration_identical.h"},
          commentChildren = []}}]
