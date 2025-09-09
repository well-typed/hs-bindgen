[
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "a",
      varDeclType = ForallTy {
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = TyConAppTy
            (ATyCon
              (GenerativeTyCon
                (DataTyCon IntLikeTyCon)))
            [
              TyConAppTy
                (ATyCon
                  (GenerativeTyCon
                    (DataTyCon
                      (IntLikeTyCon
                        (CIntegralType
                          (IntLike (Int Signed)))))))
                []]}},
      varDeclBody = VarDeclIntegral
        5
        HsPrimCInt,
      varDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "A",
          commentLocation = Just
            "redeclaration_identical.h:3:9",
          commentHeader = Just
            "redeclaration_identical.h",
          commentChildren = []}}]
