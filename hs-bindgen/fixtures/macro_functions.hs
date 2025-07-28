[
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "iNCR",
      varDeclType = ForallTy {
        forallTyBinders = [
          NameHint "a"],
        forallTy = QuantTy {
          quantTyCts = [
            ClassTy
              (AClass
                (GenerativeTyCon
                  (ClassTyCon AddTyCon)))
              [
                TyVarTy (Idx 0),
                TyConAppTy
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
                      []]]],
          quantTyBody = FunTy
            (TyVarTy (Idx 0))
            (TyConAppTy
              (ATyCon
                (FamilyTyCon AddResTyCon))
              [
                TyVarTy (Idx 0),
                TyConAppTy
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
                      []]])}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "x")
          (VarDeclApp
            (InfixAppHead MAdd)
            [
              VarDeclVar (Idx 0),
              VarDeclIntegral 1 HsPrimCInt])),
      varDeclComment = Nothing},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "aDD",
      varDeclType = ForallTy {
        forallTyBinders = [
          NameHint "a",
          NameHint "b"],
        forallTy = QuantTy {
          quantTyCts = [
            ClassTy
              (AClass
                (GenerativeTyCon
                  (ClassTyCon AddTyCon)))
              [
                TyVarTy (Idx 0),
                TyVarTy (Idx 1)]],
          quantTyBody = FunTy
            (TyVarTy (Idx 0))
            (FunTy
              (TyVarTy (Idx 1))
              (TyConAppTy
                (ATyCon
                  (FamilyTyCon AddResTyCon))
                [
                  TyVarTy (Idx 0),
                  TyVarTy (Idx 1)]))}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "x")
          (VarDeclLambda
            (Lambda
              (NameHint "y")
              (VarDeclApp
                (InfixAppHead MAdd)
                [
                  VarDeclVar (Idx 1),
                  VarDeclVar (Idx 0)])))),
      varDeclComment = Nothing},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "iD",
      varDeclType = ForallTy {
        forallTyBinders = [
          NameHint "a"],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = FunTy
            (TyVarTy (Idx 0))
            (TyVarTy (Idx 0))}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "x")
          (VarDeclVar (Idx 0))),
      varDeclComment = Nothing},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "cONST",
      varDeclType = ForallTy {
        forallTyBinders = [
          NameHint "a",
          NameHint "b"],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = FunTy
            (TyVarTy (Idx 0))
            (FunTy
              (TyVarTy (Idx 1))
              (TyVarTy (Idx 0)))}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "x")
          (VarDeclLambda
            (Lambda
              (NameHint "y")
              (VarDeclVar (Idx 1))))),
      varDeclComment = Nothing},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "cMP",
      varDeclType = ForallTy {
        forallTyBinders = [
          NameHint "a",
          NameHint "b"],
        forallTy = QuantTy {
          quantTyCts = [
            ClassTy
              (AClass
                (GenerativeTyCon
                  (ClassTyCon RelOrdTyCon)))
              [
                TyVarTy (Idx 0),
                TyVarTy (Idx 1)]],
          quantTyBody = FunTy
            (TyVarTy (Idx 0))
            (FunTy
              (TyVarTy (Idx 1))
              (TyConAppTy
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
                    []]))}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "x")
          (VarDeclLambda
            (Lambda
              (NameHint "y")
              (VarDeclApp
                (InfixAppHead MRelLT)
                [
                  VarDeclVar (Idx 1),
                  VarDeclVar (Idx 0)])))),
      varDeclComment = Nothing},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "fUN1",
      varDeclType = ForallTy {
        forallTyBinders = [
          NameHint "a",
          NameHint "b"],
        forallTy = QuantTy {
          quantTyCts = [
            ClassTy
              (AClass
                (GenerativeTyCon
                  (ClassTyCon AddTyCon)))
              [
                TyVarTy (Idx 0),
                TyConAppTy
                  (ATyCon
                    (FamilyTyCon MultResTyCon))
                  [
                    TyConAppTy
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
                                    (IntLike
                                      (LongLong Unsigned)))))))
                          []],
                    TyVarTy (Idx 1)]],
            ClassTy
              (AClass
                (GenerativeTyCon
                  (ClassTyCon MultTyCon)))
              [
                TyConAppTy
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
                                (IntLike
                                  (LongLong Unsigned)))))))
                      []],
                TyVarTy (Idx 1)]],
          quantTyBody = FunTy
            (TyVarTy (Idx 0))
            (FunTy
              (TyVarTy (Idx 1))
              (TyConAppTy
                (ATyCon
                  (FamilyTyCon AddResTyCon))
                [
                  TyVarTy (Idx 0),
                  TyConAppTy
                    (ATyCon
                      (FamilyTyCon MultResTyCon))
                    [
                      TyConAppTy
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
                                      (IntLike
                                        (LongLong Unsigned)))))))
                            []],
                      TyVarTy (Idx 1)]]))}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "x")
          (VarDeclLambda
            (Lambda
              (NameHint "y")
              (VarDeclApp
                (InfixAppHead MAdd)
                [
                  VarDeclVar (Idx 1),
                  VarDeclApp
                    (InfixAppHead MMult)
                    [
                      VarDeclIntegral
                        12
                        HsPrimCULLong,
                      VarDeclVar (Idx 0)]])))),
      varDeclComment = Nothing},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "fUN2",
      varDeclType = ForallTy {
        forallTyBinders = [
          NameHint "a",
          NameHint "b"],
        forallTy = QuantTy {
          quantTyCts = [
            ClassTy
              (AClass
                (GenerativeTyCon
                  (ClassTyCon MultTyCon)))
              [
                TyConAppTy
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
                                (IntLike
                                  (LongLong Unsigned)))))))
                      []],
                TyVarTy (Idx 1)],
            ClassTy
              (AClass
                (GenerativeTyCon
                  (ClassTyCon ShiftTyCon)))
              [
                TyVarTy (Idx 0),
                TyConAppTy
                  (ATyCon
                    (FamilyTyCon MultResTyCon))
                  [
                    TyConAppTy
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
                                    (IntLike
                                      (LongLong Unsigned)))))))
                          []],
                    TyVarTy (Idx 1)]]],
          quantTyBody = FunTy
            (TyVarTy (Idx 0))
            (FunTy
              (TyVarTy (Idx 1))
              (TyConAppTy
                (ATyCon
                  (FamilyTyCon ShiftResTyCon))
                [TyVarTy (Idx 0)]))}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "x")
          (VarDeclLambda
            (Lambda
              (NameHint "y")
              (VarDeclApp
                (InfixAppHead MShiftLeft)
                [
                  VarDeclVar (Idx 1),
                  VarDeclApp
                    (InfixAppHead MMult)
                    [
                      VarDeclIntegral 3 HsPrimCULLong,
                      VarDeclVar (Idx 0)]])))),
      varDeclComment = Nothing},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "g",
      varDeclType = ForallTy {
        forallTyBinders = [
          NameHint "a",
          NameHint "b"],
        forallTy = QuantTy {
          quantTyCts = [
            ClassTy
              (AClass
                (GenerativeTyCon
                  (ClassTyCon AddTyCon)))
              [
                TyVarTy (Idx 0),
                TyConAppTy
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
                      []]]],
          quantTyBody = FunTy
            (TyVarTy (Idx 1))
            (FunTy
              (TyVarTy (Idx 0))
              (TyConAppTy
                (ATyCon
                  (FamilyTyCon AddResTyCon))
                [
                  TyVarTy (Idx 0),
                  TyConAppTy
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
                        []]]))}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "x")
          (VarDeclLambda
            (Lambda
              (NameHint "y")
              (VarDeclApp
                (VarAppHead
                  (HsName "@NsVar" "cONST"))
                [
                  VarDeclApp
                    (VarAppHead
                      (HsName "@NsVar" "iNCR"))
                    [VarDeclVar (Idx 0)],
                  VarDeclApp
                    (VarAppHead
                      (HsName "@NsVar" "iD"))
                    [VarDeclVar (Idx 1)]])))),
      varDeclComment = Nothing},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "dIV1",
      varDeclType = ForallTy {
        forallTyBinders = [
          NameHint "a",
          NameHint "b"],
        forallTy = QuantTy {
          quantTyCts = [
            ClassTy
              (AClass
                (GenerativeTyCon
                  (ClassTyCon AddTyCon)))
              [
                TyVarTy (Idx 1),
                TyConAppTy
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
                                (IntLike (Int Unsigned)))))))
                      []]],
            ClassTy
              (AClass
                (GenerativeTyCon
                  (ClassTyCon DivTyCon)))
              [
                TyVarTy (Idx 0),
                TyConAppTy
                  (ATyCon
                    (FamilyTyCon AddResTyCon))
                  [
                    TyVarTy (Idx 1),
                    TyConAppTy
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
                                    (IntLike (Int Unsigned)))))))
                          []]]]],
          quantTyBody = FunTy
            (TyVarTy (Idx 0))
            (FunTy
              (TyVarTy (Idx 1))
              (TyConAppTy
                (ATyCon
                  (FamilyTyCon DivResTyCon))
                [
                  TyVarTy (Idx 0),
                  TyConAppTy
                    (ATyCon
                      (FamilyTyCon AddResTyCon))
                    [
                      TyVarTy (Idx 1),
                      TyConAppTy
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
                                      (IntLike (Int Unsigned)))))))
                            []]]]))}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "x")
          (VarDeclLambda
            (Lambda
              (NameHint "y")
              (VarDeclApp
                (InfixAppHead MDiv)
                [
                  VarDeclVar (Idx 1),
                  VarDeclApp
                    (InfixAppHead MAdd)
                    [
                      VarDeclVar (Idx 0),
                      VarDeclIntegral
                        12
                        HsPrimCUInt]])))),
      varDeclComment = Nothing},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "dIV2",
      varDeclType = ForallTy {
        forallTyBinders = [
          NameHint "a",
          NameHint "b"],
        forallTy = QuantTy {
          quantTyCts = [
            ClassTy
              (AClass
                (GenerativeTyCon
                  (ClassTyCon MultTyCon)))
              [
                TyConAppTy
                  (ATyCon
                    (GenerativeTyCon
                      (DataTyCon FloatLikeTyCon)))
                  [
                    TyConAppTy
                      (ATyCon
                        (GenerativeTyCon
                          (DataTyCon
                            (FloatLikeTyCon FloatType))))
                      []],
                TyVarTy (Idx 0)],
            ClassTy
              (AClass
                (GenerativeTyCon
                  (ClassTyCon DivTyCon)))
              [
                TyConAppTy
                  (ATyCon
                    (FamilyTyCon MultResTyCon))
                  [
                    TyConAppTy
                      (ATyCon
                        (GenerativeTyCon
                          (DataTyCon FloatLikeTyCon)))
                      [
                        TyConAppTy
                          (ATyCon
                            (GenerativeTyCon
                              (DataTyCon
                                (FloatLikeTyCon FloatType))))
                          []],
                    TyVarTy (Idx 0)],
                TyVarTy (Idx 1)]],
          quantTyBody = FunTy
            (TyVarTy (Idx 0))
            (FunTy
              (TyVarTy (Idx 1))
              (TyConAppTy
                (ATyCon
                  (FamilyTyCon DivResTyCon))
                [
                  TyConAppTy
                    (ATyCon
                      (FamilyTyCon MultResTyCon))
                    [
                      TyConAppTy
                        (ATyCon
                          (GenerativeTyCon
                            (DataTyCon FloatLikeTyCon)))
                        [
                          TyConAppTy
                            (ATyCon
                              (GenerativeTyCon
                                (DataTyCon
                                  (FloatLikeTyCon FloatType))))
                            []],
                      TyVarTy (Idx 0)],
                  TyVarTy (Idx 1)]))}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "x")
          (VarDeclLambda
            (Lambda
              (NameHint "y")
              (VarDeclApp
                (InfixAppHead MDiv)
                [
                  VarDeclApp
                    (InfixAppHead MMult)
                    [
                      VarDeclFloat 10.0,
                      VarDeclVar (Idx 1)],
                  VarDeclVar (Idx 0)])))),
      varDeclComment = Nothing},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "sWAP32",
      varDeclType = ForallTy {
        forallTyBinders = [
          NameHint "a"],
        forallTy = QuantTy {
          quantTyCts = [
            ClassTy
              (AClass
                (GenerativeTyCon
                  (ClassTyCon BitwiseTyCon)))
              [
                TyConAppTy
                  (ATyCon
                    (FamilyTyCon BitsResTyCon))
                  [
                    TyConAppTy
                      (ATyCon
                        (FamilyTyCon ShiftResTyCon))
                      [TyVarTy (Idx 0)],
                    TyConAppTy
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
                          []]],
                TyConAppTy
                  (ATyCon
                    (FamilyTyCon BitsResTyCon))
                  [
                    TyConAppTy
                      (ATyCon
                        (FamilyTyCon ShiftResTyCon))
                      [TyVarTy (Idx 0)],
                    TyConAppTy
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
                          []]]],
            ClassTy
              (AClass
                (GenerativeTyCon
                  (ClassTyCon BitwiseTyCon)))
              [
                TyConAppTy
                  (ATyCon
                    (FamilyTyCon ShiftResTyCon))
                  [TyVarTy (Idx 0)],
                TyConAppTy
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
                      []]],
            ClassTy
              (AClass
                (GenerativeTyCon
                  (ClassTyCon ShiftTyCon)))
              [
                TyVarTy (Idx 0),
                TyConAppTy
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
                      []]]],
          quantTyBody = FunTy
            (TyVarTy (Idx 0))
            (TyConAppTy
              (ATyCon
                (FamilyTyCon BitsResTyCon))
              [
                TyConAppTy
                  (ATyCon
                    (FamilyTyCon BitsResTyCon))
                  [
                    TyConAppTy
                      (ATyCon
                        (FamilyTyCon ShiftResTyCon))
                      [TyVarTy (Idx 0)],
                    TyConAppTy
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
                          []]],
                TyConAppTy
                  (ATyCon
                    (FamilyTyCon BitsResTyCon))
                  [
                    TyConAppTy
                      (ATyCon
                        (FamilyTyCon ShiftResTyCon))
                      [TyVarTy (Idx 0)],
                    TyConAppTy
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
                          []]]])}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "w")
          (VarDeclApp
            (InfixAppHead MBitwiseOr)
            [
              VarDeclApp
                (InfixAppHead MBitwiseAnd)
                [
                  VarDeclApp
                    (InfixAppHead MShiftRight)
                    [
                      VarDeclVar (Idx 0),
                      VarDeclIntegral 24 HsPrimCInt],
                  VarDeclIntegral 255 HsPrimCInt],
              VarDeclApp
                (InfixAppHead MBitwiseAnd)
                [
                  VarDeclApp
                    (InfixAppHead MShiftLeft)
                    [
                      VarDeclVar (Idx 0),
                      VarDeclIntegral 8 HsPrimCInt],
                  VarDeclIntegral
                    16711680
                    HsPrimCInt]])),
      varDeclComment = Nothing},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "aV_VERSION_INT",
      varDeclType = ForallTy {
        forallTyBinders = [
          NameHint "a",
          NameHint "b",
          NameHint "c"],
        forallTy = QuantTy {
          quantTyCts = [
            ClassTy
              (AClass
                (GenerativeTyCon
                  (ClassTyCon BitwiseTyCon)))
              [
                TyConAppTy
                  (ATyCon
                    (FamilyTyCon ShiftResTyCon))
                  [TyVarTy (Idx 0)],
                TyConAppTy
                  (ATyCon
                    (FamilyTyCon ShiftResTyCon))
                  [TyVarTy (Idx 1)]],
            ClassTy
              (AClass
                (GenerativeTyCon
                  (ClassTyCon BitwiseTyCon)))
              [
                TyConAppTy
                  (ATyCon
                    (FamilyTyCon BitsResTyCon))
                  [
                    TyConAppTy
                      (ATyCon
                        (FamilyTyCon ShiftResTyCon))
                      [TyVarTy (Idx 0)],
                    TyConAppTy
                      (ATyCon
                        (FamilyTyCon ShiftResTyCon))
                      [TyVarTy (Idx 1)]],
                TyVarTy (Idx 2)],
            ClassTy
              (AClass
                (GenerativeTyCon
                  (ClassTyCon ShiftTyCon)))
              [
                TyVarTy (Idx 1),
                TyConAppTy
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
                      []]],
            ClassTy
              (AClass
                (GenerativeTyCon
                  (ClassTyCon ShiftTyCon)))
              [
                TyVarTy (Idx 0),
                TyConAppTy
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
                      []]]],
          quantTyBody = FunTy
            (TyVarTy (Idx 0))
            (FunTy
              (TyVarTy (Idx 1))
              (FunTy
                (TyVarTy (Idx 2))
                (TyConAppTy
                  (ATyCon
                    (FamilyTyCon BitsResTyCon))
                  [
                    TyConAppTy
                      (ATyCon
                        (FamilyTyCon BitsResTyCon))
                      [
                        TyConAppTy
                          (ATyCon
                            (FamilyTyCon ShiftResTyCon))
                          [TyVarTy (Idx 0)],
                        TyConAppTy
                          (ATyCon
                            (FamilyTyCon ShiftResTyCon))
                          [TyVarTy (Idx 1)]],
                    TyVarTy (Idx 2)])))}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "a")
          (VarDeclLambda
            (Lambda
              (NameHint "b")
              (VarDeclLambda
                (Lambda
                  (NameHint "c")
                  (VarDeclApp
                    (InfixAppHead MBitwiseOr)
                    [
                      VarDeclApp
                        (InfixAppHead MBitwiseOr)
                        [
                          VarDeclApp
                            (InfixAppHead MShiftLeft)
                            [
                              VarDeclVar (Idx 2),
                              VarDeclIntegral 16 HsPrimCInt],
                          VarDeclApp
                            (InfixAppHead MShiftLeft)
                            [
                              VarDeclVar (Idx 1),
                              VarDeclIntegral 8 HsPrimCInt]],
                      VarDeclVar (Idx 0)])))))),
      varDeclComment = Nothing}]
