[
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "n",
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
        3
        HsPrimCInt,
      varDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "N",
          commentLocation = Just
            "type_naturals.h:4:9",
          commentHeader = Just
            "type_naturals.h",
          commentChildren = []}},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "m",
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
      varDeclBody = VarDeclApp
        (InfixAppHead MAdd)
        [
          VarDeclIntegral 1 HsPrimCInt,
          VarDeclApp
            (VarAppHead
              (HsName "@NsVar" "n"))
            []],
      varDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "M",
          commentLocation = Just
            "type_naturals.h:5:9",
          commentHeader = Just
            "type_naturals.h",
          commentChildren = []}},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "f",
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
                                    (IntLike (Int Signed)))))))
                          []],
                    TyVarTy (Idx 1)]],
            ClassTy
              (AClass
                (GenerativeTyCon
                  (ClassTyCon SubTyCon)))
              [
                TyConAppTy
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
                                        (IntLike (Int Signed)))))))
                              []],
                        TyVarTy (Idx 1)]],
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
                                (IntLike (Int Signed)))))))
                      []],
                TyVarTy (Idx 1)]],
          quantTyBody = FunTy
            (TyVarTy (Idx 0))
            (FunTy
              (TyVarTy (Idx 1))
              (TyConAppTy
                (ATyCon
                  (FamilyTyCon SubResTyCon))
                [
                  TyConAppTy
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
                                          (IntLike (Int Signed)))))))
                                []],
                          TyVarTy (Idx 1)]],
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
          (NameHint "a")
          (VarDeclLambda
            (Lambda
              (NameHint "b")
              (VarDeclApp
                (InfixAppHead MSub)
                [
                  VarDeclApp
                    (InfixAppHead MAdd)
                    [
                      VarDeclVar (Idx 1),
                      VarDeclApp
                        (InfixAppHead MMult)
                        [
                          VarDeclIntegral 2 HsPrimCInt,
                          VarDeclVar (Idx 0)]],
                  VarDeclIntegral
                    1
                    HsPrimCInt])))),
      varDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "F",
          commentLocation = Just
            "type_naturals.h:6:9",
          commentHeader = Just
            "type_naturals.h",
          commentChildren = []}},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "g",
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
                  (ClassTyCon AddTyCon)))
              [
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
                                    (IntLike (Int Signed)))))))
                          []],
                    TyVarTy (Idx 0)],
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
                                    (IntLike (Int Signed)))))))
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
                                (IntLike (Int Signed)))))))
                      []],
                TyVarTy (Idx 1)],
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
                                (IntLike (Int Signed)))))))
                      []],
                TyVarTy (Idx 0)]],
          quantTyBody = FunTy
            (TyVarTy (Idx 2))
            (FunTy
              (TyVarTy (Idx 0))
              (FunTy
                (TyVarTy (Idx 1))
                (TyConAppTy
                  (ATyCon
                    (FamilyTyCon AddResTyCon))
                  [
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
                                        (IntLike (Int Signed)))))))
                              []],
                        TyVarTy (Idx 0)],
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
                                        (IntLike (Int Signed)))))))
                              []],
                        TyVarTy (Idx 1)]])))}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "u")
          (VarDeclLambda
            (Lambda
              (NameHint "x")
              (VarDeclLambda
                (Lambda
                  (NameHint "y")
                  (VarDeclApp
                    (InfixAppHead MAdd)
                    [
                      VarDeclApp
                        (InfixAppHead MMult)
                        [
                          VarDeclIntegral 10 HsPrimCInt,
                          VarDeclVar (Idx 1)],
                      VarDeclApp
                        (InfixAppHead MMult)
                        [
                          VarDeclIntegral 16 HsPrimCInt,
                          VarDeclVar (Idx 0)]])))))),
      varDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "G",
          commentLocation = Just
            "type_naturals.h:7:9",
          commentHeader = Just
            "type_naturals.h",
          commentChildren = []}},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "k",
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
                      []],
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
                                    (IntLike (Int Signed)))))))
                          []],
                    TyVarTy (Idx 0)]],
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
                                (IntLike (Int Signed)))))))
                      []],
                TyVarTy (Idx 0)]],
          quantTyBody = FunTy
            (TyVarTy (Idx 0))
            (TyConAppTy
              (ATyCon
                (FamilyTyCon AddResTyCon))
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
                                (IntLike (Int Signed)))))))
                      []],
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
                                    (IntLike (Int Signed)))))))
                          []],
                    TyVarTy (Idx 0)]])}},
      varDeclBody = VarDeclApp
        (VarAppHead
          (HsName "@NsVar" "g"))
        [
          VarDeclDouble 11.77,
          VarDeclApp
            (VarAppHead
              (HsName "@NsVar" "f"))
            [
              VarDeclApp
                (VarAppHead
                  (HsName "@NsVar" "f"))
                [
                  VarDeclIntegral 2 HsPrimCInt,
                  VarDeclApp
                    (VarAppHead
                      (HsName "@NsVar" "m"))
                    []],
              VarDeclApp
                (VarAppHead
                  (HsName "@NsVar" "n"))
                []]],
      varDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "K",
          commentLocation = Just
            "type_naturals.h:8:9",
          commentHeader = Just
            "type_naturals.h",
          commentChildren = []}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Arr1",
      newtypeConstr = HsName
        "@NsConstr"
        "Arr1",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Arr1",
        fieldType = HsConstArray
          3
          (HsPrimType HsPrimCInt),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "type_naturals.h:10:9",
          declId = NamePair {
            nameC = Name "Arr1",
            nameHsIdent = HsIdentifier
              "Arr1"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "type_naturals.h",
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Arr1",
              newtypeField = HsName
                "@NsVar"
                "un_Arr1"},
            macroType = TypeConstArray
              3
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "Arr1",
          commentLocation = Just
            "type_naturals.h:10:9",
          commentHeader = Just
            "type_naturals.h",
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Arr1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Arr1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Arr1",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Arr2",
      newtypeConstr = HsName
        "@NsConstr"
        "Arr2",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Arr2",
        fieldType = HsConstArray
          8
          (HsPrimType HsPrimCInt),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "type_naturals.h:11:9",
          declId = NamePair {
            nameC = Name "Arr2",
            nameHsIdent = HsIdentifier
              "Arr2"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "type_naturals.h",
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Arr2",
              newtypeField = HsName
                "@NsVar"
                "un_Arr2"},
            macroType = TypeConstArray
              8
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "Arr2",
          commentLocation = Just
            "type_naturals.h:11:9",
          commentHeader = Just
            "type_naturals.h",
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Arr2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Arr2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Arr2",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Arr3",
      newtypeConstr = HsName
        "@NsConstr"
        "Arr3",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Arr3",
        fieldType = HsConstArray
          18
          (HsPrimType HsPrimCInt),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "type_naturals.h:12:9",
          declId = NamePair {
            nameC = Name "Arr3",
            nameHsIdent = HsIdentifier
              "Arr3"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "type_naturals.h",
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Arr3",
              newtypeField = HsName
                "@NsVar"
                "un_Arr3"},
            macroType = TypeConstArray
              18
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "Arr3",
          commentLocation = Just
            "type_naturals.h:12:9",
          commentHeader = Just
            "type_naturals.h",
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Arr3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Arr3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Arr3",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Arr4",
      newtypeConstr = HsName
        "@NsConstr"
        "Arr4",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Arr4",
        fieldType = HsConstArray
          252
          (HsPrimType HsPrimCInt),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "type_naturals.h:13:9",
          declId = NamePair {
            nameC = Name "Arr4",
            nameHsIdent = HsIdentifier
              "Arr4"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "type_naturals.h",
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Arr4",
              newtypeField = HsName
                "@NsVar"
                "un_Arr4"},
            macroType = TypeConstArray
              252
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "Arr4",
          commentLocation = Just
            "type_naturals.h:13:9",
          commentHeader = Just
            "type_naturals.h",
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Arr4",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Arr4",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Arr4",
      deriveInstanceComment =
      Nothing}]
