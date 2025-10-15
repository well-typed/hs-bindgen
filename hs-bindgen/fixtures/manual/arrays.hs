[
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Triplet",
      newtypeConstr = Name
        "@NsConstr"
        "Triplet",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Triplet",
        fieldType = HsConstArray
          3
          (HsPrimType HsPrimCInt),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "arrays.h:32:13",
          declId = NamePair {
            nameC = Name "triplet",
            nameHsIdent = Identifier
              "Triplet"},
          declOrigin = NameOriginInSource,
          declAliases = [Name "matrix"],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/arrays.h"],
              headerInclude =
              "manual/arrays.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Triplet",
              newtypeField = Name
                "@NsVar"
                "un_Triplet"},
            typedefType = TypeConstArray
              3
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "triplet",
          commentLocation = Just
            "arrays.h:32:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/arrays.h"],
              headerInclude =
              "manual/arrays.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Triplet",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Triplet",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Triplet",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Matrix",
      newtypeConstr = Name
        "@NsConstr"
        "Matrix",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Matrix",
        fieldType = HsConstArray
          3
          (HsTypRef
            (Name
              "@NsTypeConstr"
              "Triplet")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "arrays.h:34:17",
          declId = NamePair {
            nameC = Name "matrix",
            nameHsIdent = Identifier
              "Matrix"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/arrays.h"],
              headerInclude =
              "manual/arrays.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Matrix",
              newtypeField = Name
                "@NsVar"
                "un_Matrix"},
            typedefType = TypeConstArray
              3
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "triplet",
                    nameHsIdent = Identifier
                      "Triplet"}
                  (TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "matrix",
          commentLocation = Just
            "arrays.h:34:17",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/arrays.h"],
              headerInclude =
              "manual/arrays.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Matrix",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Matrix",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Matrix",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Triplet_ptrs",
      newtypeConstr = Name
        "@NsConstr"
        "Triplet_ptrs",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Triplet_ptrs",
        fieldType = HsIncompleteArray
          (HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin =
      Decl {
        declInfo =
        DeclInfo {
          declLoc = "arrays.h:44:15",
          declId = NamePair {
            nameC = Name "triplet_ptrs",
            nameHsIdent = Identifier
              "Triplet_ptrs"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/arrays.h"],
              headerInclude =
              "manual/arrays.h"},
          declComment =
          Just
            (Comment
              [
                Paragraph
                  [
                    TextContent
                      "A typedef representing a an array of unknown size, where each element is a",
                    TextContent
                      "pointer to an array of known size 3, where each element is an int."]])},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Triplet_ptrs",
              newtypeField = Name
                "@NsVar"
                "un_Triplet_ptrs"},
            typedefType =
            TypeIncompleteArray
              (TypePointer
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [Eq, Show],
      newtypeComment =
      Just
        Comment {
          commentTitle =
          Just
            [
              TextContent
                "A typedef representing a an array of unknown size, where each element is a",
              TextContent
                "pointer to an array of known size 3, where each element is an int."],
          commentOrigin = Just
            "triplet_ptrs",
          commentLocation = Just
            "arrays.h:44:15",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/arrays.h"],
              headerInclude =
              "manual/arrays.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Triplet_ptrs",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Triplet_ptrs",
      deriveInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "transpose_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "input"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "output"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_manualarrays_f3d0c8dd1a83b3d0",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_manualarrays_f3d0c8dd1a83b3d0 (triplet *arg1, triplet *arg2) { transpose(arg1, arg2); }",
          capiWrapperImport =
          "manual/arrays.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "input",
                  nameHsIdent = Identifier
                    "input"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "matrix",
                      nameHsIdent = Identifier
                        "Matrix"}
                    (TypeConstArray
                      3
                      (TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "triplet",
                            nameHsIdent = Identifier
                              "Triplet"}
                          (TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral
                                PrimInt
                                Signed))))))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "output",
                  nameHsIdent = Identifier
                    "output"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "matrix",
                    nameHsIdent = Identifier
                      "Matrix"}
                  (TypeConstArray
                    3
                    (TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "triplet",
                          nameHsIdent = Identifier
                            "Triplet"}
                        (TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral
                              PrimInt
                              Signed))))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Pointer-based API for",
              Identifier "transpose"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "transpose",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "input"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Matrix"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "input",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "output"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "output",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimUnit),
      functionDeclBody =
      `ELam "x" (ELam "x" (EApp (EApp (EGlobal ConstantArray_withPtr) (EBound 1)) (ELam "ptr" (EApp (EApp (EFree "transpose_wrapper") (EBound 0)) (EBound 1)))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "input",
                  nameHsIdent = Identifier
                    "input"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "matrix",
                      nameHsIdent = Identifier
                        "Matrix"}
                    (TypeConstArray
                      3
                      (TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "triplet",
                            nameHsIdent = Identifier
                              "Triplet"}
                          (TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral
                                PrimInt
                                Signed))))))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "output",
                  nameHsIdent = Identifier
                    "output"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "matrix",
                    nameHsIdent = Identifier
                      "Matrix"}
                  (TypeConstArray
                    3
                    (TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "triplet",
                          nameHsIdent = Identifier
                            "Triplet"}
                        (TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral
                              PrimInt
                              Signed))))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      functionDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "transpose",
          commentLocation = Just
            "arrays.h:36:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/arrays.h"],
              headerInclude =
              "manual/arrays.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "pretty_print_triplets",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsPtr
            (HsPtr
              (HsConstArray
                3
                (HsPrimType HsPrimCInt))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_manualarrays_27135f4747eb87db",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_manualarrays_27135f4747eb87db (signed int (**arg1)[3]) { pretty_print_triplets(arg1); }",
          capiWrapperImport =
          "manual/arrays.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "triplet_ptrs",
                    nameHsIdent = Identifier
                      "Triplet_ptrs"}
                  (TypeIncompleteArray
                    (TypePointer
                      (TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral
                            PrimInt
                            Signed)))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "A function that prints the given triplet_ptrs"],
          commentOrigin = Just
            "pretty_print_triplets",
          commentLocation = Just
            "arrays.h:50:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/arrays.h"],
              headerInclude =
              "manual/arrays.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "transpose_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "input"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "output"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_manualarrays_d7aa7016f1b951b2",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_manualarrays_d7aa7016f1b951b2 (triplet *arg1, triplet *arg2) { transpose(arg1, arg2); }",
          capiWrapperImport =
          "manual/arrays.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "input",
                  nameHsIdent = Identifier
                    "input"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "matrix",
                      nameHsIdent = Identifier
                        "Matrix"}
                    (TypeConstArray
                      3
                      (TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "triplet",
                            nameHsIdent = Identifier
                              "Triplet"}
                          (TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral
                                PrimInt
                                Signed))))))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "output",
                  nameHsIdent = Identifier
                    "output"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "matrix",
                    nameHsIdent = Identifier
                      "Matrix"}
                  (TypeConstArray
                    3
                    (TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "triplet",
                          nameHsIdent = Identifier
                            "Triplet"}
                        (TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral
                              PrimInt
                              Signed))))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Pointer-based API for",
              Identifier "transpose"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "transpose",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "input"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Matrix"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "input",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "output"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "output",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimUnit),
      functionDeclBody =
      `ELam "x" (ELam "x" (EApp (EApp (EGlobal ConstantArray_withPtr) (EBound 1)) (ELam "ptr" (EApp (EApp (EFree "transpose_wrapper") (EBound 0)) (EBound 1)))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "input",
                  nameHsIdent = Identifier
                    "input"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "matrix",
                      nameHsIdent = Identifier
                        "Matrix"}
                    (TypeConstArray
                      3
                      (TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "triplet",
                            nameHsIdent = Identifier
                              "Triplet"}
                          (TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral
                                PrimInt
                                Signed))))))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "output",
                  nameHsIdent = Identifier
                    "output"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "matrix",
                    nameHsIdent = Identifier
                      "Matrix"}
                  (TypeConstArray
                    3
                    (TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "triplet",
                          nameHsIdent = Identifier
                            "Triplet"}
                        (TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral
                              PrimInt
                              Signed))))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      functionDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "transpose",
          commentLocation = Just
            "arrays.h:36:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/arrays.h"],
              headerInclude =
              "manual/arrays.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "pretty_print_triplets",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsPtr
            (HsPtr
              (HsConstArray
                3
                (HsPrimType HsPrimCInt))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_manualarrays_bfd9ee42829f9ddb",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_manualarrays_bfd9ee42829f9ddb (signed int (**arg1)[3]) { pretty_print_triplets(arg1); }",
          capiWrapperImport =
          "manual/arrays.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "triplet_ptrs",
                    nameHsIdent = Identifier
                      "Triplet_ptrs"}
                  (TypeIncompleteArray
                    (TypePointer
                      (TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral
                            PrimInt
                            Signed)))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "A function that prints the given triplet_ptrs"],
          commentOrigin = Just
            "pretty_print_triplets",
          commentLocation = Just
            "arrays.h:50:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/arrays.h"],
              headerInclude =
              "manual/arrays.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_manualarrays_fc1dad225b555299",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "Matrix"))
              (HsFun
                (HsTypRef
                  (Name "@NsTypeConstr" "Matrix"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_manualarrays_fc1dad225b555299",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_transpose_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_manualarrays_fc1dad225b555299 (void)) (matrix const arg1, matrix arg2) { return &transpose; } ",
          capiWrapperImport =
          "manual/arrays.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeQualified
              TypeQualifierConst
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "matrix",
                    nameHsIdent = Identifier
                      "Matrix"}
                  (TypeConstArray
                    3
                    (TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "triplet",
                          nameHsIdent = Identifier
                            "Triplet"}
                        (TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral
                              PrimInt
                              Signed)))))))),
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "matrix",
                  nameHsIdent = Identifier
                    "Matrix"}
                (TypeConstArray
                  3
                  (TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "triplet",
                        nameHsIdent = Identifier
                          "Triplet"}
                      (TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral
                            PrimInt
                            Signed)))))))]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_manualarrays_0b485cea747ee35d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Triplet_ptrs"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_manualarrays_0b485cea747ee35d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_pretty_print_triplets_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_manualarrays_0b485cea747ee35d (void)) (triplet_ptrs arg1) { return &pretty_print_triplets; } ",
          capiWrapperImport =
          "manual/arrays.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "triplet_ptrs",
                  nameHsIdent = Identifier
                    "Triplet_ptrs"}
                (TypeIncompleteArray
                  (TypePointer
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed))))))]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_manualarrays_1693226264ba4aeb",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              1
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_manualarrays_1693226264ba4aeb",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_manualarrays_1693226264ba4aeb (void))[1] { return &arr1; } ",
          capiWrapperImport =
          "manual/arrays.h"},
      foreignImportOrigin = Global
        (TypeConstArray
          1
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_manualarrays_dafcf99a73b93389",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_manualarrays_dafcf99a73b93389",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr2_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_manualarrays_dafcf99a73b93389 (void))[3] { return &arr2; } ",
          capiWrapperImport =
          "manual/arrays.h"},
      foreignImportOrigin = Global
        (TypeConstArray
          3
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_manualarrays_ca1016acc3449dee",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsIncompleteArray
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_manualarrays_ca1016acc3449dee",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr3_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_manualarrays_ca1016acc3449dee (void))[] { return &arr3; } ",
          capiWrapperImport =
          "manual/arrays.h"},
      foreignImportOrigin = Global
        (TypeIncompleteArray
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_manualarrays_76857c9492b9374d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              3
              (HsConstArray
                3
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_manualarrays_76857c9492b9374d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_sudoku_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_manualarrays_76857c9492b9374d (void))[3][3] { return &sudoku; } ",
          capiWrapperImport =
          "manual/arrays.h"},
      foreignImportOrigin = Global
        (TypeConstArray
          3
          (TypeConstArray
            3
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_manualarrays_76f4df4c63822352",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsIncompleteArray
              (HsConstArray
                3
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_manualarrays_76f4df4c63822352",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_triplets_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_manualarrays_76f4df4c63822352 (void))[][3] { return &triplets; } ",
          capiWrapperImport =
          "manual/arrays.h"},
      foreignImportOrigin = Global
        (TypeIncompleteArray
          (TypeConstArray
            3
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_manualarrays_f5de5a56e036b125",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet_ptrs")))),
      foreignImportOrigName =
      "hs_bindgen_test_manualarrays_f5de5a56e036b125",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_global_triplet_ptrs_ptr */ __attribute__ ((const)) triplet_ptrs *hs_bindgen_test_manualarrays_f5de5a56e036b125 (void) { return &global_triplet_ptrs; } ",
          capiWrapperImport =
          "manual/arrays.h"},
      foreignImportOrigin = Global
        (TypeTypedef
          (TypedefRegular
            NamePair {
              nameC = Name "triplet_ptrs",
              nameHsIdent = Identifier
                "Triplet_ptrs"}
            (TypeIncompleteArray
              (TypePointer
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
