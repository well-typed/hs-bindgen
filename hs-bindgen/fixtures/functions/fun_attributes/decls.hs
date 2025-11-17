[
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "FILE",
      structConstr = Name
        "@NsConstr"
        "FILE",
      structFields = [],
      structOrigin =
      Just
        Decl {
          declInfo =
          DeclInfo {
            declLoc =
            "fun_attributes.h:7:9",
            declId = NamePair {
              nameC = Name "FILE",
              nameHsIdent = Identifier
                "FILE"},
            declOrigin = NameOriginGenerated
              (AnonId "fun_attributes.h:7:9"),
            declAliases = [Name "FILE"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["functions/fun_attributes.h"],
                headerInclude =
                "functions/fun_attributes.h"},
            declComment =
            Just
              (Comment
                [
                  Paragraph
                    [
                      TextContent
                        "Attributes on functions"],
                  Paragraph
                    [
                      TextContent
                        "Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html"]])},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "FILE"),
              structSizeof = 0,
              structAlignment = 1,
              structFields = [],
              structFlam = Nothing},
          declSpec = DeclSpec {
            declSpecC = Nothing,
            declSpecHs = Nothing}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment =
      Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Attributes on functions"],
          commentOrigin = Nothing,
          commentLocation = Just
            "fun_attributes.h:7:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren =
          [
            Paragraph
              [
                TextContent
                  "Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html"]]}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "FILE",
          structConstr = Name
            "@NsConstr"
            "FILE",
          structFields = [],
          structOrigin =
          Just
            Decl {
              declInfo =
              DeclInfo {
                declLoc =
                "fun_attributes.h:7:9",
                declId = NamePair {
                  nameC = Name "FILE",
                  nameHsIdent = Identifier
                    "FILE"},
                declOrigin = NameOriginGenerated
                  (AnonId "fun_attributes.h:7:9"),
                declAliases = [Name "FILE"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["functions/fun_attributes.h"],
                    headerInclude =
                    "functions/fun_attributes.h"},
                declComment =
                Just
                  (Comment
                    [
                      Paragraph
                        [
                          TextContent
                            "Attributes on functions"],
                      Paragraph
                        [
                          TextContent
                            "Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html"]])},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "FILE"),
                  structSizeof = 0,
                  structAlignment = 1,
                  structFields = [],
                  structFlam = Nothing},
              declSpec = DeclSpec {
                declSpecC = Nothing,
                declSpecHs = Nothing}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment =
          Just
            Comment {
              commentTitle = Just
                [
                  TextContent
                    "Attributes on functions"],
              commentOrigin = Nothing,
              commentLocation = Just
                "fun_attributes.h:7:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["functions/fun_attributes.h"],
                  headerInclude =
                  "functions/fun_attributes.h"},
              commentChildren =
              [
                Paragraph
                  [
                    TextContent
                      "Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html"]]}}
        StorableInstance {
          storableSizeOf = 0,
          storableAlignment = 1,
          storablePeek =
          Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "FILE",
                  structConstr = Name
                    "@NsConstr"
                    "FILE",
                  structFields = [],
                  structOrigin =
                  Just
                    Decl {
                      declInfo =
                      DeclInfo {
                        declLoc =
                        "fun_attributes.h:7:9",
                        declId = NamePair {
                          nameC = Name "FILE",
                          nameHsIdent = Identifier
                            "FILE"},
                        declOrigin = NameOriginGenerated
                          (AnonId "fun_attributes.h:7:9"),
                        declAliases = [Name "FILE"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["functions/fun_attributes.h"],
                            headerInclude =
                            "functions/fun_attributes.h"},
                        declComment =
                        Just
                          (Comment
                            [
                              Paragraph
                                [
                                  TextContent
                                    "Attributes on functions"],
                              Paragraph
                                [
                                  TextContent
                                    "Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html"]])},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "FILE"),
                          structSizeof = 0,
                          structAlignment = 1,
                          structFields = [],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment =
                  Just
                    Comment {
                      commentTitle = Just
                        [
                          TextContent
                            "Attributes on functions"],
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "fun_attributes.h:7:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["functions/fun_attributes.h"],
                          headerInclude =
                          "functions/fun_attributes.h"},
                      commentChildren =
                      [
                        Paragraph
                          [
                            TextContent
                              "Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html"]]}})
              []),
          storablePoke =
          Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "FILE",
                  structConstr = Name
                    "@NsConstr"
                    "FILE",
                  structFields = [],
                  structOrigin =
                  Just
                    Decl {
                      declInfo =
                      DeclInfo {
                        declLoc =
                        "fun_attributes.h:7:9",
                        declId = NamePair {
                          nameC = Name "FILE",
                          nameHsIdent = Identifier
                            "FILE"},
                        declOrigin = NameOriginGenerated
                          (AnonId "fun_attributes.h:7:9"),
                        declAliases = [Name "FILE"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["functions/fun_attributes.h"],
                            headerInclude =
                            "functions/fun_attributes.h"},
                        declComment =
                        Just
                          (Comment
                            [
                              Paragraph
                                [
                                  TextContent
                                    "Attributes on functions"],
                              Paragraph
                                [
                                  TextContent
                                    "Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html"]])},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "FILE"),
                          structSizeof = 0,
                          structAlignment = 1,
                          structFields = [],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment =
                  Just
                    Comment {
                      commentTitle = Just
                        [
                          TextContent
                            "Attributes on functions"],
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "fun_attributes.h:7:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["functions/fun_attributes.h"],
                          headerInclude =
                          "functions/fun_attributes.h"},
                      commentChildren =
                      [
                        Paragraph
                          [
                            TextContent
                              "Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html"]]}}
                (Add 0)
                (Seq [])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "FILE",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "FILE",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Size_t",
      newtypeConstr = Name
        "@NsConstr"
        "Size_t",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Size_t",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "fun_attributes.h:8:13",
          declId = NamePair {
            nameC = Name "size_t",
            nameHsIdent = Identifier
              "Size_t"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Size_t",
              newtypeField = Name
                "@NsVar"
                "un_Size_t"},
            typedefType = TypePrim
              (PrimIntegral PrimInt Signed)},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [
          Bits,
          Bounded,
          Enum,
          Eq,
          FiniteBits,
          Integral,
          Ix,
          Num,
          Ord,
          Read,
          Real,
          Show,
          Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "size_t",
          commentLocation = Just
            "fun_attributes.h:8:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass =
      FiniteBits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Size_t"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_Size_t",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Size_t"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_Size_t",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCInt,
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "__f1",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_8de545512324157b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_functionsfun_attributes_8de545512324157b (void)\n",
              "{\n",
              "  __f1();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "__f1",
          commentLocation = Just
            "fun_attributes.h:16:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "f1",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_a2f84d2570ef3892",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_functionsfun_attributes_a2f84d2570ef3892 (void)\n",
              "{\n",
              "  f1();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "f1",
          commentLocation = Just
            "fun_attributes.h:19:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "my_memalign",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Size_t"),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Size_t"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_cefda6b95395d829",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void *hs_bindgen_test_functionsfun_attributes_cefda6b95395d829 (\n",
              "  size_t arg1,\n",
              "  size_t arg2\n",
              ")\n",
              "{\n",
              "  return my_memalign(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = Identifier
                      "Size_t"}
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))),
            _×_
              Nothing
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = Identifier
                      "Size_t"}
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_memalign",
          commentLocation = Just
            "fun_attributes.h:23:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "my_calloc",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Size_t"),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Size_t"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_e25f06c3ebec2536",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void *hs_bindgen_test_functionsfun_attributes_e25f06c3ebec2536 (\n",
              "  size_t arg1,\n",
              "  size_t arg2\n",
              ")\n",
              "{\n",
              "  return my_calloc(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = Identifier
                      "Size_t"}
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))),
            _×_
              Nothing
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = Identifier
                      "Size_t"}
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_calloc",
          commentLocation = Just
            "fun_attributes.h:28:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "my_realloc",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsPtr
            (HsPrimType HsPrimVoid),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Size_t"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_51fa664668350a00",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void *hs_bindgen_test_functionsfun_attributes_51fa664668350a00 (\n",
              "  void *arg1,\n",
              "  size_t arg2\n",
              ")\n",
              "{\n",
              "  return my_realloc(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypePointer TypeVoid),
            _×_
              Nothing
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = Identifier
                      "Size_t"}
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_realloc",
          commentLocation = Just
            "fun_attributes.h:29:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "my_alloc1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Size_t"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_93a5d6b7d4e02c33",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void *hs_bindgen_test_functionsfun_attributes_93a5d6b7d4e02c33 (\n",
              "  size_t arg1\n",
              ")\n",
              "{\n",
              "  return my_alloc1(arg1);\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = Identifier
                      "Size_t"}
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_alloc1",
          commentLocation = Just
            "fun_attributes.h:34:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "my_alloc2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Size_t"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_c948fd867be322fa",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void *hs_bindgen_test_functionsfun_attributes_c948fd867be322fa (\n",
              "  size_t arg1\n",
              ")\n",
              "{\n",
              "  return my_alloc2(arg1);\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = Identifier
                      "Size_t"}
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_alloc2",
          commentLocation = Just
            "fun_attributes.h:35:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "square",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsPrimType HsPrimCInt),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_55e5eb89e54abf83",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_functionsfun_attributes_55e5eb89e54abf83 (\n",
              "  signed int arg1\n",
              ")\n",
              "{\n",
              "  return square(arg1);\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            HaskellPureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "square",
          commentLocation = Just
            "fun_attributes.h:39:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "old_fn_deprecated",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_e9647b9c99c68776",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_functionsfun_attributes_e9647b9c99c68776 (void)\n",
              "{\n",
              "  return old_fn_deprecated();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "old_fn_deprecated",
          commentLocation = Just
            "fun_attributes.h:48:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "my_dgettext",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "my_domain"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCChar),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "my_domain",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "my_format"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCChar),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "my_format",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCChar))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_023f7813e909f518",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "char *hs_bindgen_test_functionsfun_attributes_023f7813e909f518 (\n",
              "  char *arg1,\n",
              "  char const *arg2\n",
              ")\n",
              "{\n",
              "  return my_dgettext(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "my_domain",
                  nameHsIdent = Identifier
                    "my_domain"})
              (TypePointer
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "my_format",
                  nameHsIdent = Identifier
                    "my_format"})
              (TypePointer
                (TypeQualified
                  TypeQualifierConst
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypePrim
              (PrimChar
                (PrimSignImplicit
                  (Just Signed))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_dgettext",
          commentLocation = Just
            "fun_attributes.h:64:1",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fdopen",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsPtr
            (HsPrimType HsPrimCChar),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "FILE")))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_e39bbd59f1c96c14",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "FILE *hs_bindgen_test_functionsfun_attributes_e39bbd59f1c96c14 (\n",
              "  signed int arg1,\n",
              "  char const *arg2\n",
              ")\n",
              "{\n",
              "  return fdopen(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              Nothing
              (TypePointer
                (TypeQualified
                  TypeQualifierConst
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeTypedef
              (TypedefSquashed
                (Name "FILE")
                (TypeStruct
                  NamePair {
                    nameC = Name "FILE",
                    nameHsIdent = Identifier "FILE"}
                  (NameOriginGenerated
                    (AnonId
                      "fun_attributes.h:7:9")))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "fdopen",
          commentLocation = Just
            "fun_attributes.h:75:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "f2",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_1d043de05a457e90",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_functionsfun_attributes_1d043de05a457e90 (void)\n",
              "{\n",
              "  f2();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "f2",
          commentLocation = Just
            "fun_attributes.h:79:65",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "my_memcpy",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "dest"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimVoid),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "dest",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "src"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimVoid),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "src",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "len"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Size_t"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "len",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_4b3bfd2d72a2db5d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void *hs_bindgen_test_functionsfun_attributes_4b3bfd2d72a2db5d (\n",
              "  void *arg1,\n",
              "  void const *arg2,\n",
              "  size_t arg3\n",
              ")\n",
              "{\n",
              "  return my_memcpy(arg1, arg2, arg3);\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "dest",
                  nameHsIdent = Identifier
                    "dest"})
              (TypePointer TypeVoid),
            _×_
              (Just
                NamePair {
                  nameC = Name "src",
                  nameHsIdent = Identifier "src"})
              (TypePointer
                (TypeQualified
                  TypeQualifierConst
                  TypeVoid)),
            _×_
              (Just
                NamePair {
                  nameC = Name "len",
                  nameHsIdent = Identifier "len"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = Identifier
                      "Size_t"}
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_memcpy",
          commentLocation = Just
            "fun_attributes.h:85:1",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fatal",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_348fe595d62421cf",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_functionsfun_attributes_348fe595d62421cf (void)\n",
              "{\n",
              "  fatal();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "fatal",
          commentLocation = Just
            "fun_attributes.h:102:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hash",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsPtr
            (HsPrimType HsPrimCChar),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_e30754e2591f701a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_functionsfun_attributes_e30754e2591f701a (\n",
              "  char *arg1\n",
              ")\n",
              "{\n",
              "  return hash(arg1);\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypePointer
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))))],
          functionAttrs =
          FunctionAttributes
            CPureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "hash",
          commentLocation = Just
            "fun_attributes.h:110:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = [
            Paragraph
              [
                TextContent "Marked",
                Monospace
                  [
                    Bold
                      [
                        TextContent
                          "attribute((pure))"]]]]},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "mymalloc",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "len"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Size_t"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "len",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_f6f68a022a15937a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void *hs_bindgen_test_functionsfun_attributes_f6f68a022a15937a (\n",
              "  size_t arg1\n",
              ")\n",
              "{\n",
              "  return mymalloc(arg1);\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "len",
                  nameHsIdent = Identifier "len"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = Identifier
                      "Size_t"}
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "mymalloc",
          commentLocation = Just
            "fun_attributes.h:115:1",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "foobar",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_d1bf41da7ab64db1",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_functionsfun_attributes_d1bf41da7ab64db1 (void)\n",
              "{\n",
              "  foobar();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "foobar",
          commentLocation = Just
            "fun_attributes.h:119:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "core2_func",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_00405e83bcb9b271",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_functionsfun_attributes_00405e83bcb9b271 (void)\n",
              "{\n",
              "  return core2_func();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "core2_func",
          commentLocation = Just
            "fun_attributes.h:126:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "sse3_func",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_06e7d2f8bcf43684",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_functionsfun_attributes_06e7d2f8bcf43684 (void)\n",
              "{\n",
              "  return sse3_func();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "sse3_func",
          commentLocation = Just
            "fun_attributes.h:127:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "f3",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_e23eff1955ebb459",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_functionsfun_attributes_e23eff1955ebb459 (void)\n",
              "{\n",
              "  f3();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "f3",
          commentLocation = Just
            "fun_attributes.h:131:49",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fn",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_ef0eea5f61ef9228",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_functionsfun_attributes_ef0eea5f61ef9228 (void)\n",
              "{\n",
              "  return fn();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "fn",
          commentLocation = Just
            "fun_attributes.h:136:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "y",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_b007466f7ff1cf28",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_functionsfun_attributes_b007466f7ff1cf28 (void)\n",
              "{\n",
              "  return y();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "y",
          commentLocation = Just
            "fun_attributes.h:142:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "x1",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_8c9825e1b20a7ea1",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_functionsfun_attributes_8c9825e1b20a7ea1 (void)\n",
              "{\n",
              "  return x1();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "x1",
          commentLocation = Just
            "fun_attributes.h:145:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "x2",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_c80d61b7727dab77",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_functionsfun_attributes_c80d61b7727dab77 (void)\n",
              "{\n",
              "  return x2();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "x2",
          commentLocation = Just
            "fun_attributes.h:148:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "__f1",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_b44da51a357ae983",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_functionsfun_attributes_b44da51a357ae983 (void)\n",
              "{\n",
              "  __f1();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "__f1",
          commentLocation = Just
            "fun_attributes.h:16:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "f1",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_c1788128a5b1c813",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_functionsfun_attributes_c1788128a5b1c813 (void)\n",
              "{\n",
              "  f1();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "f1",
          commentLocation = Just
            "fun_attributes.h:19:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "my_memalign",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Size_t"),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Size_t"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_9ca07f6722bd48dc",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void *hs_bindgen_test_functionsfun_attributes_9ca07f6722bd48dc (\n",
              "  size_t arg1,\n",
              "  size_t arg2\n",
              ")\n",
              "{\n",
              "  return my_memalign(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = Identifier
                      "Size_t"}
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))),
            _×_
              Nothing
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = Identifier
                      "Size_t"}
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_memalign",
          commentLocation = Just
            "fun_attributes.h:23:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "my_calloc",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Size_t"),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Size_t"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_72df124450cc6d26",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void *hs_bindgen_test_functionsfun_attributes_72df124450cc6d26 (\n",
              "  size_t arg1,\n",
              "  size_t arg2\n",
              ")\n",
              "{\n",
              "  return my_calloc(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = Identifier
                      "Size_t"}
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))),
            _×_
              Nothing
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = Identifier
                      "Size_t"}
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_calloc",
          commentLocation = Just
            "fun_attributes.h:28:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "my_realloc",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsPtr
            (HsPrimType HsPrimVoid),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Size_t"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_effc1fd567613950",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void *hs_bindgen_test_functionsfun_attributes_effc1fd567613950 (\n",
              "  void *arg1,\n",
              "  size_t arg2\n",
              ")\n",
              "{\n",
              "  return my_realloc(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypePointer TypeVoid),
            _×_
              Nothing
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = Identifier
                      "Size_t"}
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_realloc",
          commentLocation = Just
            "fun_attributes.h:29:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "my_alloc1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Size_t"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_b3544e53af074ef1",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void *hs_bindgen_test_functionsfun_attributes_b3544e53af074ef1 (\n",
              "  size_t arg1\n",
              ")\n",
              "{\n",
              "  return my_alloc1(arg1);\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = Identifier
                      "Size_t"}
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_alloc1",
          commentLocation = Just
            "fun_attributes.h:34:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "my_alloc2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Size_t"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_0b659f90fec40284",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void *hs_bindgen_test_functionsfun_attributes_0b659f90fec40284 (\n",
              "  size_t arg1\n",
              ")\n",
              "{\n",
              "  return my_alloc2(arg1);\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = Identifier
                      "Size_t"}
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_alloc2",
          commentLocation = Just
            "fun_attributes.h:35:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "square",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsPrimType HsPrimCInt),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_cb3c687f16289bb3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_functionsfun_attributes_cb3c687f16289bb3 (\n",
              "  signed int arg1\n",
              ")\n",
              "{\n",
              "  return square(arg1);\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            HaskellPureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "square",
          commentLocation = Just
            "fun_attributes.h:39:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "old_fn_deprecated",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_c48f18f4f06068eb",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_functionsfun_attributes_c48f18f4f06068eb (void)\n",
              "{\n",
              "  return old_fn_deprecated();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "old_fn_deprecated",
          commentLocation = Just
            "fun_attributes.h:48:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "my_dgettext",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "my_domain"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCChar),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "my_domain",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "my_format"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCChar),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "my_format",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCChar))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_d492bd76e82890da",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "char *hs_bindgen_test_functionsfun_attributes_d492bd76e82890da (\n",
              "  char *arg1,\n",
              "  char const *arg2\n",
              ")\n",
              "{\n",
              "  return my_dgettext(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "my_domain",
                  nameHsIdent = Identifier
                    "my_domain"})
              (TypePointer
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "my_format",
                  nameHsIdent = Identifier
                    "my_format"})
              (TypePointer
                (TypeQualified
                  TypeQualifierConst
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypePrim
              (PrimChar
                (PrimSignImplicit
                  (Just Signed))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_dgettext",
          commentLocation = Just
            "fun_attributes.h:64:1",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fdopen",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsPtr
            (HsPrimType HsPrimCChar),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "FILE")))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_3c91a267bd66cc10",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "FILE *hs_bindgen_test_functionsfun_attributes_3c91a267bd66cc10 (\n",
              "  signed int arg1,\n",
              "  char const *arg2\n",
              ")\n",
              "{\n",
              "  return fdopen(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              Nothing
              (TypePointer
                (TypeQualified
                  TypeQualifierConst
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeTypedef
              (TypedefSquashed
                (Name "FILE")
                (TypeStruct
                  NamePair {
                    nameC = Name "FILE",
                    nameHsIdent = Identifier "FILE"}
                  (NameOriginGenerated
                    (AnonId
                      "fun_attributes.h:7:9")))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "fdopen",
          commentLocation = Just
            "fun_attributes.h:75:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "f2",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_14361e995fb5684a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_functionsfun_attributes_14361e995fb5684a (void)\n",
              "{\n",
              "  f2();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "f2",
          commentLocation = Just
            "fun_attributes.h:79:65",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "my_memcpy",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "dest"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimVoid),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "dest",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "src"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimVoid),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "src",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "len"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Size_t"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "len",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_e8c4a96cefd6117e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void *hs_bindgen_test_functionsfun_attributes_e8c4a96cefd6117e (\n",
              "  void *arg1,\n",
              "  void const *arg2,\n",
              "  size_t arg3\n",
              ")\n",
              "{\n",
              "  return my_memcpy(arg1, arg2, arg3);\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "dest",
                  nameHsIdent = Identifier
                    "dest"})
              (TypePointer TypeVoid),
            _×_
              (Just
                NamePair {
                  nameC = Name "src",
                  nameHsIdent = Identifier "src"})
              (TypePointer
                (TypeQualified
                  TypeQualifierConst
                  TypeVoid)),
            _×_
              (Just
                NamePair {
                  nameC = Name "len",
                  nameHsIdent = Identifier "len"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = Identifier
                      "Size_t"}
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_memcpy",
          commentLocation = Just
            "fun_attributes.h:85:1",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fatal",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_64aa41e835dbb892",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_functionsfun_attributes_64aa41e835dbb892 (void)\n",
              "{\n",
              "  fatal();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "fatal",
          commentLocation = Just
            "fun_attributes.h:102:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hash",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsPtr
            (HsPrimType HsPrimCChar),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_88887d4b5f42f079",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_functionsfun_attributes_88887d4b5f42f079 (\n",
              "  char *arg1\n",
              ")\n",
              "{\n",
              "  return hash(arg1);\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypePointer
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))))],
          functionAttrs =
          FunctionAttributes
            CPureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "hash",
          commentLocation = Just
            "fun_attributes.h:110:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = [
            Paragraph
              [
                TextContent "Marked",
                Monospace
                  [
                    Bold
                      [
                        TextContent
                          "attribute((pure))"]]]]},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "mymalloc",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "len"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Size_t"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "len",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_31e6e14ecb251fa2",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void *hs_bindgen_test_functionsfun_attributes_31e6e14ecb251fa2 (\n",
              "  size_t arg1\n",
              ")\n",
              "{\n",
              "  return mymalloc(arg1);\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "len",
                  nameHsIdent = Identifier "len"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = Identifier
                      "Size_t"}
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "mymalloc",
          commentLocation = Just
            "fun_attributes.h:115:1",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "foobar",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_bb77a71513994934",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_functionsfun_attributes_bb77a71513994934 (void)\n",
              "{\n",
              "  foobar();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "foobar",
          commentLocation = Just
            "fun_attributes.h:119:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "core2_func",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_640ec5b51b0819d1",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_functionsfun_attributes_640ec5b51b0819d1 (void)\n",
              "{\n",
              "  return core2_func();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "core2_func",
          commentLocation = Just
            "fun_attributes.h:126:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "sse3_func",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_a1f7636643d63586",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_functionsfun_attributes_a1f7636643d63586 (void)\n",
              "{\n",
              "  return sse3_func();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "sse3_func",
          commentLocation = Just
            "fun_attributes.h:127:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "f3",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_2bef032cbe15ffd0",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_functionsfun_attributes_2bef032cbe15ffd0 (void)\n",
              "{\n",
              "  f3();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "f3",
          commentLocation = Just
            "fun_attributes.h:131:49",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fn",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_8f406104a21ff66e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_functionsfun_attributes_8f406104a21ff66e (void)\n",
              "{\n",
              "  return fn();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "fn",
          commentLocation = Just
            "fun_attributes.h:136:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "y",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_4beb0cbf65b462bd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_functionsfun_attributes_4beb0cbf65b462bd (void)\n",
              "{\n",
              "  return y();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "y",
          commentLocation = Just
            "fun_attributes.h:142:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "x1",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_ac7386c785058f4d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_functionsfun_attributes_ac7386c785058f4d (void)\n",
              "{\n",
              "  return x1();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "x1",
          commentLocation = Just
            "fun_attributes.h:145:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "x2",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_b6f428ed915f03cc",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_functionsfun_attributes_b6f428ed915f03cc (void)\n",
              "{\n",
              "  return x2();\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "x2",
          commentLocation = Just
            "fun_attributes.h:148:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["functions/fun_attributes.h"],
              headerInclude =
              "functions/fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_functionsfun_attributes_7003b306f73c174b",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_7003b306f73c174b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get___f1_ptr */\n",
              "__attribute__ ((const))\n",
              "void (*hs_bindgen_test_functionsfun_attributes_7003b306f73c174b (void)) (void)\n",
              "{\n",
              "  return &__f1;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun [] TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_functionsfun_attributes_5469bdc0395f86c1",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_5469bdc0395f86c1",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_f1_ptr */\n",
              "__attribute__ ((const))\n",
              "void (*hs_bindgen_test_functionsfun_attributes_5469bdc0395f86c1 (void)) (void)\n",
              "{\n",
              "  return &f1;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun [] TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_functionsfun_attributes_b3c956e53724162c",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "Size_t"))
              (HsFun
                (HsTypRef
                  (Name "@NsTypeConstr" "Size_t"))
                (HsIO
                  (HsPtr
                    (HsPrimType HsPrimVoid))))))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_b3c956e53724162c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_my_memalign_ptr */\n",
              "__attribute__ ((const))\n",
              "void *(*hs_bindgen_test_functionsfun_attributes_b3c956e53724162c (void)) (\n",
              "  size_t arg1,\n",
              "  size_t arg2\n",
              ")\n",
              "{\n",
              "  return &my_memalign;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "size_t",
                  nameHsIdent = Identifier
                    "Size_t"}
                (TypePrim
                  (PrimIntegral PrimInt Signed))),
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "size_t",
                  nameHsIdent = Identifier
                    "Size_t"}
                (TypePrim
                  (PrimIntegral PrimInt Signed)))]
          (TypePointer TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_functionsfun_attributes_733646ca96f39979",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "Size_t"))
              (HsFun
                (HsTypRef
                  (Name "@NsTypeConstr" "Size_t"))
                (HsIO
                  (HsPtr
                    (HsPrimType HsPrimVoid))))))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_733646ca96f39979",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_my_calloc_ptr */\n",
              "__attribute__ ((const))\n",
              "void *(*hs_bindgen_test_functionsfun_attributes_733646ca96f39979 (void)) (\n",
              "  size_t arg1,\n",
              "  size_t arg2\n",
              ")\n",
              "{\n",
              "  return &my_calloc;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "size_t",
                  nameHsIdent = Identifier
                    "Size_t"}
                (TypePrim
                  (PrimIntegral PrimInt Signed))),
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "size_t",
                  nameHsIdent = Identifier
                    "Size_t"}
                (TypePrim
                  (PrimIntegral PrimInt Signed)))]
          (TypePointer TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_functionsfun_attributes_94e8271f186110fd",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr (HsPrimType HsPrimVoid))
              (HsFun
                (HsTypRef
                  (Name "@NsTypeConstr" "Size_t"))
                (HsIO
                  (HsPtr
                    (HsPrimType HsPrimVoid))))))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_94e8271f186110fd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_my_realloc_ptr */\n",
              "__attribute__ ((const))\n",
              "void *(*hs_bindgen_test_functionsfun_attributes_94e8271f186110fd (void)) (\n",
              "  void *arg1,\n",
              "  size_t arg2\n",
              ")\n",
              "{\n",
              "  return &my_realloc;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer TypeVoid,
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "size_t",
                  nameHsIdent = Identifier
                    "Size_t"}
                (TypePrim
                  (PrimIntegral PrimInt Signed)))]
          (TypePointer TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_functionsfun_attributes_48d9862d70f58e70",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "Size_t"))
              (HsIO
                (HsPtr
                  (HsPrimType HsPrimVoid)))))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_48d9862d70f58e70",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_my_alloc1_ptr */\n",
              "__attribute__ ((const))\n",
              "void *(*hs_bindgen_test_functionsfun_attributes_48d9862d70f58e70 (void)) (\n",
              "  size_t arg1\n",
              ")\n",
              "{\n",
              "  return &my_alloc1;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "size_t",
                  nameHsIdent = Identifier
                    "Size_t"}
                (TypePrim
                  (PrimIntegral PrimInt Signed)))]
          (TypePointer TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_functionsfun_attributes_17a11fd10dc57357",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "Size_t"))
              (HsIO
                (HsPtr
                  (HsPrimType HsPrimVoid)))))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_17a11fd10dc57357",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_my_alloc2_ptr */\n",
              "__attribute__ ((const))\n",
              "void *(*hs_bindgen_test_functionsfun_attributes_17a11fd10dc57357 (void)) (\n",
              "  size_t arg1\n",
              ")\n",
              "{\n",
              "  return &my_alloc2;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "size_t",
                  nameHsIdent = Identifier
                    "Size_t"}
                (TypePrim
                  (PrimIntegral PrimInt Signed)))]
          (TypePointer TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_functionsfun_attributes_c41111f40a04cdc9",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_c41111f40a04cdc9",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_square_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*hs_bindgen_test_functionsfun_attributes_c41111f40a04cdc9 (void)) (\n",
              "  signed int arg1\n",
              ")\n",
              "{\n",
              "  return &square;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed)]
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
        "hs_bindgen_test_functionsfun_attributes_17f68fdc3f464b20",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_17f68fdc3f464b20",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_old_fn_deprecated_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*hs_bindgen_test_functionsfun_attributes_17f68fdc3f464b20 (void)) (void)\n",
              "{\n",
              "  return &old_fn_deprecated;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
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
        "hs_bindgen_test_functionsfun_attributes_a0be4f488601c252",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr (HsPrimType HsPrimCChar))
              (HsFun
                (HsPtr (HsPrimType HsPrimCChar))
                (HsIO
                  (HsPtr
                    (HsPrimType HsPrimCChar))))))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_a0be4f488601c252",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_my_dgettext_ptr */\n",
              "__attribute__ ((const))\n",
              "char *(*hs_bindgen_test_functionsfun_attributes_a0be4f488601c252 (void)) (\n",
              "  char *arg1,\n",
              "  char const *arg2\n",
              ")\n",
              "{\n",
              "  return &my_dgettext;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed)))),
            TypePointer
              (TypeQualified
                TypeQualifierConst
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))))]
          (TypePointer
            (TypePrim
              (PrimChar
                (PrimSignImplicit
                  (Just Signed)))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_functionsfun_attributes_2b987c3b5c01a326",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsFun
                (HsPtr (HsPrimType HsPrimCChar))
                (HsIO
                  (HsPtr
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "FILE")))))))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_2b987c3b5c01a326",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_fdopen_ptr */\n",
              "__attribute__ ((const))\n",
              "FILE *(*hs_bindgen_test_functionsfun_attributes_2b987c3b5c01a326 (void)) (\n",
              "  signed int arg1,\n",
              "  char const *arg2\n",
              ")\n",
              "{\n",
              "  return &fdopen;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypePointer
              (TypeQualified
                TypeQualifierConst
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))))]
          (TypePointer
            (TypeTypedef
              (TypedefSquashed
                (Name "FILE")
                (TypeStruct
                  NamePair {
                    nameC = Name "FILE",
                    nameHsIdent = Identifier "FILE"}
                  (NameOriginGenerated
                    (AnonId
                      "fun_attributes.h:7:9"))))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_functionsfun_attributes_490ca7e8c8282a69",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_490ca7e8c8282a69",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_f2_ptr */\n",
              "__attribute__ ((const))\n",
              "void (*hs_bindgen_test_functionsfun_attributes_490ca7e8c8282a69 (void)) (void)\n",
              "{\n",
              "  return &f2;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun [] TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_functionsfun_attributes_e2e8b5d5ac435de8",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr (HsPrimType HsPrimVoid))
              (HsFun
                (HsPtr (HsPrimType HsPrimVoid))
                (HsFun
                  (HsTypRef
                    (Name "@NsTypeConstr" "Size_t"))
                  (HsIO
                    (HsPtr
                      (HsPrimType HsPrimVoid)))))))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_e2e8b5d5ac435de8",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_my_memcpy_ptr */\n",
              "__attribute__ ((const))\n",
              "void *(*hs_bindgen_test_functionsfun_attributes_e2e8b5d5ac435de8 (void)) (\n",
              "  void *arg1,\n",
              "  void const *arg2,\n",
              "  size_t arg3\n",
              ")\n",
              "{\n",
              "  return &my_memcpy;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer TypeVoid,
            TypePointer
              (TypeQualified
                TypeQualifierConst
                TypeVoid),
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "size_t",
                  nameHsIdent = Identifier
                    "Size_t"}
                (TypePrim
                  (PrimIntegral PrimInt Signed)))]
          (TypePointer TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_functionsfun_attributes_ea0bb781f9eca7f5",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_ea0bb781f9eca7f5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_fatal_ptr */\n",
              "__attribute__ ((const))\n",
              "void (*hs_bindgen_test_functionsfun_attributes_ea0bb781f9eca7f5 (void)) (void)\n",
              "{\n",
              "  return &fatal;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun [] TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_functionsfun_attributes_4de9606eb9c5dd01",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr (HsPrimType HsPrimCChar))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_4de9606eb9c5dd01",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_hash_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*hs_bindgen_test_functionsfun_attributes_4de9606eb9c5dd01 (void)) (\n",
              "  char *arg1\n",
              ")\n",
              "{\n",
              "  return &hash;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))))]
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
        "hs_bindgen_test_functionsfun_attributes_4ce141c884649d49",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "Size_t"))
              (HsIO
                (HsPtr
                  (HsPrimType HsPrimVoid)))))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_4ce141c884649d49",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_mymalloc_ptr */\n",
              "__attribute__ ((const))\n",
              "void *(*hs_bindgen_test_functionsfun_attributes_4ce141c884649d49 (void)) (\n",
              "  size_t arg1\n",
              ")\n",
              "{\n",
              "  return &mymalloc;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "size_t",
                  nameHsIdent = Identifier
                    "Size_t"}
                (TypePrim
                  (PrimIntegral PrimInt Signed)))]
          (TypePointer TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_functionsfun_attributes_5c243ced544ab0aa",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_5c243ced544ab0aa",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_foobar_ptr */\n",
              "__attribute__ ((const))\n",
              "void (*hs_bindgen_test_functionsfun_attributes_5c243ced544ab0aa (void)) (void)\n",
              "{\n",
              "  return &foobar;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun [] TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_functionsfun_attributes_14ef55245a14f816",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_14ef55245a14f816",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_core2_func_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*hs_bindgen_test_functionsfun_attributes_14ef55245a14f816 (void)) (void)\n",
              "{\n",
              "  return &core2_func;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
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
        "hs_bindgen_test_functionsfun_attributes_72956748bb6eee67",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_72956748bb6eee67",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_sse3_func_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*hs_bindgen_test_functionsfun_attributes_72956748bb6eee67 (void)) (void)\n",
              "{\n",
              "  return &sse3_func;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
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
        "hs_bindgen_test_functionsfun_attributes_38506a9ac5626bf2",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_38506a9ac5626bf2",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_f3_ptr */\n",
              "__attribute__ ((const))\n",
              "void (*hs_bindgen_test_functionsfun_attributes_38506a9ac5626bf2 (void)) (void)\n",
              "{\n",
              "  return &f3;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun [] TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_functionsfun_attributes_5929da82079150d1",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_5929da82079150d1",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_fn_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*hs_bindgen_test_functionsfun_attributes_5929da82079150d1 (void)) (void)\n",
              "{\n",
              "  return &fn;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
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
        "hs_bindgen_test_functionsfun_attributes_7bcb4a1873e6ece6",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_7bcb4a1873e6ece6",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_y_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*hs_bindgen_test_functionsfun_attributes_7bcb4a1873e6ece6 (void)) (void)\n",
              "{\n",
              "  return &y;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
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
        "hs_bindgen_test_functionsfun_attributes_11098262b345351a",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_11098262b345351a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_x1_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*hs_bindgen_test_functionsfun_attributes_11098262b345351a (void)) (void)\n",
              "{\n",
              "  return &x1;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
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
        "hs_bindgen_test_functionsfun_attributes_0d19f83087f278f9",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_0d19f83087f278f9",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_x2_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*hs_bindgen_test_functionsfun_attributes_0d19f83087f278f9 (void)) (void)\n",
              "{\n",
              "  return &x2;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
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
        "hs_bindgen_test_functionsfun_attributes_cdc30ae5fb72cd6e",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsfun_attributes_cdc30ae5fb72cd6e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_i_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int *hs_bindgen_test_functionsfun_attributes_cdc30ae5fb72cd6e (void)\n",
              "{\n",
              "  return &i;\n",
              "}"],
          capiWrapperImport =
          "functions/fun_attributes.h"},
      foreignImportOrigin = Global
        (TypePrim
          (PrimIntegral PrimInt Signed)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
