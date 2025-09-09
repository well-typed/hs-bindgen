[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Thing",
      structConstr = HsName
        "@NsConstr"
        "Thing",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "thing_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "struct_arg.h:3:9",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier
                    "thing_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Just
                "struct_arg.h:3:9",
              commentHeader = Just
                "struct_arg.h",
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "struct_arg.h:2:8",
            declId = NamePair {
              nameC = Name "thing",
              nameHsIdent = HsIdentifier
                "Thing"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "struct_arg.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Thing"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "struct_arg.h:3:9",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "thing_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            TypeSpec {
              typeSpecModule = Nothing,
              typeSpecIdentifier = Nothing,
              typeSpecInstances = Map.fromList
                []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "thing",
          commentLocation = Just
            "struct_arg.h:2:8",
          commentHeader = Just
            "struct_arg.h",
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Thing",
          structConstr = HsName
            "@NsConstr"
            "Thing",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "thing_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "struct_arg.h:3:9",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "thing_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "x",
                  commentLocation = Just
                    "struct_arg.h:3:9",
                  commentHeader = Just
                    "struct_arg.h",
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "struct_arg.h:2:8",
                declId = NamePair {
                  nameC = Name "thing",
                  nameHsIdent = HsIdentifier
                    "Thing"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "struct_arg.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Thing"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "struct_arg.h:3:9",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "thing_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                TypeSpec {
                  typeSpecModule = Nothing,
                  typeSpecIdentifier = Nothing,
                  typeSpecInstances = Map.fromList
                    []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "thing",
              commentLocation = Just
                "struct_arg.h:2:8",
              commentHeader = Just
                "struct_arg.h",
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 4,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Thing",
                  structConstr = HsName
                    "@NsConstr"
                    "Thing",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "thing_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "struct_arg.h:3:9",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "thing_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "struct_arg.h:3:9",
                          commentHeader = Just
                            "struct_arg.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "struct_arg.h:2:8",
                        declId = NamePair {
                          nameC = Name "thing",
                          nameHsIdent = HsIdentifier
                            "Thing"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "struct_arg.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Thing"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "struct_arg.h:3:9",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "thing_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "thing",
                      commentLocation = Just
                        "struct_arg.h:2:8",
                      commentHeader = Just
                        "struct_arg.h",
                      commentChildren = []}})
              [PeekByteOff (Idx 0) 0]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Thing",
                  structConstr = HsName
                    "@NsConstr"
                    "Thing",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "thing_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "struct_arg.h:3:9",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "thing_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "struct_arg.h:3:9",
                          commentHeader = Just
                            "struct_arg.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "struct_arg.h:2:8",
                        declId = NamePair {
                          nameC = Name "thing",
                          nameHsIdent = HsIdentifier
                            "Thing"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "struct_arg.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Thing"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "struct_arg.h:3:9",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "thing_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "thing",
                      commentLocation = Just
                        "struct_arg.h:2:8",
                      commentHeader = Just
                        "struct_arg.h",
                      commentChildren = []}}
                (Add 1)
                (Seq
                  [
                    PokeByteOff
                      (Idx 2)
                      0
                      (Idx 0)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Thing",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Thing",
      deriveInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "thing_fun_1_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Thing")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_be997777eb388096",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_struct_arg_be997777eb388096 (struct thing *arg1) { return thing_fun_1(*arg1); }",
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypeStruct
                NamePair {
                  nameC = Name "thing",
                  nameHsIdent = HsIdentifier
                    "Thing"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "thing_fun_1",
          commentLocation = Just
            "struct_arg.h:6:5",
          commentHeader = Just
            "struct_arg.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_struct_arg_d5cf000d627eba66",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Thing"))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_d5cf000d627eba66",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_thing_fun_1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_struct_arg_d5cf000d627eba66 (void)) (struct thing arg1) { return &thing_fun_1; } ",
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeStruct
              NamePair {
                nameC = Name "thing",
                nameHsIdent = HsIdentifier
                  "Thing"}
              NameOriginInSource]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "thing_fun_1",
          commentLocation = Just
            "struct_arg.h:6:5",
          commentHeader = Just
            "struct_arg.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "thing_fun_2_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      HeapResultType
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Thing"))),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_c719e5e844a53956",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_struct_arg_c719e5e844a53956 (signed int arg1, struct thing *arg2) { *arg2 = thing_fun_2(arg1); }",
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeStruct
            NamePair {
              nameC = Name "thing",
              nameHsIdent = HsIdentifier
                "Thing"}
            NameOriginInSource},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "thing_fun_2",
          commentLocation = Just
            "struct_arg.h:7:14",
          commentHeader = Just
            "struct_arg.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_struct_arg_c5543d9dadeca704",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Thing")))))),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_c5543d9dadeca704",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_thing_fun_2_ptr */ __attribute__ ((const)) struct thing (*hs_bindgen_test_struct_arg_c5543d9dadeca704 (void)) (signed int arg1) { return &thing_fun_2; } ",
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed)]
          (TypeStruct
            NamePair {
              nameC = Name "thing",
              nameHsIdent = HsIdentifier
                "Thing"}
            NameOriginInSource)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "thing_fun_2",
          commentLocation = Just
            "struct_arg.h:7:14",
          commentHeader = Just
            "struct_arg.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "thing_fun_3a_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "y"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Thing")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "z"),
          functionParameterType =
          HsPrimType HsPrimCDouble,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "z",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      HeapResultType
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Thing"))),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_9540300ca2ef6349",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_struct_arg_9540300ca2ef6349 (signed int arg1, struct thing *arg2, double arg3, struct thing *arg4) { *arg4 = thing_fun_3a(arg1, *arg2, arg3); }",
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier "y"})
              (TypeStruct
                NamePair {
                  nameC = Name "thing",
                  nameHsIdent = HsIdentifier
                    "Thing"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "z",
                  nameHsIdent = HsIdentifier "z"})
              (TypePrim
                (PrimFloating PrimDouble))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeStruct
            NamePair {
              nameC = Name "thing",
              nameHsIdent = HsIdentifier
                "Thing"}
            NameOriginInSource},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "thing_fun_3a",
          commentLocation = Just
            "struct_arg.h:9:14",
          commentHeader = Just
            "struct_arg.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_struct_arg_6f4d585feed7ca5e",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsFun
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Thing"))
                (HsFun
                  (HsPrimType HsPrimCDouble)
                  (HsIO
                    (HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Thing")))))))),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_6f4d585feed7ca5e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_thing_fun_3a_ptr */ __attribute__ ((const)) struct thing (*hs_bindgen_test_struct_arg_6f4d585feed7ca5e (void)) (signed int arg1, struct thing arg2, double arg3) { return &thing_fun_3a; } ",
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypeStruct
              NamePair {
                nameC = Name "thing",
                nameHsIdent = HsIdentifier
                  "Thing"}
              NameOriginInSource,
            TypePrim
              (PrimFloating PrimDouble)]
          (TypeStruct
            NamePair {
              nameC = Name "thing",
              nameHsIdent = HsIdentifier
                "Thing"}
            NameOriginInSource)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "thing_fun_3a",
          commentLocation = Just
            "struct_arg.h:9:14",
          commentHeader = Just
            "struct_arg.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "thing_fun_3b_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "y"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Thing")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "z"),
          functionParameterType =
          HsPrimType HsPrimCDouble,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "z",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCChar)),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_f6f54b421741a2de",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "char hs_bindgen_test_struct_arg_f6f54b421741a2de (signed int arg1, struct thing *arg2, double arg3) { return thing_fun_3b(arg1, *arg2, arg3); }",
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier "y"})
              (TypeStruct
                NamePair {
                  nameC = Name "thing",
                  nameHsIdent = HsIdentifier
                    "Thing"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "z",
                  nameHsIdent = HsIdentifier "z"})
              (TypePrim
                (PrimFloating PrimDouble))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimChar
              (PrimSignImplicit
                (Just Signed)))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "thing_fun_3b",
          commentLocation = Just
            "struct_arg.h:10:6",
          commentHeader = Just
            "struct_arg.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_struct_arg_ef6a607b6432889d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsFun
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Thing"))
                (HsFun
                  (HsPrimType HsPrimCDouble)
                  (HsIO
                    (HsPrimType HsPrimCChar))))))),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_ef6a607b6432889d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_thing_fun_3b_ptr */ __attribute__ ((const)) char (*hs_bindgen_test_struct_arg_ef6a607b6432889d (void)) (signed int arg1, struct thing arg2, double arg3) { return &thing_fun_3b; } ",
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypeStruct
              NamePair {
                nameC = Name "thing",
                nameHsIdent = HsIdentifier
                  "Thing"}
              NameOriginInSource,
            TypePrim
              (PrimFloating PrimDouble)]
          (TypePrim
            (PrimChar
              (PrimSignImplicit
                (Just Signed))))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "thing_fun_3b",
          commentLocation = Just
            "struct_arg.h:10:6",
          commentHeader = Just
            "struct_arg.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
