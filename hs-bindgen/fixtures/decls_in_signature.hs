[
  DeclEmpty
    EmptyData {
      emptyDataName = HsName
        "@NsTypeConstr"
        "Opaque",
      emptyDataOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "decls_in_signature.h:2:8",
          declId = NamePair {
            nameC = Name "opaque",
            nameHsIdent = HsIdentifier
              "Opaque"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["decls_in_signature.h"],
              headerInclude =
              "decls_in_signature.h"},
          declComment = Nothing},
        declKind = OpaqueStruct,
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      emptyDataComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "opaque",
          commentLocation = Just
            "decls_in_signature.h:2:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["decls_in_signature.h"],
              headerInclude =
              "decls_in_signature.h"},
          commentChildren = []}},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Outside",
      structConstr = HsName
        "@NsConstr"
        "Outside",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "outside_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "decls_in_signature.h:4:7",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier
                    "outside_x"},
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
                "decls_in_signature.h:4:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["decls_in_signature.h"],
                  headerInclude =
                  "decls_in_signature.h"},
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "outside_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "decls_in_signature.h:5:7",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier
                    "outside_y"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Just
                "decls_in_signature.h:5:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["decls_in_signature.h"],
                  headerInclude =
                  "decls_in_signature.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "decls_in_signature.h:3:8",
            declId = NamePair {
              nameC = Name "outside",
              nameHsIdent = HsIdentifier
                "Outside"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["decls_in_signature.h"],
                headerInclude =
                "decls_in_signature.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Outside"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "decls_in_signature.h:4:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "outside_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "decls_in_signature.h:5:7",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "outside_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
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
          commentOrigin = Just "outside",
          commentLocation = Just
            "decls_in_signature.h:3:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["decls_in_signature.h"],
              headerInclude =
              "decls_in_signature.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Outside",
          structConstr = HsName
            "@NsConstr"
            "Outside",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "outside_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "decls_in_signature.h:4:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "outside_x"},
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
                    "decls_in_signature.h:4:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["decls_in_signature.h"],
                      headerInclude =
                      "decls_in_signature.h"},
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "outside_y",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "decls_in_signature.h:5:7",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "outside_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "y",
                  commentLocation = Just
                    "decls_in_signature.h:5:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["decls_in_signature.h"],
                      headerInclude =
                      "decls_in_signature.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "decls_in_signature.h:3:8",
                declId = NamePair {
                  nameC = Name "outside",
                  nameHsIdent = HsIdentifier
                    "Outside"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["decls_in_signature.h"],
                    headerInclude =
                    "decls_in_signature.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Outside"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "decls_in_signature.h:4:7",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "outside_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "decls_in_signature.h:5:7",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = HsIdentifier
                            "outside_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 32,
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
              commentOrigin = Just "outside",
              commentLocation = Just
                "decls_in_signature.h:3:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["decls_in_signature.h"],
                  headerInclude =
                  "decls_in_signature.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 8,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Outside",
                  structConstr = HsName
                    "@NsConstr"
                    "Outside",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "outside_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "decls_in_signature.h:4:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "outside_x"},
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
                            "decls_in_signature.h:4:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["decls_in_signature.h"],
                              headerInclude =
                              "decls_in_signature.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "outside_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "decls_in_signature.h:5:7",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "outside_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "decls_in_signature.h:5:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["decls_in_signature.h"],
                              headerInclude =
                              "decls_in_signature.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "decls_in_signature.h:3:8",
                        declId = NamePair {
                          nameC = Name "outside",
                          nameHsIdent = HsIdentifier
                            "Outside"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["decls_in_signature.h"],
                            headerInclude =
                            "decls_in_signature.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Outside"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "decls_in_signature.h:4:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "outside_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "decls_in_signature.h:5:7",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "outside_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
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
                      commentOrigin = Just "outside",
                      commentLocation = Just
                        "decls_in_signature.h:3:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["decls_in_signature.h"],
                          headerInclude =
                          "decls_in_signature.h"},
                      commentChildren = []}})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 4]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Outside",
                  structConstr = HsName
                    "@NsConstr"
                    "Outside",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "outside_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "decls_in_signature.h:4:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "outside_x"},
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
                            "decls_in_signature.h:4:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["decls_in_signature.h"],
                              headerInclude =
                              "decls_in_signature.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "outside_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "decls_in_signature.h:5:7",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "outside_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "decls_in_signature.h:5:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["decls_in_signature.h"],
                              headerInclude =
                              "decls_in_signature.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "decls_in_signature.h:3:8",
                        declId = NamePair {
                          nameC = Name "outside",
                          nameHsIdent = HsIdentifier
                            "Outside"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["decls_in_signature.h"],
                            headerInclude =
                            "decls_in_signature.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Outside"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "decls_in_signature.h:4:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "outside_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "decls_in_signature.h:5:7",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "outside_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
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
                      commentOrigin = Just "outside",
                      commentLocation = Just
                        "decls_in_signature.h:3:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["decls_in_signature.h"],
                          headerInclude =
                          "decls_in_signature.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      4
                      (Idx 1)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Outside",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Outside",
      deriveInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "normal_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName
              "@NsVar"
              "ptr_to_opaque"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Opaque")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "ptr_to_opaque",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName
              "@NsVar"
              "ptr_to_defined"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Outside")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "ptr_to_defined",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "by_value"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Outside")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "by_value",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_decls_in_signature_16f5d4c94f55e369",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_decls_in_signature_16f5d4c94f55e369 (struct opaque *arg1, struct outside *arg2, struct outside *arg3) { normal(arg1, arg2, *arg3); }",
          capiWrapperImport =
          "decls_in_signature.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "ptr_to_opaque",
                  nameHsIdent = HsIdentifier
                    "ptr_to_opaque"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "opaque",
                    nameHsIdent = HsIdentifier
                      "Opaque"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "ptr_to_defined",
                  nameHsIdent = HsIdentifier
                    "ptr_to_defined"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "outside",
                    nameHsIdent = HsIdentifier
                      "Outside"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "by_value",
                  nameHsIdent = HsIdentifier
                    "by_value"})
              (TypeStruct
                NamePair {
                  nameC = Name "outside",
                  nameHsIdent = HsIdentifier
                    "Outside"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "normal",
          commentLocation = Just
            "decls_in_signature.h:7:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["decls_in_signature.h"],
              headerInclude =
              "decls_in_signature.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_decls_in_signature_87a8c2dd9b065b93",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Opaque")))
              (HsFun
                (HsPtr
                  (HsTypRef
                    (HsName
                      "@NsTypeConstr"
                      "Outside")))
                (HsFun
                  (HsTypRef
                    (HsName
                      "@NsTypeConstr"
                      "Outside"))
                  (HsIO
                    (HsPrimType HsPrimUnit))))))),
      foreignImportOrigName =
      "hs_bindgen_test_decls_in_signature_87a8c2dd9b065b93",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_normal_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_decls_in_signature_87a8c2dd9b065b93 (void)) (struct opaque *arg1, struct outside *arg2, struct outside arg3) { return &normal; } ",
          capiWrapperImport =
          "decls_in_signature.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name "opaque",
                  nameHsIdent = HsIdentifier
                    "Opaque"}
                NameOriginInSource),
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name "outside",
                  nameHsIdent = HsIdentifier
                    "Outside"}
                NameOriginInSource),
            TypeStruct
              NamePair {
                nameC = Name "outside",
                nameHsIdent = HsIdentifier
                  "Outside"}
              NameOriginInSource]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "normal",
          commentLocation = Just
            "decls_in_signature.h:7:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["decls_in_signature.h"],
              headerInclude =
              "decls_in_signature.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Named_struct",
      structConstr = HsName
        "@NsConstr"
        "Named_struct",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "named_struct_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "decls_in_signature.h:17:35",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier
                    "named_struct_x"},
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
                "decls_in_signature.h:17:35",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["decls_in_signature.h"],
                  headerInclude =
                  "decls_in_signature.h"},
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "named_struct_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "decls_in_signature.h:17:42",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier
                    "named_struct_y"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Just
                "decls_in_signature.h:17:42",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["decls_in_signature.h"],
                  headerInclude =
                  "decls_in_signature.h"},
              commentChildren = []}}],
      structOrigin =
      Just
        Decl {
          declInfo =
          DeclInfo {
            declLoc =
            "decls_in_signature.h:17:16",
            declId = NamePair {
              nameC = Name "named_struct",
              nameHsIdent = HsIdentifier
                "Named_struct"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["decls_in_signature.h"],
                headerInclude =
                "decls_in_signature.h"},
            declComment =
            Just
              (Comment
                [
                  Paragraph
                    [TextContent "Error cases"],
                  Paragraph
                    [
                      TextContent
                        "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                      TextContent
                        "and the edge cases below)."]])},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Named_struct"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "decls_in_signature.h:17:35",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "named_struct_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "decls_in_signature.h:17:42",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "named_struct_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
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
      structComment =
      Just
        Comment {
          commentTitle = Just
            [TextContent "Error cases"],
          commentOrigin = Just
            "named_struct",
          commentLocation = Just
            "decls_in_signature.h:17:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["decls_in_signature.h"],
              headerInclude =
              "decls_in_signature.h"},
          commentChildren =
          [
            Paragraph
              [
                TextContent
                  "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                TextContent
                  "and the edge cases below)."]]}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Named_struct",
          structConstr = HsName
            "@NsConstr"
            "Named_struct",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "named_struct_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "decls_in_signature.h:17:35",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "named_struct_x"},
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
                    "decls_in_signature.h:17:35",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["decls_in_signature.h"],
                      headerInclude =
                      "decls_in_signature.h"},
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "named_struct_y",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "decls_in_signature.h:17:42",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "named_struct_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "y",
                  commentLocation = Just
                    "decls_in_signature.h:17:42",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["decls_in_signature.h"],
                      headerInclude =
                      "decls_in_signature.h"},
                  commentChildren = []}}],
          structOrigin =
          Just
            Decl {
              declInfo =
              DeclInfo {
                declLoc =
                "decls_in_signature.h:17:16",
                declId = NamePair {
                  nameC = Name "named_struct",
                  nameHsIdent = HsIdentifier
                    "Named_struct"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["decls_in_signature.h"],
                    headerInclude =
                    "decls_in_signature.h"},
                declComment =
                Just
                  (Comment
                    [
                      Paragraph
                        [TextContent "Error cases"],
                      Paragraph
                        [
                          TextContent
                            "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                          TextContent
                            "and the edge cases below)."]])},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName
                      "@NsConstr"
                      "Named_struct"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "decls_in_signature.h:17:35",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "named_struct_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "decls_in_signature.h:17:42",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = HsIdentifier
                            "named_struct_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 32,
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
          structComment =
          Just
            Comment {
              commentTitle = Just
                [TextContent "Error cases"],
              commentOrigin = Just
                "named_struct",
              commentLocation = Just
                "decls_in_signature.h:17:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["decls_in_signature.h"],
                  headerInclude =
                  "decls_in_signature.h"},
              commentChildren =
              [
                Paragraph
                  [
                    TextContent
                      "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                    TextContent
                      "and the edge cases below)."]]}}
        StorableInstance {
          storableSizeOf = 8,
          storableAlignment = 4,
          storablePeek =
          Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Named_struct",
                  structConstr = HsName
                    "@NsConstr"
                    "Named_struct",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "named_struct_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "decls_in_signature.h:17:35",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "named_struct_x"},
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
                            "decls_in_signature.h:17:35",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["decls_in_signature.h"],
                              headerInclude =
                              "decls_in_signature.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "named_struct_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "decls_in_signature.h:17:42",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "named_struct_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "decls_in_signature.h:17:42",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["decls_in_signature.h"],
                              headerInclude =
                              "decls_in_signature.h"},
                          commentChildren = []}}],
                  structOrigin =
                  Just
                    Decl {
                      declInfo =
                      DeclInfo {
                        declLoc =
                        "decls_in_signature.h:17:16",
                        declId = NamePair {
                          nameC = Name "named_struct",
                          nameHsIdent = HsIdentifier
                            "Named_struct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["decls_in_signature.h"],
                            headerInclude =
                            "decls_in_signature.h"},
                        declComment =
                        Just
                          (Comment
                            [
                              Paragraph
                                [TextContent "Error cases"],
                              Paragraph
                                [
                                  TextContent
                                    "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                                  TextContent
                                    "and the edge cases below)."]])},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Named_struct"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "decls_in_signature.h:17:35",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "named_struct_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "decls_in_signature.h:17:42",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "named_struct_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
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
                  structComment =
                  Just
                    Comment {
                      commentTitle = Just
                        [TextContent "Error cases"],
                      commentOrigin = Just
                        "named_struct",
                      commentLocation = Just
                        "decls_in_signature.h:17:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["decls_in_signature.h"],
                          headerInclude =
                          "decls_in_signature.h"},
                      commentChildren =
                      [
                        Paragraph
                          [
                            TextContent
                              "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                            TextContent
                              "and the edge cases below)."]]}})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 4]),
          storablePoke =
          Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Named_struct",
                  structConstr = HsName
                    "@NsConstr"
                    "Named_struct",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "named_struct_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "decls_in_signature.h:17:35",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "named_struct_x"},
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
                            "decls_in_signature.h:17:35",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["decls_in_signature.h"],
                              headerInclude =
                              "decls_in_signature.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "named_struct_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "decls_in_signature.h:17:42",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "named_struct_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "decls_in_signature.h:17:42",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["decls_in_signature.h"],
                              headerInclude =
                              "decls_in_signature.h"},
                          commentChildren = []}}],
                  structOrigin =
                  Just
                    Decl {
                      declInfo =
                      DeclInfo {
                        declLoc =
                        "decls_in_signature.h:17:16",
                        declId = NamePair {
                          nameC = Name "named_struct",
                          nameHsIdent = HsIdentifier
                            "Named_struct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["decls_in_signature.h"],
                            headerInclude =
                            "decls_in_signature.h"},
                        declComment =
                        Just
                          (Comment
                            [
                              Paragraph
                                [TextContent "Error cases"],
                              Paragraph
                                [
                                  TextContent
                                    "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                                  TextContent
                                    "and the edge cases below)."]])},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Named_struct"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "decls_in_signature.h:17:35",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "named_struct_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "decls_in_signature.h:17:42",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "named_struct_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
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
                  structComment =
                  Just
                    Comment {
                      commentTitle = Just
                        [TextContent "Error cases"],
                      commentOrigin = Just
                        "named_struct",
                      commentLocation = Just
                        "decls_in_signature.h:17:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["decls_in_signature.h"],
                          headerInclude =
                          "decls_in_signature.h"},
                      commentChildren =
                      [
                        Paragraph
                          [
                            TextContent
                              "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                            TextContent
                              "and the edge cases below)."]]}}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      4
                      (Idx 1)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Named_struct",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Named_struct",
      deriveInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "f1_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Named_struct")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_decls_in_signature_8b60d38de80093fa",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_decls_in_signature_8b60d38de80093fa (struct named_struct *arg1) { f1(*arg1); }",
          capiWrapperImport =
          "decls_in_signature.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg",
                  nameHsIdent = HsIdentifier
                    "arg"})
              (TypeStruct
                NamePair {
                  nameC = Name "named_struct",
                  nameHsIdent = HsIdentifier
                    "Named_struct"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment =
      Just
        Comment {
          commentTitle = Just
            [TextContent "Error cases"],
          commentOrigin = Just "f1",
          commentLocation = Just
            "decls_in_signature.h:17:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["decls_in_signature.h"],
              headerInclude =
              "decls_in_signature.h"},
          commentChildren =
          [
            Paragraph
              [
                TextContent
                  "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                TextContent
                  "and the edge cases below)."]]},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_decls_in_signature_a1b79fe9af8e18b8",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Named_struct"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_decls_in_signature_a1b79fe9af8e18b8",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_f1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_decls_in_signature_a1b79fe9af8e18b8 (void)) (struct named_struct arg1) { return &f1; } ",
          capiWrapperImport =
          "decls_in_signature.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeStruct
              NamePair {
                nameC = Name "named_struct",
                nameHsIdent = HsIdentifier
                  "Named_struct"}
              NameOriginInSource]
          TypeVoid),
      foreignImportComment =
      Just
        Comment {
          commentTitle = Just
            [TextContent "Error cases"],
          commentOrigin = Just "f1",
          commentLocation = Just
            "decls_in_signature.h:17:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["decls_in_signature.h"],
              headerInclude =
              "decls_in_signature.h"},
          commentChildren =
          [
            Paragraph
              [
                TextContent
                  "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                TextContent
                  "and the edge cases below)."]]},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Named_union",
      newtypeConstr = HsName
        "@NsConstr"
        "Named_union",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Named_union",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "decls_in_signature.h:20:15",
          declId = NamePair {
            nameC = Name "named_union",
            nameHsIdent = HsIdentifier
              "Named_union"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["decls_in_signature.h"],
              headerInclude =
              "decls_in_signature.h"},
          declComment = Nothing},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Named_union",
              newtypeField = HsName
                "@NsVar"
                "un_Named_union"},
            unionSizeof = 4,
            unionAlignment = 4,
            unionFields = [
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc =
                  "decls_in_signature.h:20:33",
                  fieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "named_union_x"},
                  fieldComment = Nothing},
                unionFieldType = TypePrim
                  (PrimIntegral PrimInt Signed)},
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc =
                  "decls_in_signature.h:20:41",
                  fieldName = NamePair {
                    nameC = Name "y",
                    nameHsIdent = HsIdentifier
                      "named_union_y"},
                  fieldComment = Nothing},
                unionFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))}]},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "named_union",
          commentLocation = Just
            "decls_in_signature.h:20:15",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["decls_in_signature.h"],
              headerInclude =
              "decls_in_signature.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveVia
        (HsSizedByteArray 4 4),
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Named_union",
      deriveInstanceComment =
      Nothing},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = HsName
        "@NsVar"
        "get_named_union_x",
      unionGetterType = HsPrimType
        HsPrimCInt,
      unionGetterConstr = HsName
        "@NsTypeConstr"
        "Named_union",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "x",
          commentLocation = Just
            "decls_in_signature.h:20:33",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["decls_in_signature.h"],
              headerInclude =
              "decls_in_signature.h"},
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_named_union_x"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = HsName
        "@NsVar"
        "set_named_union_x",
      unionSetterType = HsPrimType
        HsPrimCInt,
      unionSetterConstr = HsName
        "@NsTypeConstr"
        "Named_union",
      unionSetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "get_named_union_x"]]}},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = HsName
        "@NsVar"
        "get_named_union_y",
      unionGetterType = HsPrimType
        HsPrimCChar,
      unionGetterConstr = HsName
        "@NsTypeConstr"
        "Named_union",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "y",
          commentLocation = Just
            "decls_in_signature.h:20:41",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["decls_in_signature.h"],
              headerInclude =
              "decls_in_signature.h"},
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_named_union_y"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = HsName
        "@NsVar"
        "set_named_union_y",
      unionSetterType = HsPrimType
        HsPrimCChar,
      unionSetterConstr = HsName
        "@NsTypeConstr"
        "Named_union",
      unionSetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "get_named_union_y"]]}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "f2_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Named_union")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_decls_in_signature_4a86b0420a250963",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_decls_in_signature_4a86b0420a250963 (union named_union *arg1) { f2(*arg1); }",
          capiWrapperImport =
          "decls_in_signature.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg",
                  nameHsIdent = HsIdentifier
                    "arg"})
              (TypeUnion
                NamePair {
                  nameC = Name "named_union",
                  nameHsIdent = HsIdentifier
                    "Named_union"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "f2",
          commentLocation = Just
            "decls_in_signature.h:20:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["decls_in_signature.h"],
              headerInclude =
              "decls_in_signature.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_decls_in_signature_74cfd16f2b7e27ba",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Named_union"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_decls_in_signature_74cfd16f2b7e27ba",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_f2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_decls_in_signature_74cfd16f2b7e27ba (void)) (union named_union arg1) { return &f2; } ",
          capiWrapperImport =
          "decls_in_signature.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeUnion
              NamePair {
                nameC = Name "named_union",
                nameHsIdent = HsIdentifier
                  "Named_union"}
              NameOriginInSource]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "f2",
          commentLocation = Just
            "decls_in_signature.h:20:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["decls_in_signature.h"],
              headerInclude =
              "decls_in_signature.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
