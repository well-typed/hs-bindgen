[
  DeclInlineCInclude
    "skip_over_long_double.h",
  DeclInlineC
    "void hs_bindgen_test_skip_over_long_double_c7f5e756cd95b3ed (signed int arg1) { fun2(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fun2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_skip_over_long_double_c7f5e756cd95b3ed",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        (Comment
          Nothing
          (Just
            "skip_over_long_double.h:7:6")
          (Just "skip_over_long_double.h")
          []),
      foreignImportSafety = Safe},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Struct2",
      structConstr = HsName
        "@NsConstr"
        "Struct2",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "struct2_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "skip_over_long_double.h:14:7",
              structFieldName = NamePair {
                nameC = Name "x",
                nameHsIdent = HsIdentifier
                  "struct2_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just
                "skip_over_long_double.h:14:7")
              (Just "skip_over_long_double.h")
              [])}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "skip_over_long_double.h:13:8",
            declId = NamePair {
              nameC = Name "struct2",
              nameHsIdent = HsIdentifier
                "Struct2"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "skip_over_long_double.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Struct2"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "skip_over_long_double.h:14:7",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "struct2_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing}],
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
        (Comment
          Nothing
          (Just
            "skip_over_long_double.h:13:8")
          (Just "skip_over_long_double.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Struct2",
          structConstr = HsName
            "@NsConstr"
            "Struct2",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "struct2_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc =
                  "skip_over_long_double.h:14:7",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "struct2_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just
                    "skip_over_long_double.h:14:7")
                  (Just "skip_over_long_double.h")
                  [])}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "skip_over_long_double.h:13:8",
                declId = NamePair {
                  nameC = Name "struct2",
                  nameHsIdent = HsIdentifier
                    "Struct2"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader =
                "skip_over_long_double.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Struct2"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldLoc =
                      "skip_over_long_double.h:14:7",
                      structFieldName = NamePair {
                        nameC = Name "x",
                        nameHsIdent = HsIdentifier
                          "struct2_x"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing}],
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
            (Comment
              Nothing
              (Just
                "skip_over_long_double.h:13:8")
              (Just "skip_over_long_double.h")
              [])}
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
                    "Struct2",
                  structConstr = HsName
                    "@NsConstr"
                    "Struct2",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "struct2_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "skip_over_long_double.h:14:7",
                          structFieldName = NamePair {
                            nameC = Name "x",
                            nameHsIdent = HsIdentifier
                              "struct2_x"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just
                            "skip_over_long_double.h:14:7")
                          (Just "skip_over_long_double.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "skip_over_long_double.h:13:8",
                        declId = NamePair {
                          nameC = Name "struct2",
                          nameHsIdent = HsIdentifier
                            "Struct2"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "skip_over_long_double.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Struct2"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "skip_over_long_double.h:14:7",
                              structFieldName = NamePair {
                                nameC = Name "x",
                                nameHsIdent = HsIdentifier
                                  "struct2_x"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}],
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
                    (Comment
                      Nothing
                      (Just
                        "skip_over_long_double.h:13:8")
                      (Just "skip_over_long_double.h")
                      [])})
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
                    "Struct2",
                  structConstr = HsName
                    "@NsConstr"
                    "Struct2",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "struct2_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "skip_over_long_double.h:14:7",
                          structFieldName = NamePair {
                            nameC = Name "x",
                            nameHsIdent = HsIdentifier
                              "struct2_x"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just
                            "skip_over_long_double.h:14:7")
                          (Just "skip_over_long_double.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "skip_over_long_double.h:13:8",
                        declId = NamePair {
                          nameC = Name "struct2",
                          nameHsIdent = HsIdentifier
                            "Struct2"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "skip_over_long_double.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Struct2"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "skip_over_long_double.h:14:7",
                              structFieldName = NamePair {
                                nameC = Name "x",
                                nameHsIdent = HsIdentifier
                                  "struct2_x"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}],
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
                    (Comment
                      Nothing
                      (Just
                        "skip_over_long_double.h:13:8")
                      (Just "skip_over_long_double.h")
                      [])}
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
        "Struct2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Struct2",
      deriveInstanceComment =
      Nothing}]
