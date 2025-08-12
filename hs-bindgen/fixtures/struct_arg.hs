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
              structFieldLoc =
              "struct_arg.h:3:9",
              structFieldName = NamePair {
                nameC = Name "x",
                nameHsIdent = HsIdentifier
                  "thing_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
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
                  structFieldLoc =
                  "struct_arg.h:3:9",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "thing_x"},
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
      structComment = Nothing},
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
                  structFieldLoc =
                  "struct_arg.h:3:9",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "thing_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
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
                      structFieldLoc =
                      "struct_arg.h:3:9",
                      structFieldName = NamePair {
                        nameC = Name "x",
                        nameHsIdent = HsIdentifier
                          "thing_x"},
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
          structComment = Nothing}
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
                          structFieldLoc =
                          "struct_arg.h:3:9",
                          structFieldName = NamePair {
                            nameC = Name "x",
                            nameHsIdent = HsIdentifier
                              "thing_x"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
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
                              structFieldLoc =
                              "struct_arg.h:3:9",
                              structFieldName = NamePair {
                                nameC = Name "x",
                                nameHsIdent = HsIdentifier
                                  "thing_x"},
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
                  structComment = Nothing})
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
                          structFieldLoc =
                          "struct_arg.h:3:9",
                          structFieldName = NamePair {
                            nameC = Name "x",
                            nameHsIdent = HsIdentifier
                              "thing_x"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
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
                              structFieldLoc =
                              "struct_arg.h:3:9",
                              structFieldName = NamePair {
                                nameC = Name "x",
                                nameHsIdent = HsIdentifier
                                  "thing_x"},
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
                  structComment = Nothing}
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
  DeclInlineCInclude
    "struct_arg.h",
  DeclInlineC
    "signed int test_internal_thing_fun_1 (struct thing *arg1) { return thing_fun_1(*arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "thing_fun_1_wrapper",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Thing")))
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "test_internal_thing_fun_1",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypeStruct
              NamePair {
                nameC = Name "thing",
                nameHsIdent = HsIdentifier
                  "Thing"}
              NameOriginInSource],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Nothing},
  DeclSimple,
  DeclInlineCInclude
    "struct_arg.h",
  DeclInlineC
    "void test_internal_thing_fun_2 (signed int arg1, struct thing *arg2) { *arg2 = thing_fun_2(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "thing_fun_2_wrapper",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsFun
          (HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Thing")))
          (HsIO (HsPrimType HsPrimUnit))),
      foreignImportOrigName =
      "test_internal_thing_fun_2",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeStruct
            NamePair {
              nameC = Name "thing",
              nameHsIdent = HsIdentifier
                "Thing"}
            NameOriginInSource},
      foreignImportComment = Nothing},
  DeclSimple,
  DeclInlineCInclude
    "struct_arg.h",
  DeclInlineC
    "void test_internal_thing_fun_3a (signed int arg1, struct thing *arg2, double arg3, struct thing *arg4) { *arg4 = thing_fun_3a(arg1, *arg2, arg3); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "thing_fun_3a_wrapper",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsFun
          (HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Thing")))
          (HsFun
            (HsPrimType HsPrimCDouble)
            (HsFun
              (HsPtr
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Thing")))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "test_internal_thing_fun_3a",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypeStruct
              NamePair {
                nameC = Name "thing",
                nameHsIdent = HsIdentifier
                  "Thing"}
              NameOriginInSource,
            TypePrim
              (PrimFloating PrimDouble)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeStruct
            NamePair {
              nameC = Name "thing",
              nameHsIdent = HsIdentifier
                "Thing"}
            NameOriginInSource},
      foreignImportComment = Nothing},
  DeclSimple,
  DeclInlineCInclude
    "struct_arg.h",
  DeclInlineC
    "char test_internal_thing_fun_3b (signed int arg1, struct thing *arg2, double arg3) { return thing_fun_3b(arg1, *arg2, arg3); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "thing_fun_3b_wrapper",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsFun
          (HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Thing")))
          (HsFun
            (HsPrimType HsPrimCDouble)
            (HsIO
              (HsPrimType HsPrimCChar)))),
      foreignImportOrigName =
      "test_internal_thing_fun_3b",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypeStruct
              NamePair {
                nameC = Name "thing",
                nameHsIdent = HsIdentifier
                  "Thing"}
              NameOriginInSource,
            TypePrim
              (PrimFloating PrimDouble)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimChar
              (PrimSignImplicit
                (Just Signed)))},
      foreignImportComment = Nothing},
  DeclSimple]
