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
                nameC = CName "x",
                nameHsIdent = HsIdentifier
                  "thing_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "struct_arg.h:2:8",
            declId = NamePair {
              nameC = CName "thing",
              nameHsIdent = HsIdentifier
                "Thing"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "struct_arg.h"},
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
                    nameC = CName "x",
                    nameHsIdent = HsIdentifier
                      "thing_x"},
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
        [Eq, Show, Storable]},
  DeclInstance
    (InstanceStorable
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
                  nameC = CName "x",
                  nameHsIdent = HsIdentifier
                    "thing_x"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "struct_arg.h:2:8",
              declId = NamePair {
                nameC = CName "thing",
                nameHsIdent = HsIdentifier
                  "Thing"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "struct_arg.h"},
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
                      nameC = CName "x",
                      nameHsIdent = HsIdentifier
                        "thing_x"},
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
          [Eq, Show, Storable]}
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
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "thing_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "struct_arg.h:2:8",
                      declId = NamePair {
                        nameC = CName "thing",
                        nameHsIdent = HsIdentifier
                          "Thing"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "struct_arg.h"},
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
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "thing_x"},
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
                  [Eq, Show, Storable]})
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
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "thing_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "struct_arg.h:2:8",
                      declId = NamePair {
                        nameC = CName "thing",
                        nameHsIdent = HsIdentifier
                          "Thing"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "struct_arg.h"},
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
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "thing_x"},
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
                  [Eq, Show, Storable]}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Thing"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Thing"),
  DeclInlineCInclude
    "struct_arg.h",
  DeclInlineC
    "signed int testmodule_thing_fun_1 (struct thing *arg1) { return thing_fun_1(*arg1); }",
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
      "testmodule_thing_fun_1",
      foreignImportHeader =
      "struct_arg.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypeStruct
              NamePair {
                nameC = CName "thing",
                nameHsIdent = HsIdentifier
                  "Thing"}
              NameOriginInSource],
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)}},
  DeclSimple,
  DeclInlineCInclude
    "struct_arg.h",
  DeclInlineC
    "void testmodule_thing_fun_2 (signed int arg1, struct thing *arg2) { *arg2 = thing_fun_2(arg1); }",
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
      "testmodule_thing_fun_2",
      foreignImportHeader =
      "struct_arg.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed)],
          functionRes = TypeStruct
            NamePair {
              nameC = CName "thing",
              nameHsIdent = HsIdentifier
                "Thing"}
            NameOriginInSource}},
  DeclSimple,
  DeclInlineCInclude
    "struct_arg.h",
  DeclInlineC
    "void testmodule_thing_fun_3a (signed int arg1, struct thing *arg2, double arg3, struct thing *arg4) { *arg4 = thing_fun_3a(arg1, *arg2, arg3); }",
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
      "testmodule_thing_fun_3a",
      foreignImportHeader =
      "struct_arg.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypeStruct
              NamePair {
                nameC = CName "thing",
                nameHsIdent = HsIdentifier
                  "Thing"}
              NameOriginInSource,
            TypePrim
              (PrimFloating PrimDouble)],
          functionRes = TypeStruct
            NamePair {
              nameC = CName "thing",
              nameHsIdent = HsIdentifier
                "Thing"}
            NameOriginInSource}},
  DeclSimple,
  DeclInlineCInclude
    "struct_arg.h",
  DeclInlineC
    "char testmodule_thing_fun_3b (signed int arg1, struct thing *arg2, double arg3) { return thing_fun_3b(arg1, *arg2, arg3); }",
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
      "testmodule_thing_fun_3b",
      foreignImportHeader =
      "struct_arg.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypeStruct
              NamePair {
                nameC = CName "thing",
                nameHsIdent = HsIdentifier
                  "Thing"}
              NameOriginInSource,
            TypePrim
              (PrimFloating PrimDouble)],
          functionRes = TypePrim
            (PrimChar
              (PrimSignImplicit
                (Just Signed)))}},
  DeclSimple]
