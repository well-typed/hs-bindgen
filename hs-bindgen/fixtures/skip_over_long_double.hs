[
  DeclInlineCInclude
    "skip_over_long_double.h",
  DeclInlineC
    "void testmodule_fun2 (signed int arg1) { fun2(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fun2",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "testmodule_fun2",
      foreignImportHeader =
      "skip_over_long_double.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed)],
          functionRes = TypeVoid}},
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
                nameC = CName "x",
                nameHsIdent = HsIdentifier
                  "struct2_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "skip_over_long_double.h:13:8",
            declId = NamePair {
              nameC = CName "struct2",
              nameHsIdent = HsIdentifier
                "Struct2"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "skip_over_long_double.h"},
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
                    nameC = CName "x",
                    nameHsIdent = HsIdentifier
                      "struct2_x"},
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
                  nameC = CName "x",
                  nameHsIdent = HsIdentifier
                    "struct2_x"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "skip_over_long_double.h:13:8",
              declId = NamePair {
                nameC = CName "struct2",
                nameHsIdent = HsIdentifier
                  "Struct2"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader =
              "skip_over_long_double.h"},
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
                      nameC = CName "x",
                      nameHsIdent = HsIdentifier
                        "struct2_x"},
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
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "struct2_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "skip_over_long_double.h:13:8",
                      declId = NamePair {
                        nameC = CName "struct2",
                        nameHsIdent = HsIdentifier
                          "Struct2"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "skip_over_long_double.h"},
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
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "struct2_x"},
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
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "struct2_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "skip_over_long_double.h:13:8",
                      declId = NamePair {
                        nameC = CName "struct2",
                        nameHsIdent = HsIdentifier
                          "Struct2"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "skip_over_long_double.h"},
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
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "struct2_x"},
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
      "Struct2"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct2")]
