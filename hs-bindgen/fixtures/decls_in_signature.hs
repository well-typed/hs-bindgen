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
            nameC = CName "opaque",
            nameHsIdent = HsIdentifier
              "Opaque"},
          declOrigin = NameOriginInSource,
          declAliases = []},
        declKind = OpaqueStruct,
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}}},
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
              structFieldLoc =
              "decls_in_signature.h:4:7",
              structFieldName = NamePair {
                nameC = CName "x",
                nameHsIdent = HsIdentifier
                  "outside_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "outside_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "decls_in_signature.h:5:7",
              structFieldName = NamePair {
                nameC = CName "y",
                nameHsIdent = HsIdentifier
                  "outside_y"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "decls_in_signature.h:3:8",
            declId = NamePair {
              nameC = CName "outside",
              nameHsIdent = HsIdentifier
                "Outside"},
            declOrigin = NameOriginInSource,
            declAliases = []},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Outside"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "decls_in_signature.h:4:7",
                  structFieldName = NamePair {
                    nameC = CName "x",
                    nameHsIdent = HsIdentifier
                      "outside_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "decls_in_signature.h:5:7",
                  structFieldName = NamePair {
                    nameC = CName "y",
                    nameHsIdent = HsIdentifier
                      "outside_y"},
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
        [Eq, Show, Storable]},
  DeclInstance
    (InstanceStorable
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
                structFieldLoc =
                "decls_in_signature.h:4:7",
                structFieldName = NamePair {
                  nameC = CName "x",
                  nameHsIdent = HsIdentifier
                    "outside_x"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "outside_y",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "decls_in_signature.h:5:7",
                structFieldName = NamePair {
                  nameC = CName "y",
                  nameHsIdent = HsIdentifier
                    "outside_y"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 32,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "decls_in_signature.h:3:8",
              declId = NamePair {
                nameC = CName "outside",
                nameHsIdent = HsIdentifier
                  "Outside"},
              declOrigin = NameOriginInSource,
              declAliases = []},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Outside"),
                structSizeof = 8,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "decls_in_signature.h:4:7",
                    structFieldName = NamePair {
                      nameC = CName "x",
                      nameHsIdent = HsIdentifier
                        "outside_x"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "decls_in_signature.h:5:7",
                    structFieldName = NamePair {
                      nameC = CName "y",
                      nameHsIdent = HsIdentifier
                        "outside_y"},
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
          [Eq, Show, Storable]}
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
                        structFieldLoc =
                        "decls_in_signature.h:4:7",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "outside_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "outside_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "decls_in_signature.h:5:7",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "outside_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "decls_in_signature.h:3:8",
                      declId = NamePair {
                        nameC = CName "outside",
                        nameHsIdent = HsIdentifier
                          "Outside"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Outside"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "decls_in_signature.h:4:7",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "outside_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "decls_in_signature.h:5:7",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "outside_y"},
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
                  [Eq, Show, Storable]})
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
                        structFieldLoc =
                        "decls_in_signature.h:4:7",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "outside_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "outside_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "decls_in_signature.h:5:7",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "outside_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "decls_in_signature.h:3:8",
                      declId = NamePair {
                        nameC = CName "outside",
                        nameHsIdent = HsIdentifier
                          "Outside"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Outside"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "decls_in_signature.h:4:7",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "outside_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "decls_in_signature.h:5:7",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "outside_y"},
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
                  [Eq, Show, Storable]}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    4
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Outside"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Outside"),
  DeclInlineCInclude
    "decls_in_signature.h",
  DeclInlineC
    "void testmodule_normal (struct opaque *arg1, struct outside *arg2, struct outside *arg3) { normal(arg1, arg2, *arg3); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "normal_wrapper",
      foreignImportType = HsFun
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
            (HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Outside")))
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "testmodule_normal",
      foreignImportHeader =
      "decls_in_signature.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = CName "opaque",
                  nameHsIdent = HsIdentifier
                    "Opaque"}
                NameOriginInSource),
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = CName "outside",
                  nameHsIdent = HsIdentifier
                    "Outside"}
                NameOriginInSource),
            TypeStruct
              NamePair {
                nameC = CName "outside",
                nameHsIdent = HsIdentifier
                  "Outside"}
              NameOriginInSource],
          functionRes = TypeVoid,
          functionHeader =
          "decls_in_signature.h"}},
  DeclSimple]
