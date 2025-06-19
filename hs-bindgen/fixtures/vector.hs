[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Vector",
      structConstr = HsName
        "@NsConstr"
        "Vector",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "vector_x",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "vector.h:2:12",
              structFieldName = NamePair {
                nameC = CName "x",
                nameHsIdent = HsIdentifier
                  "vector_x"},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "vector_y",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "vector.h:3:12",
              structFieldName = NamePair {
                nameC = CName "y",
                nameHsIdent = HsIdentifier
                  "vector_y"},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "vector.h:1:9",
            declId = NamePair {
              nameC = CName "vector",
              nameHsIdent = HsIdentifier
                "Vector"},
            declOrigin = NameOriginGenerated
              (AnonId "vector.h:1:9"),
            declAliases = [CName "vector"]},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Vector"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "vector.h:2:12",
                  structFieldName = NamePair {
                    nameC = CName "x",
                    nameHsIdent = HsIdentifier
                      "vector_x"},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "vector.h:3:12",
                  structFieldName = NamePair {
                    nameC = CName "y",
                    nameHsIdent = HsIdentifier
                      "vector_y"},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 64,
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
          "Vector",
        structConstr = HsName
          "@NsConstr"
          "Vector",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "vector_x",
            fieldType = HsPrimType
              HsPrimCDouble,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "vector.h:2:12",
                structFieldName = NamePair {
                  nameC = CName "x",
                  nameHsIdent = HsIdentifier
                    "vector_x"},
                structFieldType = TypePrim
                  (PrimFloating PrimDouble),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "vector_y",
            fieldType = HsPrimType
              HsPrimCDouble,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "vector.h:3:12",
                structFieldName = NamePair {
                  nameC = CName "y",
                  nameHsIdent = HsIdentifier
                    "vector_y"},
                structFieldType = TypePrim
                  (PrimFloating PrimDouble),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "vector.h:1:9",
              declId = NamePair {
                nameC = CName "vector",
                nameHsIdent = HsIdentifier
                  "Vector"},
              declOrigin = NameOriginGenerated
                (AnonId "vector.h:1:9"),
              declAliases = [CName "vector"]},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Vector"),
                structSizeof = 16,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "vector.h:2:12",
                    structFieldName = NamePair {
                      nameC = CName "x",
                      nameHsIdent = HsIdentifier
                        "vector_x"},
                    structFieldType = TypePrim
                      (PrimFloating PrimDouble),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "vector.h:3:12",
                    structFieldName = NamePair {
                      nameC = CName "y",
                      nameHsIdent = HsIdentifier
                        "vector_y"},
                    structFieldType = TypePrim
                      (PrimFloating PrimDouble),
                    structFieldOffset = 64,
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
        storableSizeOf = 16,
        storableAlignment = 8,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Vector",
                structConstr = HsName
                  "@NsConstr"
                  "Vector",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "vector_x",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "vector.h:2:12",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "vector_x"},
                        structFieldType = TypePrim
                          (PrimFloating PrimDouble),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "vector_y",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "vector.h:3:12",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "vector_y"},
                        structFieldType = TypePrim
                          (PrimFloating PrimDouble),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "vector.h:1:9",
                      declId = NamePair {
                        nameC = CName "vector",
                        nameHsIdent = HsIdentifier
                          "Vector"},
                      declOrigin = NameOriginGenerated
                        (AnonId "vector.h:1:9"),
                      declAliases = [CName "vector"]},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Vector"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "vector.h:2:12",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "vector_x"},
                            structFieldType = TypePrim
                              (PrimFloating PrimDouble),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "vector.h:3:12",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "vector_y"},
                            structFieldType = TypePrim
                              (PrimFloating PrimDouble),
                            structFieldOffset = 64,
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
              PeekByteOff (Idx 0) 8]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Vector",
                structConstr = HsName
                  "@NsConstr"
                  "Vector",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "vector_x",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "vector.h:2:12",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "vector_x"},
                        structFieldType = TypePrim
                          (PrimFloating PrimDouble),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "vector_y",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "vector.h:3:12",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "vector_y"},
                        structFieldType = TypePrim
                          (PrimFloating PrimDouble),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "vector.h:1:9",
                      declId = NamePair {
                        nameC = CName "vector",
                        nameHsIdent = HsIdentifier
                          "Vector"},
                      declOrigin = NameOriginGenerated
                        (AnonId "vector.h:1:9"),
                      declAliases = [CName "vector"]},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Vector"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "vector.h:2:12",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "vector_x"},
                            structFieldType = TypePrim
                              (PrimFloating PrimDouble),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "vector.h:3:12",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "vector_y"},
                            structFieldType = TypePrim
                              (PrimFloating PrimDouble),
                            structFieldOffset = 64,
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
                    8
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Vector"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Vector"),
  DeclInlineCInclude "vector.h",
  DeclInlineC
    "vector *testmodule_new_vector (double arg1, double arg2) { return new_vector(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "new_vector",
      foreignImportType = HsFun
        (HsPrimType HsPrimCDouble)
        (HsFun
          (HsPrimType HsPrimCDouble)
          (HsIO
            (HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Vector"))))),
      foreignImportOrigName =
      "testmodule_new_vector",
      foreignImportHeader =
      "vector.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePrim
              (PrimFloating PrimDouble),
            TypePrim
              (PrimFloating PrimDouble)],
          functionRes = TypePointer
            (TypeTypedef
              (TypedefSquashed
                (CName "vector")
                (TypeStruct
                  NamePair {
                    nameC = CName "vector",
                    nameHsIdent = HsIdentifier
                      "Vector"}
                  (NameOriginGenerated
                    (AnonId "vector.h:1:9"))))),
          functionHeader = "vector.h"}}]
