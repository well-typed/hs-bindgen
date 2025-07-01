[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Triple",
      newtypeConstr = HsName
        "@NsConstr"
        "Triple",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Triple",
        fieldType = HsConstArray
          3
          (HsPrimType HsPrimCInt),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "fixedarray.h:1:13",
          declId = NamePair {
            nameC = CName "triple",
            nameHsIdent = HsIdentifier
              "Triple"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "fixedarray.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Triple",
              newtypeField = HsName
                "@NsVar"
                "un_Triple"},
            typedefType = TypeConstArray
              3
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Triple"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Triple"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Triple"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Example",
      structConstr = HsName
        "@NsConstr"
        "Example",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "example_triple",
          fieldType = HsConstArray
            3
            (HsPrimType HsPrimCInt),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "fixedarray.h:4:9",
              structFieldName = NamePair {
                nameC = CName "triple",
                nameHsIdent = HsIdentifier
                  "example_triple"},
              structFieldType = TypeConstArray
                3
                (TypePrim
                  (PrimIntegral PrimInt Signed)),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "example_sudoku",
          fieldType = HsConstArray
            3
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "fixedarray.h:5:9",
              structFieldName = NamePair {
                nameC = CName "sudoku",
                nameHsIdent = HsIdentifier
                  "example_sudoku"},
              structFieldType = TypeConstArray
                3
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral PrimInt Signed))),
              structFieldOffset = 96,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "fixedarray.h:3:8",
            declId = NamePair {
              nameC = CName "Example",
              nameHsIdent = HsIdentifier
                "Example"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "fixedarray.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Example"),
              structSizeof = 48,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "fixedarray.h:4:9",
                  structFieldName = NamePair {
                    nameC = CName "triple",
                    nameHsIdent = HsIdentifier
                      "example_triple"},
                  structFieldType = TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral PrimInt Signed)),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "fixedarray.h:5:9",
                  structFieldName = NamePair {
                    nameC = CName "sudoku",
                    nameHsIdent = HsIdentifier
                      "example_sudoku"},
                  structFieldType = TypeConstArray
                    3
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral PrimInt Signed))),
                  structFieldOffset = 96,
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
          "Example",
        structConstr = HsName
          "@NsConstr"
          "Example",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "example_triple",
            fieldType = HsConstArray
              3
              (HsPrimType HsPrimCInt),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "fixedarray.h:4:9",
                structFieldName = NamePair {
                  nameC = CName "triple",
                  nameHsIdent = HsIdentifier
                    "example_triple"},
                structFieldType = TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral PrimInt Signed)),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "example_sudoku",
            fieldType = HsConstArray
              3
              (HsConstArray
                3
                (HsPrimType HsPrimCInt)),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "fixedarray.h:5:9",
                structFieldName = NamePair {
                  nameC = CName "sudoku",
                  nameHsIdent = HsIdentifier
                    "example_sudoku"},
                structFieldType = TypeConstArray
                  3
                  (TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral PrimInt Signed))),
                structFieldOffset = 96,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "fixedarray.h:3:8",
              declId = NamePair {
                nameC = CName "Example",
                nameHsIdent = HsIdentifier
                  "Example"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "fixedarray.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Example"),
                structSizeof = 48,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "fixedarray.h:4:9",
                    structFieldName = NamePair {
                      nameC = CName "triple",
                      nameHsIdent = HsIdentifier
                        "example_triple"},
                    structFieldType = TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral PrimInt Signed)),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "fixedarray.h:5:9",
                    structFieldName = NamePair {
                      nameC = CName "sudoku",
                      nameHsIdent = HsIdentifier
                        "example_sudoku"},
                    structFieldType = TypeConstArray
                      3
                      (TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral PrimInt Signed))),
                    structFieldOffset = 96,
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
        storableSizeOf = 48,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Example",
                structConstr = HsName
                  "@NsConstr"
                  "Example",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "example_triple",
                    fieldType = HsConstArray
                      3
                      (HsPrimType HsPrimCInt),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "fixedarray.h:4:9",
                        structFieldName = NamePair {
                          nameC = CName "triple",
                          nameHsIdent = HsIdentifier
                            "example_triple"},
                        structFieldType = TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral PrimInt Signed)),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "example_sudoku",
                    fieldType = HsConstArray
                      3
                      (HsConstArray
                        3
                        (HsPrimType HsPrimCInt)),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "fixedarray.h:5:9",
                        structFieldName = NamePair {
                          nameC = CName "sudoku",
                          nameHsIdent = HsIdentifier
                            "example_sudoku"},
                        structFieldType = TypeConstArray
                          3
                          (TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral PrimInt Signed))),
                        structFieldOffset = 96,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "fixedarray.h:3:8",
                      declId = NamePair {
                        nameC = CName "Example",
                        nameHsIdent = HsIdentifier
                          "Example"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "fixedarray.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Example"),
                        structSizeof = 48,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "fixedarray.h:4:9",
                            structFieldName = NamePair {
                              nameC = CName "triple",
                              nameHsIdent = HsIdentifier
                                "example_triple"},
                            structFieldType = TypeConstArray
                              3
                              (TypePrim
                                (PrimIntegral PrimInt Signed)),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "fixedarray.h:5:9",
                            structFieldName = NamePair {
                              nameC = CName "sudoku",
                              nameHsIdent = HsIdentifier
                                "example_sudoku"},
                            structFieldType = TypeConstArray
                              3
                              (TypeConstArray
                                3
                                (TypePrim
                                  (PrimIntegral PrimInt Signed))),
                            structFieldOffset = 96,
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
              PeekByteOff (Idx 0) 12]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Example",
                structConstr = HsName
                  "@NsConstr"
                  "Example",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "example_triple",
                    fieldType = HsConstArray
                      3
                      (HsPrimType HsPrimCInt),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "fixedarray.h:4:9",
                        structFieldName = NamePair {
                          nameC = CName "triple",
                          nameHsIdent = HsIdentifier
                            "example_triple"},
                        structFieldType = TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral PrimInt Signed)),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "example_sudoku",
                    fieldType = HsConstArray
                      3
                      (HsConstArray
                        3
                        (HsPrimType HsPrimCInt)),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "fixedarray.h:5:9",
                        structFieldName = NamePair {
                          nameC = CName "sudoku",
                          nameHsIdent = HsIdentifier
                            "example_sudoku"},
                        structFieldType = TypeConstArray
                          3
                          (TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral PrimInt Signed))),
                        structFieldOffset = 96,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "fixedarray.h:3:8",
                      declId = NamePair {
                        nameC = CName "Example",
                        nameHsIdent = HsIdentifier
                          "Example"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "fixedarray.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Example"),
                        structSizeof = 48,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "fixedarray.h:4:9",
                            structFieldName = NamePair {
                              nameC = CName "triple",
                              nameHsIdent = HsIdentifier
                                "example_triple"},
                            structFieldType = TypeConstArray
                              3
                              (TypePrim
                                (PrimIntegral PrimInt Signed)),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "fixedarray.h:5:9",
                            structFieldName = NamePair {
                              nameC = CName "sudoku",
                              nameHsIdent = HsIdentifier
                                "example_sudoku"},
                            structFieldType = TypeConstArray
                              3
                              (TypeConstArray
                                3
                                (TypePrim
                                  (PrimIntegral PrimInt Signed))),
                            structFieldOffset = 96,
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
                    12
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Example"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Example")]
