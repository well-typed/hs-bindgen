[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Pascal",
      structConstr = HsName
        "@NsConstr"
        "Pascal",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "pascal_len",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "flam.h:3:9",
              structFieldName = NamePair {
                nameC = CName "len",
                nameHsIdent = HsIdentifier
                  "pascal_len"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "flam.h:2:8",
            declId = NamePair {
              nameC = CName "pascal",
              nameHsIdent = HsIdentifier
                "Pascal"},
            declOrigin = NameOriginInSource,
            declAliases = []},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Pascal"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc = "flam.h:3:9",
                  structFieldName = NamePair {
                    nameC = CName "len",
                    nameHsIdent = HsIdentifier
                      "pascal_len"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing}],
              structFlam = Just
                StructField {
                  structFieldLoc = "flam.h:4:10",
                  structFieldName = NamePair {
                    nameC = CName "data",
                    nameHsIdent = HsIdentifier
                      "pascal_data"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing}},
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
          "Pascal",
        structConstr = HsName
          "@NsConstr"
          "Pascal",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "pascal_len",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc = "flam.h:3:9",
                structFieldName = NamePair {
                  nameC = CName "len",
                  nameHsIdent = HsIdentifier
                    "pascal_len"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "flam.h:2:8",
              declId = NamePair {
                nameC = CName "pascal",
                nameHsIdent = HsIdentifier
                  "Pascal"},
              declOrigin = NameOriginInSource,
              declAliases = []},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Pascal"),
                structSizeof = 4,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc = "flam.h:3:9",
                    structFieldName = NamePair {
                      nameC = CName "len",
                      nameHsIdent = HsIdentifier
                        "pascal_len"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing}],
                structFlam = Just
                  StructField {
                    structFieldLoc = "flam.h:4:10",
                    structFieldName = NamePair {
                      nameC = CName "data",
                      nameHsIdent = HsIdentifier
                        "pascal_data"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
                    structFieldOffset = 32,
                    structFieldWidth = Nothing}},
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
                  "Pascal",
                structConstr = HsName
                  "@NsConstr"
                  "Pascal",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "pascal_len",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc = "flam.h:3:9",
                        structFieldName = NamePair {
                          nameC = CName "len",
                          nameHsIdent = HsIdentifier
                            "pascal_len"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "flam.h:2:8",
                      declId = NamePair {
                        nameC = CName "pascal",
                        nameHsIdent = HsIdentifier
                          "Pascal"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Pascal"),
                        structSizeof = 4,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc = "flam.h:3:9",
                            structFieldName = NamePair {
                              nameC = CName "len",
                              nameHsIdent = HsIdentifier
                                "pascal_len"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing}],
                        structFlam = Just
                          StructField {
                            structFieldLoc = "flam.h:4:10",
                            structFieldName = NamePair {
                              nameC = CName "data",
                              nameHsIdent = HsIdentifier
                                "pascal_data"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 32,
                            structFieldWidth = Nothing}},
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
                  "Pascal",
                structConstr = HsName
                  "@NsConstr"
                  "Pascal",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "pascal_len",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc = "flam.h:3:9",
                        structFieldName = NamePair {
                          nameC = CName "len",
                          nameHsIdent = HsIdentifier
                            "pascal_len"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "flam.h:2:8",
                      declId = NamePair {
                        nameC = CName "pascal",
                        nameHsIdent = HsIdentifier
                          "Pascal"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Pascal"),
                        structSizeof = 4,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc = "flam.h:3:9",
                            structFieldName = NamePair {
                              nameC = CName "len",
                              nameHsIdent = HsIdentifier
                                "pascal_len"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing}],
                        structFlam = Just
                          StructField {
                            structFieldLoc = "flam.h:4:10",
                            structFieldName = NamePair {
                              nameC = CName "data",
                              nameHsIdent = HsIdentifier
                                "pascal_data"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 32,
                            structFieldWidth = Nothing}},
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
      "Pascal"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Pascal"),
  DeclInstance
    (InstanceHasFLAM
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Pascal",
        structConstr = HsName
          "@NsConstr"
          "Pascal",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "pascal_len",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc = "flam.h:3:9",
                structFieldName = NamePair {
                  nameC = CName "len",
                  nameHsIdent = HsIdentifier
                    "pascal_len"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "flam.h:2:8",
              declId = NamePair {
                nameC = CName "pascal",
                nameHsIdent = HsIdentifier
                  "Pascal"},
              declOrigin = NameOriginInSource,
              declAliases = []},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Pascal"),
                structSizeof = 4,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc = "flam.h:3:9",
                    structFieldName = NamePair {
                      nameC = CName "len",
                      nameHsIdent = HsIdentifier
                        "pascal_len"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing}],
                structFlam = Just
                  StructField {
                    structFieldLoc = "flam.h:4:10",
                    structFieldName = NamePair {
                      nameC = CName "data",
                      nameHsIdent = HsIdentifier
                        "pascal_data"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
                    structFieldOffset = 32,
                    structFieldWidth = Nothing}},
            declSpec = DeclSpec
              TypeSpec {
                typeSpecModule = Nothing,
                typeSpecIdentifier = Nothing,
                typeSpecInstances = Map.fromList
                  []}},
        structInstances = Set.fromList
          [Eq, Show, Storable]}
      (HsPrimType HsPrimCChar)
      4),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Foo_bar",
      structConstr = HsName
        "@NsConstr"
        "Foo_bar",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "foo_bar_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "flam.h:11:7",
              structFieldName = NamePair {
                nameC = CName "x",
                nameHsIdent = HsIdentifier
                  "foo_bar_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "foo_bar_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "flam.h:12:7",
              structFieldName = NamePair {
                nameC = CName "y",
                nameHsIdent = HsIdentifier
                  "foo_bar_y"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "flam.h:10:2",
            declId = NamePair {
              nameC = CName "foo_bar",
              nameHsIdent = HsIdentifier
                "Foo_bar"},
            declOrigin =
            NameOriginGenerated,
            declAliases = []},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Foo_bar"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc = "flam.h:11:7",
                  structFieldName = NamePair {
                    nameC = CName "x",
                    nameHsIdent = HsIdentifier
                      "foo_bar_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc = "flam.h:12:7",
                  structFieldName = NamePair {
                    nameC = CName "y",
                    nameHsIdent = HsIdentifier
                      "foo_bar_y"},
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
          "Foo_bar",
        structConstr = HsName
          "@NsConstr"
          "Foo_bar",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "foo_bar_x",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc = "flam.h:11:7",
                structFieldName = NamePair {
                  nameC = CName "x",
                  nameHsIdent = HsIdentifier
                    "foo_bar_x"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "foo_bar_y",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc = "flam.h:12:7",
                structFieldName = NamePair {
                  nameC = CName "y",
                  nameHsIdent = HsIdentifier
                    "foo_bar_y"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 32,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "flam.h:10:2",
              declId = NamePair {
                nameC = CName "foo_bar",
                nameHsIdent = HsIdentifier
                  "Foo_bar"},
              declOrigin =
              NameOriginGenerated,
              declAliases = []},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Foo_bar"),
                structSizeof = 8,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc = "flam.h:11:7",
                    structFieldName = NamePair {
                      nameC = CName "x",
                      nameHsIdent = HsIdentifier
                        "foo_bar_x"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc = "flam.h:12:7",
                    structFieldName = NamePair {
                      nameC = CName "y",
                      nameHsIdent = HsIdentifier
                        "foo_bar_y"},
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
                  "Foo_bar",
                structConstr = HsName
                  "@NsConstr"
                  "Foo_bar",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_bar_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc = "flam.h:11:7",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "foo_bar_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_bar_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc = "flam.h:12:7",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "foo_bar_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "flam.h:10:2",
                      declId = NamePair {
                        nameC = CName "foo_bar",
                        nameHsIdent = HsIdentifier
                          "Foo_bar"},
                      declOrigin =
                      NameOriginGenerated,
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Foo_bar"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc = "flam.h:11:7",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "foo_bar_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc = "flam.h:12:7",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "foo_bar_y"},
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
                  "Foo_bar",
                structConstr = HsName
                  "@NsConstr"
                  "Foo_bar",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_bar_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc = "flam.h:11:7",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "foo_bar_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_bar_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc = "flam.h:12:7",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "foo_bar_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "flam.h:10:2",
                      declId = NamePair {
                        nameC = CName "foo_bar",
                        nameHsIdent = HsIdentifier
                          "Foo_bar"},
                      declOrigin =
                      NameOriginGenerated,
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Foo_bar"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc = "flam.h:11:7",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "foo_bar_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc = "flam.h:12:7",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "foo_bar_y"},
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
      "Foo_bar"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Foo_bar"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Foo",
      structConstr = HsName
        "@NsConstr"
        "Foo",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "foo_len",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "flam.h:9:6",
              structFieldName = NamePair {
                nameC = CName "len",
                nameHsIdent = HsIdentifier
                  "foo_len"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "flam.h:8:8",
            declId = NamePair {
              nameC = CName "foo",
              nameHsIdent = HsIdentifier
                "Foo"},
            declOrigin = NameOriginInSource,
            declAliases = []},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Foo"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc = "flam.h:9:6",
                  structFieldName = NamePair {
                    nameC = CName "len",
                    nameHsIdent = HsIdentifier
                      "foo_len"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing}],
              structFlam = Just
                StructField {
                  structFieldLoc = "flam.h:13:4",
                  structFieldName = NamePair {
                    nameC = CName "bar",
                    nameHsIdent = HsIdentifier
                      "foo_bar"},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = CName "foo_bar",
                      nameHsIdent = HsIdentifier
                        "Foo_bar"}
                    NameOriginGenerated,
                  structFieldOffset = 32,
                  structFieldWidth = Nothing}},
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
          "Foo",
        structConstr = HsName
          "@NsConstr"
          "Foo",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "foo_len",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc = "flam.h:9:6",
                structFieldName = NamePair {
                  nameC = CName "len",
                  nameHsIdent = HsIdentifier
                    "foo_len"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "flam.h:8:8",
              declId = NamePair {
                nameC = CName "foo",
                nameHsIdent = HsIdentifier
                  "Foo"},
              declOrigin = NameOriginInSource,
              declAliases = []},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Foo"),
                structSizeof = 4,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc = "flam.h:9:6",
                    structFieldName = NamePair {
                      nameC = CName "len",
                      nameHsIdent = HsIdentifier
                        "foo_len"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing}],
                structFlam = Just
                  StructField {
                    structFieldLoc = "flam.h:13:4",
                    structFieldName = NamePair {
                      nameC = CName "bar",
                      nameHsIdent = HsIdentifier
                        "foo_bar"},
                    structFieldType = TypeStruct
                      NamePair {
                        nameC = CName "foo_bar",
                        nameHsIdent = HsIdentifier
                          "Foo_bar"}
                      NameOriginGenerated,
                    structFieldOffset = 32,
                    structFieldWidth = Nothing}},
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
                  "Foo",
                structConstr = HsName
                  "@NsConstr"
                  "Foo",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_len",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc = "flam.h:9:6",
                        structFieldName = NamePair {
                          nameC = CName "len",
                          nameHsIdent = HsIdentifier
                            "foo_len"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "flam.h:8:8",
                      declId = NamePair {
                        nameC = CName "foo",
                        nameHsIdent = HsIdentifier
                          "Foo"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Foo"),
                        structSizeof = 4,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc = "flam.h:9:6",
                            structFieldName = NamePair {
                              nameC = CName "len",
                              nameHsIdent = HsIdentifier
                                "foo_len"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing}],
                        structFlam = Just
                          StructField {
                            structFieldLoc = "flam.h:13:4",
                            structFieldName = NamePair {
                              nameC = CName "bar",
                              nameHsIdent = HsIdentifier
                                "foo_bar"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = CName "foo_bar",
                                nameHsIdent = HsIdentifier
                                  "Foo_bar"}
                              NameOriginGenerated,
                            structFieldOffset = 32,
                            structFieldWidth = Nothing}},
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
                  "Foo",
                structConstr = HsName
                  "@NsConstr"
                  "Foo",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_len",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc = "flam.h:9:6",
                        structFieldName = NamePair {
                          nameC = CName "len",
                          nameHsIdent = HsIdentifier
                            "foo_len"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "flam.h:8:8",
                      declId = NamePair {
                        nameC = CName "foo",
                        nameHsIdent = HsIdentifier
                          "Foo"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Foo"),
                        structSizeof = 4,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc = "flam.h:9:6",
                            structFieldName = NamePair {
                              nameC = CName "len",
                              nameHsIdent = HsIdentifier
                                "foo_len"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing}],
                        structFlam = Just
                          StructField {
                            structFieldLoc = "flam.h:13:4",
                            structFieldName = NamePair {
                              nameC = CName "bar",
                              nameHsIdent = HsIdentifier
                                "foo_bar"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = CName "foo_bar",
                                nameHsIdent = HsIdentifier
                                  "Foo_bar"}
                              NameOriginGenerated,
                            structFieldOffset = 32,
                            structFieldWidth = Nothing}},
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
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Foo"),
  DeclInstance
    (InstanceHasFLAM
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Foo",
        structConstr = HsName
          "@NsConstr"
          "Foo",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "foo_len",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc = "flam.h:9:6",
                structFieldName = NamePair {
                  nameC = CName "len",
                  nameHsIdent = HsIdentifier
                    "foo_len"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "flam.h:8:8",
              declId = NamePair {
                nameC = CName "foo",
                nameHsIdent = HsIdentifier
                  "Foo"},
              declOrigin = NameOriginInSource,
              declAliases = []},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Foo"),
                structSizeof = 4,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc = "flam.h:9:6",
                    structFieldName = NamePair {
                      nameC = CName "len",
                      nameHsIdent = HsIdentifier
                        "foo_len"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing}],
                structFlam = Just
                  StructField {
                    structFieldLoc = "flam.h:13:4",
                    structFieldName = NamePair {
                      nameC = CName "bar",
                      nameHsIdent = HsIdentifier
                        "foo_bar"},
                    structFieldType = TypeStruct
                      NamePair {
                        nameC = CName "foo_bar",
                        nameHsIdent = HsIdentifier
                          "Foo_bar"}
                      NameOriginGenerated,
                    structFieldOffset = 32,
                    structFieldWidth = Nothing}},
            declSpec = DeclSpec
              TypeSpec {
                typeSpecModule = Nothing,
                typeSpecIdentifier = Nothing,
                typeSpecInstances = Map.fromList
                  []}},
        structInstances = Set.fromList
          [Eq, Show, Storable]}
      (HsTypRef
        (HsName
          "@NsTypeConstr"
          "Foo_bar"))
      4),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Diff",
      structConstr = HsName
        "@NsConstr"
        "Diff",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "diff_first",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "flam.h:18:7",
              structFieldName = NamePair {
                nameC = CName "first",
                nameHsIdent = HsIdentifier
                  "diff_first"},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "diff_second",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "flam.h:19:7",
              structFieldName = NamePair {
                nameC = CName "second",
                nameHsIdent = HsIdentifier
                  "diff_second"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "flam.h:17:8",
            declId = NamePair {
              nameC = CName "diff",
              nameHsIdent = HsIdentifier
                "Diff"},
            declOrigin = NameOriginInSource,
            declAliases = []},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Diff"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc = "flam.h:18:7",
                  structFieldName = NamePair {
                    nameC = CName "first",
                    nameHsIdent = HsIdentifier
                      "diff_first"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc = "flam.h:19:7",
                  structFieldName = NamePair {
                    nameC = CName "second",
                    nameHsIdent = HsIdentifier
                      "diff_second"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing}],
              structFlam = Just
                StructField {
                  structFieldLoc = "flam.h:20:7",
                  structFieldName = NamePair {
                    nameC = CName "flam",
                    nameHsIdent = HsIdentifier
                      "diff_flam"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 72,
                  structFieldWidth = Nothing}},
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
          "Diff",
        structConstr = HsName
          "@NsConstr"
          "Diff",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "diff_first",
            fieldType = HsPrimType
              HsPrimCLong,
            fieldOrigin = StructField
              StructField {
                structFieldLoc = "flam.h:18:7",
                structFieldName = NamePair {
                  nameC = CName "first",
                  nameHsIdent = HsIdentifier
                    "diff_first"},
                structFieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "diff_second",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc = "flam.h:19:7",
                structFieldName = NamePair {
                  nameC = CName "second",
                  nameHsIdent = HsIdentifier
                    "diff_second"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "flam.h:17:8",
              declId = NamePair {
                nameC = CName "diff",
                nameHsIdent = HsIdentifier
                  "Diff"},
              declOrigin = NameOriginInSource,
              declAliases = []},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Diff"),
                structSizeof = 16,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc = "flam.h:18:7",
                    structFieldName = NamePair {
                      nameC = CName "first",
                      nameHsIdent = HsIdentifier
                        "diff_first"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimLong Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc = "flam.h:19:7",
                    structFieldName = NamePair {
                      nameC = CName "second",
                      nameHsIdent = HsIdentifier
                        "diff_second"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
                    structFieldOffset = 64,
                    structFieldWidth = Nothing}],
                structFlam = Just
                  StructField {
                    structFieldLoc = "flam.h:20:7",
                    structFieldName = NamePair {
                      nameC = CName "flam",
                      nameHsIdent = HsIdentifier
                        "diff_flam"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
                    structFieldOffset = 72,
                    structFieldWidth = Nothing}},
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
                  "Diff",
                structConstr = HsName
                  "@NsConstr"
                  "Diff",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "diff_first",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc = "flam.h:18:7",
                        structFieldName = NamePair {
                          nameC = CName "first",
                          nameHsIdent = HsIdentifier
                            "diff_first"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "diff_second",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc = "flam.h:19:7",
                        structFieldName = NamePair {
                          nameC = CName "second",
                          nameHsIdent = HsIdentifier
                            "diff_second"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "flam.h:17:8",
                      declId = NamePair {
                        nameC = CName "diff",
                        nameHsIdent = HsIdentifier
                          "Diff"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Diff"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc = "flam.h:18:7",
                            structFieldName = NamePair {
                              nameC = CName "first",
                              nameHsIdent = HsIdentifier
                                "diff_first"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc = "flam.h:19:7",
                            structFieldName = NamePair {
                              nameC = CName "second",
                              nameHsIdent = HsIdentifier
                                "diff_second"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 64,
                            structFieldWidth = Nothing}],
                        structFlam = Just
                          StructField {
                            structFieldLoc = "flam.h:20:7",
                            structFieldName = NamePair {
                              nameC = CName "flam",
                              nameHsIdent = HsIdentifier
                                "diff_flam"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 72,
                            structFieldWidth = Nothing}},
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
                  "Diff",
                structConstr = HsName
                  "@NsConstr"
                  "Diff",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "diff_first",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc = "flam.h:18:7",
                        structFieldName = NamePair {
                          nameC = CName "first",
                          nameHsIdent = HsIdentifier
                            "diff_first"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "diff_second",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc = "flam.h:19:7",
                        structFieldName = NamePair {
                          nameC = CName "second",
                          nameHsIdent = HsIdentifier
                            "diff_second"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "flam.h:17:8",
                      declId = NamePair {
                        nameC = CName "diff",
                        nameHsIdent = HsIdentifier
                          "Diff"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Diff"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc = "flam.h:18:7",
                            structFieldName = NamePair {
                              nameC = CName "first",
                              nameHsIdent = HsIdentifier
                                "diff_first"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc = "flam.h:19:7",
                            structFieldName = NamePair {
                              nameC = CName "second",
                              nameHsIdent = HsIdentifier
                                "diff_second"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 64,
                            structFieldWidth = Nothing}],
                        structFlam = Just
                          StructField {
                            structFieldLoc = "flam.h:20:7",
                            structFieldName = NamePair {
                              nameC = CName "flam",
                              nameHsIdent = HsIdentifier
                                "diff_flam"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 72,
                            structFieldWidth = Nothing}},
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
    (HsName "@NsTypeConstr" "Diff"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Diff"),
  DeclInstance
    (InstanceHasFLAM
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Diff",
        structConstr = HsName
          "@NsConstr"
          "Diff",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "diff_first",
            fieldType = HsPrimType
              HsPrimCLong,
            fieldOrigin = StructField
              StructField {
                structFieldLoc = "flam.h:18:7",
                structFieldName = NamePair {
                  nameC = CName "first",
                  nameHsIdent = HsIdentifier
                    "diff_first"},
                structFieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "diff_second",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc = "flam.h:19:7",
                structFieldName = NamePair {
                  nameC = CName "second",
                  nameHsIdent = HsIdentifier
                    "diff_second"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "flam.h:17:8",
              declId = NamePair {
                nameC = CName "diff",
                nameHsIdent = HsIdentifier
                  "Diff"},
              declOrigin = NameOriginInSource,
              declAliases = []},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Diff"),
                structSizeof = 16,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc = "flam.h:18:7",
                    structFieldName = NamePair {
                      nameC = CName "first",
                      nameHsIdent = HsIdentifier
                        "diff_first"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimLong Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc = "flam.h:19:7",
                    structFieldName = NamePair {
                      nameC = CName "second",
                      nameHsIdent = HsIdentifier
                        "diff_second"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
                    structFieldOffset = 64,
                    structFieldWidth = Nothing}],
                structFlam = Just
                  StructField {
                    structFieldLoc = "flam.h:20:7",
                    structFieldName = NamePair {
                      nameC = CName "flam",
                      nameHsIdent = HsIdentifier
                        "diff_flam"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
                    structFieldOffset = 72,
                    structFieldWidth = Nothing}},
            declSpec = DeclSpec
              TypeSpec {
                typeSpecModule = Nothing,
                typeSpecIdentifier = Nothing,
                typeSpecInstances = Map.fromList
                  []}},
        structInstances = Set.fromList
          [Eq, Show, Storable]}
      (HsPrimType HsPrimCChar)
      9)]
