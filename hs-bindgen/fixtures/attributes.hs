[
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
            "foo_c",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "attributes.h:11:10",
              structFieldName = NamePair {
                nameC = CName "c",
                nameHsIdent = HsIdentifier
                  "foo_c"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "foo_i",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "attributes.h:12:10",
              structFieldName = NamePair {
                nameC = CName "i",
                nameHsIdent = HsIdentifier
                  "foo_i"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 8,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "attributes.h:10:36",
            declId = NamePair {
              nameC = CName "foo",
              nameHsIdent = HsIdentifier
                "Foo"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "attributes.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Foo"),
              structSizeof = 5,
              structAlignment = 1,
              structFields = [
                StructField {
                  structFieldLoc =
                  "attributes.h:11:10",
                  structFieldName = NamePair {
                    nameC = CName "c",
                    nameHsIdent = HsIdentifier
                      "foo_c"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "attributes.h:12:10",
                  structFieldName = NamePair {
                    nameC = CName "i",
                    nameHsIdent = HsIdentifier
                      "foo_i"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 8,
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
          "Foo",
        structConstr = HsName
          "@NsConstr"
          "Foo",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "foo_c",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "attributes.h:11:10",
                structFieldName = NamePair {
                  nameC = CName "c",
                  nameHsIdent = HsIdentifier
                    "foo_c"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "foo_i",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "attributes.h:12:10",
                structFieldName = NamePair {
                  nameC = CName "i",
                  nameHsIdent = HsIdentifier
                    "foo_i"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 8,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "attributes.h:10:36",
              declId = NamePair {
                nameC = CName "foo",
                nameHsIdent = HsIdentifier
                  "Foo"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "attributes.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Foo"),
                structSizeof = 5,
                structAlignment = 1,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "attributes.h:11:10",
                    structFieldName = NamePair {
                      nameC = CName "c",
                      nameHsIdent = HsIdentifier
                        "foo_c"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "attributes.h:12:10",
                    structFieldName = NamePair {
                      nameC = CName "i",
                      nameHsIdent = HsIdentifier
                        "foo_i"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 8,
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
        storableSizeOf = 5,
        storableAlignment = 1,
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
                      "foo_c",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:11:10",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "foo_c"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_i",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:12:10",
                        structFieldName = NamePair {
                          nameC = CName "i",
                          nameHsIdent = HsIdentifier
                            "foo_i"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 8,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "attributes.h:10:36",
                      declId = NamePair {
                        nameC = CName "foo",
                        nameHsIdent = HsIdentifier
                          "Foo"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "attributes.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Foo"),
                        structSizeof = 5,
                        structAlignment = 1,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "attributes.h:11:10",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "foo_c"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "attributes.h:12:10",
                            structFieldName = NamePair {
                              nameC = CName "i",
                              nameHsIdent = HsIdentifier
                                "foo_i"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 8,
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
              PeekByteOff (Idx 0) 1]),
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
                      "foo_c",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:11:10",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "foo_c"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_i",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:12:10",
                        structFieldName = NamePair {
                          nameC = CName "i",
                          nameHsIdent = HsIdentifier
                            "foo_i"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 8,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "attributes.h:10:36",
                      declId = NamePair {
                        nameC = CName "foo",
                        nameHsIdent = HsIdentifier
                          "Foo"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "attributes.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Foo"),
                        structSizeof = 5,
                        structAlignment = 1,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "attributes.h:11:10",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "foo_c"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "attributes.h:12:10",
                            structFieldName = NamePair {
                              nameC = CName "i",
                              nameHsIdent = HsIdentifier
                                "foo_i"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 8,
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
                    1
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Foo"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Bar",
      structConstr = HsName
        "@NsConstr"
        "Bar",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "bar_c",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "attributes.h:17:10",
              structFieldName = NamePair {
                nameC = CName "c",
                nameHsIdent = HsIdentifier
                  "bar_c"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "bar_i",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "attributes.h:18:10",
              structFieldName = NamePair {
                nameC = CName "i",
                nameHsIdent = HsIdentifier
                  "bar_i"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 8,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "attributes.h:16:15",
            declId = NamePair {
              nameC = CName "bar",
              nameHsIdent = HsIdentifier
                "Bar"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "attributes.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Bar"),
              structSizeof = 5,
              structAlignment = 1,
              structFields = [
                StructField {
                  structFieldLoc =
                  "attributes.h:17:10",
                  structFieldName = NamePair {
                    nameC = CName "c",
                    nameHsIdent = HsIdentifier
                      "bar_c"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "attributes.h:18:10",
                  structFieldName = NamePair {
                    nameC = CName "i",
                    nameHsIdent = HsIdentifier
                      "bar_i"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 8,
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
          "Bar",
        structConstr = HsName
          "@NsConstr"
          "Bar",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "bar_c",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "attributes.h:17:10",
                structFieldName = NamePair {
                  nameC = CName "c",
                  nameHsIdent = HsIdentifier
                    "bar_c"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "bar_i",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "attributes.h:18:10",
                structFieldName = NamePair {
                  nameC = CName "i",
                  nameHsIdent = HsIdentifier
                    "bar_i"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 8,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "attributes.h:16:15",
              declId = NamePair {
                nameC = CName "bar",
                nameHsIdent = HsIdentifier
                  "Bar"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "attributes.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Bar"),
                structSizeof = 5,
                structAlignment = 1,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "attributes.h:17:10",
                    structFieldName = NamePair {
                      nameC = CName "c",
                      nameHsIdent = HsIdentifier
                        "bar_c"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "attributes.h:18:10",
                    structFieldName = NamePair {
                      nameC = CName "i",
                      nameHsIdent = HsIdentifier
                        "bar_i"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 8,
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
        storableSizeOf = 5,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Bar",
                structConstr = HsName
                  "@NsConstr"
                  "Bar",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bar_c",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:17:10",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "bar_c"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bar_i",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:18:10",
                        structFieldName = NamePair {
                          nameC = CName "i",
                          nameHsIdent = HsIdentifier
                            "bar_i"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 8,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "attributes.h:16:15",
                      declId = NamePair {
                        nameC = CName "bar",
                        nameHsIdent = HsIdentifier
                          "Bar"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "attributes.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Bar"),
                        structSizeof = 5,
                        structAlignment = 1,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "attributes.h:17:10",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "bar_c"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "attributes.h:18:10",
                            structFieldName = NamePair {
                              nameC = CName "i",
                              nameHsIdent = HsIdentifier
                                "bar_i"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 8,
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
              PeekByteOff (Idx 0) 1]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Bar",
                structConstr = HsName
                  "@NsConstr"
                  "Bar",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bar_c",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:17:10",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "bar_c"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bar_i",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:18:10",
                        structFieldName = NamePair {
                          nameC = CName "i",
                          nameHsIdent = HsIdentifier
                            "bar_i"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 8,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "attributes.h:16:15",
                      declId = NamePair {
                        nameC = CName "bar",
                        nameHsIdent = HsIdentifier
                          "Bar"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "attributes.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Bar"),
                        structSizeof = 5,
                        structAlignment = 1,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "attributes.h:17:10",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "bar_c"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "attributes.h:18:10",
                            structFieldName = NamePair {
                              nameC = CName "i",
                              nameHsIdent = HsIdentifier
                                "bar_i"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 8,
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
                    1
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Bar"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Bar"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Baz",
      structConstr = HsName
        "@NsConstr"
        "Baz",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "baz_c",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "attributes.h:23:10",
              structFieldName = NamePair {
                nameC = CName "c",
                nameHsIdent = HsIdentifier
                  "baz_c"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "baz_i",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "attributes.h:24:10",
              structFieldName = NamePair {
                nameC = CName "i",
                nameHsIdent = HsIdentifier
                  "baz_i"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 8,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "attributes.h:22:9",
            declId = NamePair {
              nameC = CName "baz",
              nameHsIdent = HsIdentifier
                "Baz"},
            declOrigin = NameOriginGenerated
              (AnonId "attributes.h:22:9"),
            declAliases = [CName "baz"],
            declHeader = "attributes.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Baz"),
              structSizeof = 5,
              structAlignment = 1,
              structFields = [
                StructField {
                  structFieldLoc =
                  "attributes.h:23:10",
                  structFieldName = NamePair {
                    nameC = CName "c",
                    nameHsIdent = HsIdentifier
                      "baz_c"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "attributes.h:24:10",
                  structFieldName = NamePair {
                    nameC = CName "i",
                    nameHsIdent = HsIdentifier
                      "baz_i"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 8,
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
          "Baz",
        structConstr = HsName
          "@NsConstr"
          "Baz",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "baz_c",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "attributes.h:23:10",
                structFieldName = NamePair {
                  nameC = CName "c",
                  nameHsIdent = HsIdentifier
                    "baz_c"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "baz_i",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "attributes.h:24:10",
                structFieldName = NamePair {
                  nameC = CName "i",
                  nameHsIdent = HsIdentifier
                    "baz_i"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 8,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "attributes.h:22:9",
              declId = NamePair {
                nameC = CName "baz",
                nameHsIdent = HsIdentifier
                  "Baz"},
              declOrigin = NameOriginGenerated
                (AnonId "attributes.h:22:9"),
              declAliases = [CName "baz"],
              declHeader = "attributes.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Baz"),
                structSizeof = 5,
                structAlignment = 1,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "attributes.h:23:10",
                    structFieldName = NamePair {
                      nameC = CName "c",
                      nameHsIdent = HsIdentifier
                        "baz_c"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "attributes.h:24:10",
                    structFieldName = NamePair {
                      nameC = CName "i",
                      nameHsIdent = HsIdentifier
                        "baz_i"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 8,
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
        storableSizeOf = 5,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Baz",
                structConstr = HsName
                  "@NsConstr"
                  "Baz",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "baz_c",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:23:10",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "baz_c"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "baz_i",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:24:10",
                        structFieldName = NamePair {
                          nameC = CName "i",
                          nameHsIdent = HsIdentifier
                            "baz_i"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 8,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "attributes.h:22:9",
                      declId = NamePair {
                        nameC = CName "baz",
                        nameHsIdent = HsIdentifier
                          "Baz"},
                      declOrigin = NameOriginGenerated
                        (AnonId "attributes.h:22:9"),
                      declAliases = [CName "baz"],
                      declHeader = "attributes.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Baz"),
                        structSizeof = 5,
                        structAlignment = 1,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "attributes.h:23:10",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "baz_c"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "attributes.h:24:10",
                            structFieldName = NamePair {
                              nameC = CName "i",
                              nameHsIdent = HsIdentifier
                                "baz_i"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 8,
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
              PeekByteOff (Idx 0) 1]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Baz",
                structConstr = HsName
                  "@NsConstr"
                  "Baz",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "baz_c",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:23:10",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "baz_c"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "baz_i",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:24:10",
                        structFieldName = NamePair {
                          nameC = CName "i",
                          nameHsIdent = HsIdentifier
                            "baz_i"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 8,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "attributes.h:22:9",
                      declId = NamePair {
                        nameC = CName "baz",
                        nameHsIdent = HsIdentifier
                          "Baz"},
                      declOrigin = NameOriginGenerated
                        (AnonId "attributes.h:22:9"),
                      declAliases = [CName "baz"],
                      declHeader = "attributes.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Baz"),
                        structSizeof = 5,
                        structAlignment = 1,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "attributes.h:23:10",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "baz_c"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "attributes.h:24:10",
                            structFieldName = NamePair {
                              nameC = CName "i",
                              nameHsIdent = HsIdentifier
                                "baz_i"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 8,
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
                    1
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Baz"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Baz"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Qux",
      structConstr = HsName
        "@NsConstr"
        "Qux",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "qux_c",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "attributes.h:29:10",
              structFieldName = NamePair {
                nameC = CName "c",
                nameHsIdent = HsIdentifier
                  "qux_c"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "qux_i",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "attributes.h:30:10",
              structFieldName = NamePair {
                nameC = CName "i",
                nameHsIdent = HsIdentifier
                  "qux_i"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 8,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "attributes.h:28:9",
            declId = NamePair {
              nameC = CName "qux",
              nameHsIdent = HsIdentifier
                "Qux"},
            declOrigin = NameOriginGenerated
              (AnonId "attributes.h:28:9"),
            declAliases = [CName "qux"],
            declHeader = "attributes.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Qux"),
              structSizeof = 5,
              structAlignment = 1,
              structFields = [
                StructField {
                  structFieldLoc =
                  "attributes.h:29:10",
                  structFieldName = NamePair {
                    nameC = CName "c",
                    nameHsIdent = HsIdentifier
                      "qux_c"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "attributes.h:30:10",
                  structFieldName = NamePair {
                    nameC = CName "i",
                    nameHsIdent = HsIdentifier
                      "qux_i"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 8,
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
          "Qux",
        structConstr = HsName
          "@NsConstr"
          "Qux",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "qux_c",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "attributes.h:29:10",
                structFieldName = NamePair {
                  nameC = CName "c",
                  nameHsIdent = HsIdentifier
                    "qux_c"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "qux_i",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "attributes.h:30:10",
                structFieldName = NamePair {
                  nameC = CName "i",
                  nameHsIdent = HsIdentifier
                    "qux_i"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 8,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "attributes.h:28:9",
              declId = NamePair {
                nameC = CName "qux",
                nameHsIdent = HsIdentifier
                  "Qux"},
              declOrigin = NameOriginGenerated
                (AnonId "attributes.h:28:9"),
              declAliases = [CName "qux"],
              declHeader = "attributes.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Qux"),
                structSizeof = 5,
                structAlignment = 1,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "attributes.h:29:10",
                    structFieldName = NamePair {
                      nameC = CName "c",
                      nameHsIdent = HsIdentifier
                        "qux_c"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "attributes.h:30:10",
                    structFieldName = NamePair {
                      nameC = CName "i",
                      nameHsIdent = HsIdentifier
                        "qux_i"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 8,
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
        storableSizeOf = 5,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Qux",
                structConstr = HsName
                  "@NsConstr"
                  "Qux",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "qux_c",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:29:10",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "qux_c"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "qux_i",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:30:10",
                        structFieldName = NamePair {
                          nameC = CName "i",
                          nameHsIdent = HsIdentifier
                            "qux_i"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 8,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "attributes.h:28:9",
                      declId = NamePair {
                        nameC = CName "qux",
                        nameHsIdent = HsIdentifier
                          "Qux"},
                      declOrigin = NameOriginGenerated
                        (AnonId "attributes.h:28:9"),
                      declAliases = [CName "qux"],
                      declHeader = "attributes.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Qux"),
                        structSizeof = 5,
                        structAlignment = 1,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "attributes.h:29:10",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "qux_c"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "attributes.h:30:10",
                            structFieldName = NamePair {
                              nameC = CName "i",
                              nameHsIdent = HsIdentifier
                                "qux_i"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 8,
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
              PeekByteOff (Idx 0) 1]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Qux",
                structConstr = HsName
                  "@NsConstr"
                  "Qux",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "qux_c",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:29:10",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "qux_c"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "qux_i",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:30:10",
                        structFieldName = NamePair {
                          nameC = CName "i",
                          nameHsIdent = HsIdentifier
                            "qux_i"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 8,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "attributes.h:28:9",
                      declId = NamePair {
                        nameC = CName "qux",
                        nameHsIdent = HsIdentifier
                          "Qux"},
                      declOrigin = NameOriginGenerated
                        (AnonId "attributes.h:28:9"),
                      declAliases = [CName "qux"],
                      declHeader = "attributes.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Qux"),
                        structSizeof = 5,
                        structAlignment = 1,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "attributes.h:29:10",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "qux_c"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "attributes.h:30:10",
                            structFieldName = NamePair {
                              nameC = CName "i",
                              nameHsIdent = HsIdentifier
                                "qux_i"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 8,
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
                    1
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Qux"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Qux"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "FILE",
      structConstr = HsName
        "@NsConstr"
        "FILE",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "fILE__r",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "attributes.h:35:9",
              structFieldName = NamePair {
                nameC = CName "_r",
                nameHsIdent = HsIdentifier
                  "fILE__r"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "fILE__w",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "attributes.h:36:9",
              structFieldName = NamePair {
                nameC = CName "_w",
                nameHsIdent = HsIdentifier
                  "fILE__w"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "fILE__close",
          fieldType = HsFunPtr
            (HsFun
              (HsPtr (HsPrimType HsPrimVoid))
              (HsIO (HsPrimType HsPrimCInt))),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "attributes.h:37:22",
              structFieldName = NamePair {
                nameC = CName "_close",
                nameHsIdent = HsIdentifier
                  "fILE__close"},
              structFieldType = TypePointer
                (TypeFun
                  [TypePointer TypeVoid]
                  (TypePrim
                    (PrimIntegral PrimInt Signed))),
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "attributes.h:34:16",
            declId = NamePair {
              nameC = CName "FILE",
              nameHsIdent = HsIdentifier
                "FILE"},
            declOrigin =
            NameOriginRenamedFrom
              (CName "__sFILE"),
            declAliases = [CName "FILE"],
            declHeader = "attributes.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "FILE"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "attributes.h:35:9",
                  structFieldName = NamePair {
                    nameC = CName "_r",
                    nameHsIdent = HsIdentifier
                      "fILE__r"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "attributes.h:36:9",
                  structFieldName = NamePair {
                    nameC = CName "_w",
                    nameHsIdent = HsIdentifier
                      "fILE__w"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "attributes.h:37:22",
                  structFieldName = NamePair {
                    nameC = CName "_close",
                    nameHsIdent = HsIdentifier
                      "fILE__close"},
                  structFieldType = TypePointer
                    (TypeFun
                      [TypePointer TypeVoid]
                      (TypePrim
                        (PrimIntegral PrimInt Signed))),
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
          "FILE",
        structConstr = HsName
          "@NsConstr"
          "FILE",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "fILE__r",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "attributes.h:35:9",
                structFieldName = NamePair {
                  nameC = CName "_r",
                  nameHsIdent = HsIdentifier
                    "fILE__r"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "fILE__w",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "attributes.h:36:9",
                structFieldName = NamePair {
                  nameC = CName "_w",
                  nameHsIdent = HsIdentifier
                    "fILE__w"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 32,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "fILE__close",
            fieldType = HsFunPtr
              (HsFun
                (HsPtr (HsPrimType HsPrimVoid))
                (HsIO (HsPrimType HsPrimCInt))),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "attributes.h:37:22",
                structFieldName = NamePair {
                  nameC = CName "_close",
                  nameHsIdent = HsIdentifier
                    "fILE__close"},
                structFieldType = TypePointer
                  (TypeFun
                    [TypePointer TypeVoid]
                    (TypePrim
                      (PrimIntegral PrimInt Signed))),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "attributes.h:34:16",
              declId = NamePair {
                nameC = CName "FILE",
                nameHsIdent = HsIdentifier
                  "FILE"},
              declOrigin =
              NameOriginRenamedFrom
                (CName "__sFILE"),
              declAliases = [CName "FILE"],
              declHeader = "attributes.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "FILE"),
                structSizeof = 16,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "attributes.h:35:9",
                    structFieldName = NamePair {
                      nameC = CName "_r",
                      nameHsIdent = HsIdentifier
                        "fILE__r"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "attributes.h:36:9",
                    structFieldName = NamePair {
                      nameC = CName "_w",
                      nameHsIdent = HsIdentifier
                        "fILE__w"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 32,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "attributes.h:37:22",
                    structFieldName = NamePair {
                      nameC = CName "_close",
                      nameHsIdent = HsIdentifier
                        "fILE__close"},
                    structFieldType = TypePointer
                      (TypeFun
                        [TypePointer TypeVoid]
                        (TypePrim
                          (PrimIntegral PrimInt Signed))),
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
                  "FILE",
                structConstr = HsName
                  "@NsConstr"
                  "FILE",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "fILE__r",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:35:9",
                        structFieldName = NamePair {
                          nameC = CName "_r",
                          nameHsIdent = HsIdentifier
                            "fILE__r"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "fILE__w",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:36:9",
                        structFieldName = NamePair {
                          nameC = CName "_w",
                          nameHsIdent = HsIdentifier
                            "fILE__w"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "fILE__close",
                    fieldType = HsFunPtr
                      (HsFun
                        (HsPtr (HsPrimType HsPrimVoid))
                        (HsIO (HsPrimType HsPrimCInt))),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:37:22",
                        structFieldName = NamePair {
                          nameC = CName "_close",
                          nameHsIdent = HsIdentifier
                            "fILE__close"},
                        structFieldType = TypePointer
                          (TypeFun
                            [TypePointer TypeVoid]
                            (TypePrim
                              (PrimIntegral PrimInt Signed))),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "attributes.h:34:16",
                      declId = NamePair {
                        nameC = CName "FILE",
                        nameHsIdent = HsIdentifier
                          "FILE"},
                      declOrigin =
                      NameOriginRenamedFrom
                        (CName "__sFILE"),
                      declAliases = [CName "FILE"],
                      declHeader = "attributes.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "FILE"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "attributes.h:35:9",
                            structFieldName = NamePair {
                              nameC = CName "_r",
                              nameHsIdent = HsIdentifier
                                "fILE__r"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "attributes.h:36:9",
                            structFieldName = NamePair {
                              nameC = CName "_w",
                              nameHsIdent = HsIdentifier
                                "fILE__w"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "attributes.h:37:22",
                            structFieldName = NamePair {
                              nameC = CName "_close",
                              nameHsIdent = HsIdentifier
                                "fILE__close"},
                            structFieldType = TypePointer
                              (TypeFun
                                [TypePointer TypeVoid]
                                (TypePrim
                                  (PrimIntegral PrimInt Signed))),
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
              PeekByteOff (Idx 0) 4,
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
                  "FILE",
                structConstr = HsName
                  "@NsConstr"
                  "FILE",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "fILE__r",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:35:9",
                        structFieldName = NamePair {
                          nameC = CName "_r",
                          nameHsIdent = HsIdentifier
                            "fILE__r"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "fILE__w",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:36:9",
                        structFieldName = NamePair {
                          nameC = CName "_w",
                          nameHsIdent = HsIdentifier
                            "fILE__w"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "fILE__close",
                    fieldType = HsFunPtr
                      (HsFun
                        (HsPtr (HsPrimType HsPrimVoid))
                        (HsIO (HsPrimType HsPrimCInt))),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:37:22",
                        structFieldName = NamePair {
                          nameC = CName "_close",
                          nameHsIdent = HsIdentifier
                            "fILE__close"},
                        structFieldType = TypePointer
                          (TypeFun
                            [TypePointer TypeVoid]
                            (TypePrim
                              (PrimIntegral PrimInt Signed))),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "attributes.h:34:16",
                      declId = NamePair {
                        nameC = CName "FILE",
                        nameHsIdent = HsIdentifier
                          "FILE"},
                      declOrigin =
                      NameOriginRenamedFrom
                        (CName "__sFILE"),
                      declAliases = [CName "FILE"],
                      declHeader = "attributes.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "FILE"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "attributes.h:35:9",
                            structFieldName = NamePair {
                              nameC = CName "_r",
                              nameHsIdent = HsIdentifier
                                "fILE__r"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "attributes.h:36:9",
                            structFieldName = NamePair {
                              nameC = CName "_w",
                              nameHsIdent = HsIdentifier
                                "fILE__w"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "attributes.h:37:22",
                            structFieldName = NamePair {
                              nameC = CName "_close",
                              nameHsIdent = HsIdentifier
                                "fILE__close"},
                            structFieldType = TypePointer
                              (TypeFun
                                [TypePointer TypeVoid]
                                (TypePrim
                                  (PrimIntegral PrimInt Signed))),
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
              (Add 3)
              (Seq
                [
                  PokeByteOff (Idx 4) 0 (Idx 0),
                  PokeByteOff (Idx 4) 4 (Idx 1),
                  PokeByteOff
                    (Idx 4)
                    8
                    (Idx 2)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "FILE"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "FILE")]
