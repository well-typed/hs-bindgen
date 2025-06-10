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
            "foo_i",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "nested_types.h:2:9",
              structFieldName = NamePair {
                nameC = CName "i",
                nameHsIdent = HsIdentifier
                  "foo_i"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "foo_c",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "nested_types.h:3:10",
              structFieldName = NamePair {
                nameC = CName "c",
                nameHsIdent = HsIdentifier
                  "foo_c"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 32,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "nested_types.h:1:8",
            declId = NamePair {
              nameC = CName "foo",
              nameHsIdent = HsIdentifier
                "Foo"}},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Foo"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "nested_types.h:2:9",
                  structFieldName = NamePair {
                    nameC = CName "i",
                    nameHsIdent = HsIdentifier
                      "foo_i"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "nested_types.h:3:10",
                  structFieldName = NamePair {
                    nameC = CName "c",
                    nameHsIdent = HsIdentifier
                      "foo_c"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
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
          "Foo",
        structConstr = HsName
          "@NsConstr"
          "Foo",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "foo_i",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "nested_types.h:2:9",
                structFieldName = NamePair {
                  nameC = CName "i",
                  nameHsIdent = HsIdentifier
                    "foo_i"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "foo_c",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "nested_types.h:3:10",
                structFieldName = NamePair {
                  nameC = CName "c",
                  nameHsIdent = HsIdentifier
                    "foo_c"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                structFieldOffset = 32,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "nested_types.h:1:8",
              declId = NamePair {
                nameC = CName "foo",
                nameHsIdent = HsIdentifier
                  "Foo"}},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Foo"),
                structSizeof = 8,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "nested_types.h:2:9",
                    structFieldName = NamePair {
                      nameC = CName "i",
                      nameHsIdent = HsIdentifier
                        "foo_i"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "nested_types.h:3:10",
                    structFieldName = NamePair {
                      nameC = CName "c",
                      nameHsIdent = HsIdentifier
                        "foo_c"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
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
                  "Foo",
                structConstr = HsName
                  "@NsConstr"
                  "Foo",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_i",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:2:9",
                        structFieldName = NamePair {
                          nameC = CName "i",
                          nameHsIdent = HsIdentifier
                            "foo_i"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_c",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:3:10",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "foo_c"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "nested_types.h:1:8",
                      declId = NamePair {
                        nameC = CName "foo",
                        nameHsIdent = HsIdentifier
                          "Foo"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Foo"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "nested_types.h:2:9",
                            structFieldName = NamePair {
                              nameC = CName "i",
                              nameHsIdent = HsIdentifier
                                "foo_i"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "nested_types.h:3:10",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "foo_c"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
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
                  "Foo",
                structConstr = HsName
                  "@NsConstr"
                  "Foo",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_i",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:2:9",
                        structFieldName = NamePair {
                          nameC = CName "i",
                          nameHsIdent = HsIdentifier
                            "foo_i"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_c",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:3:10",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "foo_c"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "nested_types.h:1:8",
                      declId = NamePair {
                        nameC = CName "foo",
                        nameHsIdent = HsIdentifier
                          "Foo"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Foo"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "nested_types.h:2:9",
                            structFieldName = NamePair {
                              nameC = CName "i",
                              nameHsIdent = HsIdentifier
                                "foo_i"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "nested_types.h:3:10",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "foo_c"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
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
            "bar_foo1",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "Foo"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "nested_types.h:7:16",
              structFieldName = NamePair {
                nameC = CName "foo1",
                nameHsIdent = HsIdentifier
                  "bar_foo1"},
              structFieldType = TypeStruct
                NamePair {
                  nameC = CName "foo",
                  nameHsIdent = HsIdentifier
                    "Foo"},
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "bar_foo2",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "Foo"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "nested_types.h:8:16",
              structFieldName = NamePair {
                nameC = CName "foo2",
                nameHsIdent = HsIdentifier
                  "bar_foo2"},
              structFieldType = TypeStruct
                NamePair {
                  nameC = CName "foo",
                  nameHsIdent = HsIdentifier
                    "Foo"},
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "nested_types.h:6:8",
            declId = NamePair {
              nameC = CName "bar",
              nameHsIdent = HsIdentifier
                "Bar"}},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Bar"),
              structSizeof = 16,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "nested_types.h:7:16",
                  structFieldName = NamePair {
                    nameC = CName "foo1",
                    nameHsIdent = HsIdentifier
                      "bar_foo1"},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = CName "foo",
                      nameHsIdent = HsIdentifier
                        "Foo"},
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "nested_types.h:8:16",
                  structFieldName = NamePair {
                    nameC = CName "foo2",
                    nameHsIdent = HsIdentifier
                      "bar_foo2"},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = CName "foo",
                      nameHsIdent = HsIdentifier
                        "Foo"},
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
          "Bar",
        structConstr = HsName
          "@NsConstr"
          "Bar",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "bar_foo1",
            fieldType = HsTypRef
              (HsName "@NsTypeConstr" "Foo"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "nested_types.h:7:16",
                structFieldName = NamePair {
                  nameC = CName "foo1",
                  nameHsIdent = HsIdentifier
                    "bar_foo1"},
                structFieldType = TypeStruct
                  NamePair {
                    nameC = CName "foo",
                    nameHsIdent = HsIdentifier
                      "Foo"},
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "bar_foo2",
            fieldType = HsTypRef
              (HsName "@NsTypeConstr" "Foo"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "nested_types.h:8:16",
                structFieldName = NamePair {
                  nameC = CName "foo2",
                  nameHsIdent = HsIdentifier
                    "bar_foo2"},
                structFieldType = TypeStruct
                  NamePair {
                    nameC = CName "foo",
                    nameHsIdent = HsIdentifier
                      "Foo"},
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "nested_types.h:6:8",
              declId = NamePair {
                nameC = CName "bar",
                nameHsIdent = HsIdentifier
                  "Bar"}},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Bar"),
                structSizeof = 16,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "nested_types.h:7:16",
                    structFieldName = NamePair {
                      nameC = CName "foo1",
                      nameHsIdent = HsIdentifier
                        "bar_foo1"},
                    structFieldType = TypeStruct
                      NamePair {
                        nameC = CName "foo",
                        nameHsIdent = HsIdentifier
                          "Foo"},
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "nested_types.h:8:16",
                    structFieldName = NamePair {
                      nameC = CName "foo2",
                      nameHsIdent = HsIdentifier
                        "bar_foo2"},
                    structFieldType = TypeStruct
                      NamePair {
                        nameC = CName "foo",
                        nameHsIdent = HsIdentifier
                          "Foo"},
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
        storableAlignment = 4,
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
                      "bar_foo1",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "Foo"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:7:16",
                        structFieldName = NamePair {
                          nameC = CName "foo1",
                          nameHsIdent = HsIdentifier
                            "bar_foo1"},
                        structFieldType = TypeStruct
                          NamePair {
                            nameC = CName "foo",
                            nameHsIdent = HsIdentifier
                              "Foo"},
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bar_foo2",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "Foo"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:8:16",
                        structFieldName = NamePair {
                          nameC = CName "foo2",
                          nameHsIdent = HsIdentifier
                            "bar_foo2"},
                        structFieldType = TypeStruct
                          NamePair {
                            nameC = CName "foo",
                            nameHsIdent = HsIdentifier
                              "Foo"},
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "nested_types.h:6:8",
                      declId = NamePair {
                        nameC = CName "bar",
                        nameHsIdent = HsIdentifier
                          "Bar"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Bar"),
                        structSizeof = 16,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "nested_types.h:7:16",
                            structFieldName = NamePair {
                              nameC = CName "foo1",
                              nameHsIdent = HsIdentifier
                                "bar_foo1"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = CName "foo",
                                nameHsIdent = HsIdentifier
                                  "Foo"},
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "nested_types.h:8:16",
                            structFieldName = NamePair {
                              nameC = CName "foo2",
                              nameHsIdent = HsIdentifier
                                "bar_foo2"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = CName "foo",
                                nameHsIdent = HsIdentifier
                                  "Foo"},
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
                  "Bar",
                structConstr = HsName
                  "@NsConstr"
                  "Bar",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bar_foo1",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "Foo"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:7:16",
                        structFieldName = NamePair {
                          nameC = CName "foo1",
                          nameHsIdent = HsIdentifier
                            "bar_foo1"},
                        structFieldType = TypeStruct
                          NamePair {
                            nameC = CName "foo",
                            nameHsIdent = HsIdentifier
                              "Foo"},
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bar_foo2",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "Foo"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:8:16",
                        structFieldName = NamePair {
                          nameC = CName "foo2",
                          nameHsIdent = HsIdentifier
                            "bar_foo2"},
                        structFieldType = TypeStruct
                          NamePair {
                            nameC = CName "foo",
                            nameHsIdent = HsIdentifier
                              "Foo"},
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "nested_types.h:6:8",
                      declId = NamePair {
                        nameC = CName "bar",
                        nameHsIdent = HsIdentifier
                          "Bar"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Bar"),
                        structSizeof = 16,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "nested_types.h:7:16",
                            structFieldName = NamePair {
                              nameC = CName "foo1",
                              nameHsIdent = HsIdentifier
                                "bar_foo1"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = CName "foo",
                                nameHsIdent = HsIdentifier
                                  "Foo"},
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "nested_types.h:8:16",
                            structFieldName = NamePair {
                              nameC = CName "foo2",
                              nameHsIdent = HsIdentifier
                                "bar_foo2"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = CName "foo",
                                nameHsIdent = HsIdentifier
                                  "Foo"},
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
    (HsName "@NsTypeConstr" "Bar"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Bar"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Ex3_ex3_struct",
      structConstr = HsName
        "@NsConstr"
        "Ex3_ex3_struct",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "ex3_ex3_struct_ex3_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "nested_types.h:13:13",
              structFieldName = NamePair {
                nameC = CName "ex3_a",
                nameHsIdent = HsIdentifier
                  "ex3_ex3_struct_ex3_a"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "ex3_ex3_struct_ex3_b",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "nested_types.h:14:14",
              structFieldName = NamePair {
                nameC = CName "ex3_b",
                nameHsIdent = HsIdentifier
                  "ex3_ex3_struct_ex3_b"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 32,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "nested_types.h:12:5",
            declId = NamePair {
              nameC = CName "ex3_ex3_struct",
              nameHsIdent = HsIdentifier
                "Ex3_ex3_struct"}},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Ex3_ex3_struct"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "nested_types.h:13:13",
                  structFieldName = NamePair {
                    nameC = CName "ex3_a",
                    nameHsIdent = HsIdentifier
                      "ex3_ex3_struct_ex3_a"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "nested_types.h:14:14",
                  structFieldName = NamePair {
                    nameC = CName "ex3_b",
                    nameHsIdent = HsIdentifier
                      "ex3_ex3_struct_ex3_b"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
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
          "Ex3_ex3_struct",
        structConstr = HsName
          "@NsConstr"
          "Ex3_ex3_struct",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "ex3_ex3_struct_ex3_a",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "nested_types.h:13:13",
                structFieldName = NamePair {
                  nameC = CName "ex3_a",
                  nameHsIdent = HsIdentifier
                    "ex3_ex3_struct_ex3_a"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "ex3_ex3_struct_ex3_b",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "nested_types.h:14:14",
                structFieldName = NamePair {
                  nameC = CName "ex3_b",
                  nameHsIdent = HsIdentifier
                    "ex3_ex3_struct_ex3_b"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                structFieldOffset = 32,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "nested_types.h:12:5",
              declId = NamePair {
                nameC = CName "ex3_ex3_struct",
                nameHsIdent = HsIdentifier
                  "Ex3_ex3_struct"}},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "Ex3_ex3_struct"),
                structSizeof = 8,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "nested_types.h:13:13",
                    structFieldName = NamePair {
                      nameC = CName "ex3_a",
                      nameHsIdent = HsIdentifier
                        "ex3_ex3_struct_ex3_a"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "nested_types.h:14:14",
                    structFieldName = NamePair {
                      nameC = CName "ex3_b",
                      nameHsIdent = HsIdentifier
                        "ex3_ex3_struct_ex3_b"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
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
                  "Ex3_ex3_struct",
                structConstr = HsName
                  "@NsConstr"
                  "Ex3_ex3_struct",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex3_ex3_struct_ex3_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:13:13",
                        structFieldName = NamePair {
                          nameC = CName "ex3_a",
                          nameHsIdent = HsIdentifier
                            "ex3_ex3_struct_ex3_a"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex3_ex3_struct_ex3_b",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:14:14",
                        structFieldName = NamePair {
                          nameC = CName "ex3_b",
                          nameHsIdent = HsIdentifier
                            "ex3_ex3_struct_ex3_b"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "nested_types.h:12:5",
                      declId = NamePair {
                        nameC = CName "ex3_ex3_struct",
                        nameHsIdent = HsIdentifier
                          "Ex3_ex3_struct"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Ex3_ex3_struct"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "nested_types.h:13:13",
                            structFieldName = NamePair {
                              nameC = CName "ex3_a",
                              nameHsIdent = HsIdentifier
                                "ex3_ex3_struct_ex3_a"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "nested_types.h:14:14",
                            structFieldName = NamePair {
                              nameC = CName "ex3_b",
                              nameHsIdent = HsIdentifier
                                "ex3_ex3_struct_ex3_b"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
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
                  "Ex3_ex3_struct",
                structConstr = HsName
                  "@NsConstr"
                  "Ex3_ex3_struct",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex3_ex3_struct_ex3_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:13:13",
                        structFieldName = NamePair {
                          nameC = CName "ex3_a",
                          nameHsIdent = HsIdentifier
                            "ex3_ex3_struct_ex3_a"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex3_ex3_struct_ex3_b",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:14:14",
                        structFieldName = NamePair {
                          nameC = CName "ex3_b",
                          nameHsIdent = HsIdentifier
                            "ex3_ex3_struct_ex3_b"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "nested_types.h:12:5",
                      declId = NamePair {
                        nameC = CName "ex3_ex3_struct",
                        nameHsIdent = HsIdentifier
                          "Ex3_ex3_struct"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Ex3_ex3_struct"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "nested_types.h:13:13",
                            structFieldName = NamePair {
                              nameC = CName "ex3_a",
                              nameHsIdent = HsIdentifier
                                "ex3_ex3_struct_ex3_a"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "nested_types.h:14:14",
                            structFieldName = NamePair {
                              nameC = CName "ex3_b",
                              nameHsIdent = HsIdentifier
                                "ex3_ex3_struct_ex3_b"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
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
      "Ex3_ex3_struct"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Ex3_ex3_struct"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Ex3",
      structConstr = HsName
        "@NsConstr"
        "Ex3",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "ex3_ex3_struct",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Ex3_ex3_struct"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "nested_types.h:15:7",
              structFieldName = NamePair {
                nameC = CName "ex3_struct",
                nameHsIdent = HsIdentifier
                  "ex3_ex3_struct"},
              structFieldType = TypeStruct
                NamePair {
                  nameC = CName "ex3_ex3_struct",
                  nameHsIdent = HsIdentifier
                    "Ex3_ex3_struct"},
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "ex3_ex3_c",
          fieldType = HsPrimType
            HsPrimCFloat,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "nested_types.h:16:11",
              structFieldName = NamePair {
                nameC = CName "ex3_c",
                nameHsIdent = HsIdentifier
                  "ex3_ex3_c"},
              structFieldType = TypePrim
                (PrimFloating PrimFloat),
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "nested_types.h:11:8",
            declId = NamePair {
              nameC = CName "ex3",
              nameHsIdent = HsIdentifier
                "Ex3"}},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Ex3"),
              structSizeof = 12,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "nested_types.h:15:7",
                  structFieldName = NamePair {
                    nameC = CName "ex3_struct",
                    nameHsIdent = HsIdentifier
                      "ex3_ex3_struct"},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = CName "ex3_ex3_struct",
                      nameHsIdent = HsIdentifier
                        "Ex3_ex3_struct"},
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "nested_types.h:16:11",
                  structFieldName = NamePair {
                    nameC = CName "ex3_c",
                    nameHsIdent = HsIdentifier
                      "ex3_ex3_c"},
                  structFieldType = TypePrim
                    (PrimFloating PrimFloat),
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
          "Ex3",
        structConstr = HsName
          "@NsConstr"
          "Ex3",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "ex3_ex3_struct",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Ex3_ex3_struct"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "nested_types.h:15:7",
                structFieldName = NamePair {
                  nameC = CName "ex3_struct",
                  nameHsIdent = HsIdentifier
                    "ex3_ex3_struct"},
                structFieldType = TypeStruct
                  NamePair {
                    nameC = CName "ex3_ex3_struct",
                    nameHsIdent = HsIdentifier
                      "Ex3_ex3_struct"},
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "ex3_ex3_c",
            fieldType = HsPrimType
              HsPrimCFloat,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "nested_types.h:16:11",
                structFieldName = NamePair {
                  nameC = CName "ex3_c",
                  nameHsIdent = HsIdentifier
                    "ex3_ex3_c"},
                structFieldType = TypePrim
                  (PrimFloating PrimFloat),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "nested_types.h:11:8",
              declId = NamePair {
                nameC = CName "ex3",
                nameHsIdent = HsIdentifier
                  "Ex3"}},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Ex3"),
                structSizeof = 12,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "nested_types.h:15:7",
                    structFieldName = NamePair {
                      nameC = CName "ex3_struct",
                      nameHsIdent = HsIdentifier
                        "ex3_ex3_struct"},
                    structFieldType = TypeStruct
                      NamePair {
                        nameC = CName "ex3_ex3_struct",
                        nameHsIdent = HsIdentifier
                          "Ex3_ex3_struct"},
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "nested_types.h:16:11",
                    structFieldName = NamePair {
                      nameC = CName "ex3_c",
                      nameHsIdent = HsIdentifier
                        "ex3_ex3_c"},
                    structFieldType = TypePrim
                      (PrimFloating PrimFloat),
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
        storableSizeOf = 12,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Ex3",
                structConstr = HsName
                  "@NsConstr"
                  "Ex3",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex3_ex3_struct",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Ex3_ex3_struct"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:15:7",
                        structFieldName = NamePair {
                          nameC = CName "ex3_struct",
                          nameHsIdent = HsIdentifier
                            "ex3_ex3_struct"},
                        structFieldType = TypeStruct
                          NamePair {
                            nameC = CName "ex3_ex3_struct",
                            nameHsIdent = HsIdentifier
                              "Ex3_ex3_struct"},
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex3_ex3_c",
                    fieldType = HsPrimType
                      HsPrimCFloat,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:16:11",
                        structFieldName = NamePair {
                          nameC = CName "ex3_c",
                          nameHsIdent = HsIdentifier
                            "ex3_ex3_c"},
                        structFieldType = TypePrim
                          (PrimFloating PrimFloat),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "nested_types.h:11:8",
                      declId = NamePair {
                        nameC = CName "ex3",
                        nameHsIdent = HsIdentifier
                          "Ex3"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Ex3"),
                        structSizeof = 12,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "nested_types.h:15:7",
                            structFieldName = NamePair {
                              nameC = CName "ex3_struct",
                              nameHsIdent = HsIdentifier
                                "ex3_ex3_struct"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = CName "ex3_ex3_struct",
                                nameHsIdent = HsIdentifier
                                  "Ex3_ex3_struct"},
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "nested_types.h:16:11",
                            structFieldName = NamePair {
                              nameC = CName "ex3_c",
                              nameHsIdent = HsIdentifier
                                "ex3_ex3_c"},
                            structFieldType = TypePrim
                              (PrimFloating PrimFloat),
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
                  "Ex3",
                structConstr = HsName
                  "@NsConstr"
                  "Ex3",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex3_ex3_struct",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Ex3_ex3_struct"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:15:7",
                        structFieldName = NamePair {
                          nameC = CName "ex3_struct",
                          nameHsIdent = HsIdentifier
                            "ex3_ex3_struct"},
                        structFieldType = TypeStruct
                          NamePair {
                            nameC = CName "ex3_ex3_struct",
                            nameHsIdent = HsIdentifier
                              "Ex3_ex3_struct"},
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex3_ex3_c",
                    fieldType = HsPrimType
                      HsPrimCFloat,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:16:11",
                        structFieldName = NamePair {
                          nameC = CName "ex3_c",
                          nameHsIdent = HsIdentifier
                            "ex3_ex3_c"},
                        structFieldType = TypePrim
                          (PrimFloating PrimFloat),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "nested_types.h:11:8",
                      declId = NamePair {
                        nameC = CName "ex3",
                        nameHsIdent = HsIdentifier
                          "Ex3"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Ex3"),
                        structSizeof = 12,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "nested_types.h:15:7",
                            structFieldName = NamePair {
                              nameC = CName "ex3_struct",
                              nameHsIdent = HsIdentifier
                                "ex3_ex3_struct"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = CName "ex3_ex3_struct",
                                nameHsIdent = HsIdentifier
                                  "Ex3_ex3_struct"},
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "nested_types.h:16:11",
                            structFieldName = NamePair {
                              nameC = CName "ex3_c",
                              nameHsIdent = HsIdentifier
                                "ex3_ex3_c"},
                            structFieldType = TypePrim
                              (PrimFloating PrimFloat),
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
    (HsName "@NsTypeConstr" "Ex3"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Ex3"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Ex4_even",
      structConstr = HsName
        "@NsConstr"
        "Ex4_even",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "ex4_even_value",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "nested_types.h:25:16",
              structFieldName = NamePair {
                nameC = CName "value",
                nameHsIdent = HsIdentifier
                  "ex4_even_value"},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "ex4_even_next",
          fieldType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Ex4_odd")),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "nested_types.h:26:25",
              structFieldName = NamePair {
                nameC = CName "next",
                nameHsIdent = HsIdentifier
                  "ex4_even_next"},
              structFieldType = TypePointer
                (TypeStruct
                  NamePair {
                    nameC = CName "ex4_odd",
                    nameHsIdent = HsIdentifier
                      "Ex4_odd"}),
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "nested_types.h:24:12",
            declId = NamePair {
              nameC = CName "ex4_even",
              nameHsIdent = HsIdentifier
                "Ex4_even"}},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Ex4_even"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "nested_types.h:25:16",
                  structFieldName = NamePair {
                    nameC = CName "value",
                    nameHsIdent = HsIdentifier
                      "ex4_even_value"},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "nested_types.h:26:25",
                  structFieldName = NamePair {
                    nameC = CName "next",
                    nameHsIdent = HsIdentifier
                      "ex4_even_next"},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = CName "ex4_odd",
                        nameHsIdent = HsIdentifier
                          "Ex4_odd"}),
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
          "Ex4_even",
        structConstr = HsName
          "@NsConstr"
          "Ex4_even",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "ex4_even_value",
            fieldType = HsPrimType
              HsPrimCDouble,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "nested_types.h:25:16",
                structFieldName = NamePair {
                  nameC = CName "value",
                  nameHsIdent = HsIdentifier
                    "ex4_even_value"},
                structFieldType = TypePrim
                  (PrimFloating PrimDouble),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "ex4_even_next",
            fieldType = HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Ex4_odd")),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "nested_types.h:26:25",
                structFieldName = NamePair {
                  nameC = CName "next",
                  nameHsIdent = HsIdentifier
                    "ex4_even_next"},
                structFieldType = TypePointer
                  (TypeStruct
                    NamePair {
                      nameC = CName "ex4_odd",
                      nameHsIdent = HsIdentifier
                        "Ex4_odd"}),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "nested_types.h:24:12",
              declId = NamePair {
                nameC = CName "ex4_even",
                nameHsIdent = HsIdentifier
                  "Ex4_even"}},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Ex4_even"),
                structSizeof = 16,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "nested_types.h:25:16",
                    structFieldName = NamePair {
                      nameC = CName "value",
                      nameHsIdent = HsIdentifier
                        "ex4_even_value"},
                    structFieldType = TypePrim
                      (PrimFloating PrimDouble),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "nested_types.h:26:25",
                    structFieldName = NamePair {
                      nameC = CName "next",
                      nameHsIdent = HsIdentifier
                        "ex4_even_next"},
                    structFieldType = TypePointer
                      (TypeStruct
                        NamePair {
                          nameC = CName "ex4_odd",
                          nameHsIdent = HsIdentifier
                            "Ex4_odd"}),
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
                  "Ex4_even",
                structConstr = HsName
                  "@NsConstr"
                  "Ex4_even",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_even_value",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:25:16",
                        structFieldName = NamePair {
                          nameC = CName "value",
                          nameHsIdent = HsIdentifier
                            "ex4_even_value"},
                        structFieldType = TypePrim
                          (PrimFloating PrimDouble),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_even_next",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Ex4_odd")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:26:25",
                        structFieldName = NamePair {
                          nameC = CName "next",
                          nameHsIdent = HsIdentifier
                            "ex4_even_next"},
                        structFieldType = TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = CName "ex4_odd",
                              nameHsIdent = HsIdentifier
                                "Ex4_odd"}),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "nested_types.h:24:12",
                      declId = NamePair {
                        nameC = CName "ex4_even",
                        nameHsIdent = HsIdentifier
                          "Ex4_even"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Ex4_even"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "nested_types.h:25:16",
                            structFieldName = NamePair {
                              nameC = CName "value",
                              nameHsIdent = HsIdentifier
                                "ex4_even_value"},
                            structFieldType = TypePrim
                              (PrimFloating PrimDouble),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "nested_types.h:26:25",
                            structFieldName = NamePair {
                              nameC = CName "next",
                              nameHsIdent = HsIdentifier
                                "ex4_even_next"},
                            structFieldType = TypePointer
                              (TypeStruct
                                NamePair {
                                  nameC = CName "ex4_odd",
                                  nameHsIdent = HsIdentifier
                                    "Ex4_odd"}),
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
                  "Ex4_even",
                structConstr = HsName
                  "@NsConstr"
                  "Ex4_even",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_even_value",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:25:16",
                        structFieldName = NamePair {
                          nameC = CName "value",
                          nameHsIdent = HsIdentifier
                            "ex4_even_value"},
                        structFieldType = TypePrim
                          (PrimFloating PrimDouble),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_even_next",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Ex4_odd")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:26:25",
                        structFieldName = NamePair {
                          nameC = CName "next",
                          nameHsIdent = HsIdentifier
                            "ex4_even_next"},
                        structFieldType = TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = CName "ex4_odd",
                              nameHsIdent = HsIdentifier
                                "Ex4_odd"}),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "nested_types.h:24:12",
                      declId = NamePair {
                        nameC = CName "ex4_even",
                        nameHsIdent = HsIdentifier
                          "Ex4_even"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Ex4_even"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "nested_types.h:25:16",
                            structFieldName = NamePair {
                              nameC = CName "value",
                              nameHsIdent = HsIdentifier
                                "ex4_even_value"},
                            structFieldType = TypePrim
                              (PrimFloating PrimDouble),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "nested_types.h:26:25",
                            structFieldName = NamePair {
                              nameC = CName "next",
                              nameHsIdent = HsIdentifier
                                "ex4_even_next"},
                            structFieldType = TypePointer
                              (TypeStruct
                                NamePair {
                                  nameC = CName "ex4_odd",
                                  nameHsIdent = HsIdentifier
                                    "Ex4_odd"}),
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
      "Ex4_even"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Ex4_even"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Ex4_odd",
      structConstr = HsName
        "@NsConstr"
        "Ex4_odd",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "ex4_odd_value",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "nested_types.h:23:9",
              structFieldName = NamePair {
                nameC = CName "value",
                nameHsIdent = HsIdentifier
                  "ex4_odd_value"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "ex4_odd_next",
          fieldType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Ex4_even")),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "nested_types.h:27:8",
              structFieldName = NamePair {
                nameC = CName "next",
                nameHsIdent = HsIdentifier
                  "ex4_odd_next"},
              structFieldType = TypePointer
                (TypeStruct
                  NamePair {
                    nameC = CName "ex4_even",
                    nameHsIdent = HsIdentifier
                      "Ex4_even"}),
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "nested_types.h:22:8",
            declId = NamePair {
              nameC = CName "ex4_odd",
              nameHsIdent = HsIdentifier
                "Ex4_odd"}},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Ex4_odd"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "nested_types.h:23:9",
                  structFieldName = NamePair {
                    nameC = CName "value",
                    nameHsIdent = HsIdentifier
                      "ex4_odd_value"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "nested_types.h:27:8",
                  structFieldName = NamePair {
                    nameC = CName "next",
                    nameHsIdent = HsIdentifier
                      "ex4_odd_next"},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = CName "ex4_even",
                        nameHsIdent = HsIdentifier
                          "Ex4_even"}),
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
          "Ex4_odd",
        structConstr = HsName
          "@NsConstr"
          "Ex4_odd",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "ex4_odd_value",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "nested_types.h:23:9",
                structFieldName = NamePair {
                  nameC = CName "value",
                  nameHsIdent = HsIdentifier
                    "ex4_odd_value"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "ex4_odd_next",
            fieldType = HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Ex4_even")),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "nested_types.h:27:8",
                structFieldName = NamePair {
                  nameC = CName "next",
                  nameHsIdent = HsIdentifier
                    "ex4_odd_next"},
                structFieldType = TypePointer
                  (TypeStruct
                    NamePair {
                      nameC = CName "ex4_even",
                      nameHsIdent = HsIdentifier
                        "Ex4_even"}),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "nested_types.h:22:8",
              declId = NamePair {
                nameC = CName "ex4_odd",
                nameHsIdent = HsIdentifier
                  "Ex4_odd"}},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Ex4_odd"),
                structSizeof = 16,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "nested_types.h:23:9",
                    structFieldName = NamePair {
                      nameC = CName "value",
                      nameHsIdent = HsIdentifier
                        "ex4_odd_value"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "nested_types.h:27:8",
                    structFieldName = NamePair {
                      nameC = CName "next",
                      nameHsIdent = HsIdentifier
                        "ex4_odd_next"},
                    structFieldType = TypePointer
                      (TypeStruct
                        NamePair {
                          nameC = CName "ex4_even",
                          nameHsIdent = HsIdentifier
                            "Ex4_even"}),
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
                  "Ex4_odd",
                structConstr = HsName
                  "@NsConstr"
                  "Ex4_odd",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_odd_value",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:23:9",
                        structFieldName = NamePair {
                          nameC = CName "value",
                          nameHsIdent = HsIdentifier
                            "ex4_odd_value"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_odd_next",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Ex4_even")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:27:8",
                        structFieldName = NamePair {
                          nameC = CName "next",
                          nameHsIdent = HsIdentifier
                            "ex4_odd_next"},
                        structFieldType = TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = CName "ex4_even",
                              nameHsIdent = HsIdentifier
                                "Ex4_even"}),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "nested_types.h:22:8",
                      declId = NamePair {
                        nameC = CName "ex4_odd",
                        nameHsIdent = HsIdentifier
                          "Ex4_odd"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Ex4_odd"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "nested_types.h:23:9",
                            structFieldName = NamePair {
                              nameC = CName "value",
                              nameHsIdent = HsIdentifier
                                "ex4_odd_value"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "nested_types.h:27:8",
                            structFieldName = NamePair {
                              nameC = CName "next",
                              nameHsIdent = HsIdentifier
                                "ex4_odd_next"},
                            structFieldType = TypePointer
                              (TypeStruct
                                NamePair {
                                  nameC = CName "ex4_even",
                                  nameHsIdent = HsIdentifier
                                    "Ex4_even"}),
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
                  "Ex4_odd",
                structConstr = HsName
                  "@NsConstr"
                  "Ex4_odd",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_odd_value",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:23:9",
                        structFieldName = NamePair {
                          nameC = CName "value",
                          nameHsIdent = HsIdentifier
                            "ex4_odd_value"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_odd_next",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Ex4_even")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_types.h:27:8",
                        structFieldName = NamePair {
                          nameC = CName "next",
                          nameHsIdent = HsIdentifier
                            "ex4_odd_next"},
                        structFieldType = TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = CName "ex4_even",
                              nameHsIdent = HsIdentifier
                                "Ex4_even"}),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "nested_types.h:22:8",
                      declId = NamePair {
                        nameC = CName "ex4_odd",
                        nameHsIdent = HsIdentifier
                          "Ex4_odd"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Ex4_odd"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "nested_types.h:23:9",
                            structFieldName = NamePair {
                              nameC = CName "value",
                              nameHsIdent = HsIdentifier
                                "ex4_odd_value"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "nested_types.h:27:8",
                            structFieldName = NamePair {
                              nameC = CName "next",
                              nameHsIdent = HsIdentifier
                                "ex4_odd_next"},
                            structFieldType = TypePointer
                              (TypeStruct
                                NamePair {
                                  nameC = CName "ex4_even",
                                  nameHsIdent = HsIdentifier
                                    "Ex4_even"}),
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
      "Ex4_odd"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Ex4_odd")]
