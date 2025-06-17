[
  DeclEmpty
    EmptyData {
      emptyDataName = HsName
        "@NsTypeConstr"
        "Foo",
      emptyDataOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "opaque_declaration.h:1:8",
          declId = NamePair {
            nameC = CName "foo",
            nameHsIdent = HsIdentifier
              "Foo"},
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
        "Bar",
      structConstr = HsName
        "@NsConstr"
        "Bar",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "bar_ptrA",
          fieldType = HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "Foo")),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "opaque_declaration.h:5:17",
              structFieldName = NamePair {
                nameC = CName "ptrA",
                nameHsIdent = HsIdentifier
                  "bar_ptrA"},
              structFieldType = TypePointer
                (TypeStruct
                  NamePair {
                    nameC = CName "foo",
                    nameHsIdent = HsIdentifier
                      "Foo"}
                  NameOriginInSource),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "bar_ptrB",
          fieldType = HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "Bar")),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "opaque_declaration.h:6:17",
              structFieldName = NamePair {
                nameC = CName "ptrB",
                nameHsIdent = HsIdentifier
                  "bar_ptrB"},
              structFieldType = TypePointer
                (TypeStruct
                  NamePair {
                    nameC = CName "bar",
                    nameHsIdent = HsIdentifier
                      "Bar"}
                  NameOriginInSource),
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "opaque_declaration.h:4:8",
            declId = NamePair {
              nameC = CName "bar",
              nameHsIdent = HsIdentifier
                "Bar"},
            declOrigin = NameOriginInSource,
            declAliases = []},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Bar"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "opaque_declaration.h:5:17",
                  structFieldName = NamePair {
                    nameC = CName "ptrA",
                    nameHsIdent = HsIdentifier
                      "bar_ptrA"},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = CName "foo",
                        nameHsIdent = HsIdentifier
                          "Foo"}
                      NameOriginInSource),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "opaque_declaration.h:6:17",
                  structFieldName = NamePair {
                    nameC = CName "ptrB",
                    nameHsIdent = HsIdentifier
                      "bar_ptrB"},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = CName "bar",
                        nameHsIdent = HsIdentifier
                          "Bar"}
                      NameOriginInSource),
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
              "bar_ptrA",
            fieldType = HsPtr
              (HsTypRef
                (HsName "@NsTypeConstr" "Foo")),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "opaque_declaration.h:5:17",
                structFieldName = NamePair {
                  nameC = CName "ptrA",
                  nameHsIdent = HsIdentifier
                    "bar_ptrA"},
                structFieldType = TypePointer
                  (TypeStruct
                    NamePair {
                      nameC = CName "foo",
                      nameHsIdent = HsIdentifier
                        "Foo"}
                    NameOriginInSource),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "bar_ptrB",
            fieldType = HsPtr
              (HsTypRef
                (HsName "@NsTypeConstr" "Bar")),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "opaque_declaration.h:6:17",
                structFieldName = NamePair {
                  nameC = CName "ptrB",
                  nameHsIdent = HsIdentifier
                    "bar_ptrB"},
                structFieldType = TypePointer
                  (TypeStruct
                    NamePair {
                      nameC = CName "bar",
                      nameHsIdent = HsIdentifier
                        "Bar"}
                    NameOriginInSource),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "opaque_declaration.h:4:8",
              declId = NamePair {
                nameC = CName "bar",
                nameHsIdent = HsIdentifier
                  "Bar"},
              declOrigin = NameOriginInSource,
              declAliases = []},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Bar"),
                structSizeof = 16,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "opaque_declaration.h:5:17",
                    structFieldName = NamePair {
                      nameC = CName "ptrA",
                      nameHsIdent = HsIdentifier
                        "bar_ptrA"},
                    structFieldType = TypePointer
                      (TypeStruct
                        NamePair {
                          nameC = CName "foo",
                          nameHsIdent = HsIdentifier
                            "Foo"}
                        NameOriginInSource),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "opaque_declaration.h:6:17",
                    structFieldName = NamePair {
                      nameC = CName "ptrB",
                      nameHsIdent = HsIdentifier
                        "bar_ptrB"},
                    structFieldType = TypePointer
                      (TypeStruct
                        NamePair {
                          nameC = CName "bar",
                          nameHsIdent = HsIdentifier
                            "Bar"}
                        NameOriginInSource),
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
                  "Bar",
                structConstr = HsName
                  "@NsConstr"
                  "Bar",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bar_ptrA",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName "@NsTypeConstr" "Foo")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "opaque_declaration.h:5:17",
                        structFieldName = NamePair {
                          nameC = CName "ptrA",
                          nameHsIdent = HsIdentifier
                            "bar_ptrA"},
                        structFieldType = TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = CName "foo",
                              nameHsIdent = HsIdentifier
                                "Foo"}
                            NameOriginInSource),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bar_ptrB",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName "@NsTypeConstr" "Bar")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "opaque_declaration.h:6:17",
                        structFieldName = NamePair {
                          nameC = CName "ptrB",
                          nameHsIdent = HsIdentifier
                            "bar_ptrB"},
                        structFieldType = TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = CName "bar",
                              nameHsIdent = HsIdentifier
                                "Bar"}
                            NameOriginInSource),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "opaque_declaration.h:4:8",
                      declId = NamePair {
                        nameC = CName "bar",
                        nameHsIdent = HsIdentifier
                          "Bar"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Bar"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "opaque_declaration.h:5:17",
                            structFieldName = NamePair {
                              nameC = CName "ptrA",
                              nameHsIdent = HsIdentifier
                                "bar_ptrA"},
                            structFieldType = TypePointer
                              (TypeStruct
                                NamePair {
                                  nameC = CName "foo",
                                  nameHsIdent = HsIdentifier
                                    "Foo"}
                                NameOriginInSource),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "opaque_declaration.h:6:17",
                            structFieldName = NamePair {
                              nameC = CName "ptrB",
                              nameHsIdent = HsIdentifier
                                "bar_ptrB"},
                            structFieldType = TypePointer
                              (TypeStruct
                                NamePair {
                                  nameC = CName "bar",
                                  nameHsIdent = HsIdentifier
                                    "Bar"}
                                NameOriginInSource),
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
                      "bar_ptrA",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName "@NsTypeConstr" "Foo")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "opaque_declaration.h:5:17",
                        structFieldName = NamePair {
                          nameC = CName "ptrA",
                          nameHsIdent = HsIdentifier
                            "bar_ptrA"},
                        structFieldType = TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = CName "foo",
                              nameHsIdent = HsIdentifier
                                "Foo"}
                            NameOriginInSource),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bar_ptrB",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName "@NsTypeConstr" "Bar")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "opaque_declaration.h:6:17",
                        structFieldName = NamePair {
                          nameC = CName "ptrB",
                          nameHsIdent = HsIdentifier
                            "bar_ptrB"},
                        structFieldType = TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = CName "bar",
                              nameHsIdent = HsIdentifier
                                "Bar"}
                            NameOriginInSource),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "opaque_declaration.h:4:8",
                      declId = NamePair {
                        nameC = CName "bar",
                        nameHsIdent = HsIdentifier
                          "Bar"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Bar"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "opaque_declaration.h:5:17",
                            structFieldName = NamePair {
                              nameC = CName "ptrA",
                              nameHsIdent = HsIdentifier
                                "bar_ptrA"},
                            structFieldType = TypePointer
                              (TypeStruct
                                NamePair {
                                  nameC = CName "foo",
                                  nameHsIdent = HsIdentifier
                                    "Foo"}
                                NameOriginInSource),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "opaque_declaration.h:6:17",
                            structFieldName = NamePair {
                              nameC = CName "ptrB",
                              nameHsIdent = HsIdentifier
                                "bar_ptrB"},
                            structFieldType = TypePointer
                              (TypeStruct
                                NamePair {
                                  nameC = CName "bar",
                                  nameHsIdent = HsIdentifier
                                    "Bar"}
                                NameOriginInSource),
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
        "Baz",
      structConstr = HsName
        "@NsConstr"
        "Baz",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "opaque_declaration.h:9:8",
            declId = NamePair {
              nameC = CName "baz",
              nameHsIdent = HsIdentifier
                "Baz"},
            declOrigin = NameOriginInSource,
            declAliases = []},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Baz"),
              structSizeof = 0,
              structAlignment = 1,
              structFields = [],
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
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "opaque_declaration.h:9:8",
              declId = NamePair {
                nameC = CName "baz",
                nameHsIdent = HsIdentifier
                  "Baz"},
              declOrigin = NameOriginInSource,
              declAliases = []},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Baz"),
                structSizeof = 0,
                structAlignment = 1,
                structFields = [],
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
        storableSizeOf = 0,
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
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "opaque_declaration.h:9:8",
                      declId = NamePair {
                        nameC = CName "baz",
                        nameHsIdent = HsIdentifier
                          "Baz"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Baz"),
                        structSizeof = 0,
                        structAlignment = 1,
                        structFields = [],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]})
            []),
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
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "opaque_declaration.h:9:8",
                      declId = NamePair {
                        nameC = CName "baz",
                        nameHsIdent = HsIdentifier
                          "Baz"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Baz"),
                        structSizeof = 0,
                        structAlignment = 1,
                        structFields = [],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]}
              (Add 0)
              (Seq [])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Baz"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Baz"),
  DeclEmpty
    EmptyData {
      emptyDataName = HsName
        "@NsTypeConstr"
        "Quu",
      emptyDataOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "opaque_declaration.h:11:6",
          declId = NamePair {
            nameC = CName "quu",
            nameHsIdent = HsIdentifier
              "Quu"},
          declOrigin = NameOriginInSource,
          declAliases = []},
        declKind = OpaqueEnum,
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}}},
  DeclEmpty
    EmptyData {
      emptyDataName = HsName
        "@NsTypeConstr"
        "Opaque_union",
      emptyDataOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "opaque_declaration.h:13:7",
          declId = NamePair {
            nameC = CName "opaque_union",
            nameHsIdent = HsIdentifier
              "Opaque_union"},
          declOrigin = NameOriginInSource,
          declAliases = []},
        declKind = OpaqueUnion,
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}}}]
