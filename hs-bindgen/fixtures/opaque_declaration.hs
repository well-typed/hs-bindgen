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
            nameC = Name "foo",
            nameHsIdent = HsIdentifier
              "Foo"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "opaque_declaration.h",
          declComment = Nothing},
        declKind = OpaqueStruct,
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      emptyDataComment = Nothing},
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
                nameC = Name "ptrA",
                nameHsIdent = HsIdentifier
                  "bar_ptrA"},
              structFieldType = TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "foo",
                    nameHsIdent = HsIdentifier
                      "Foo"}
                  NameOriginInSource),
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing},
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
                nameC = Name "ptrB",
                nameHsIdent = HsIdentifier
                  "bar_ptrB"},
              structFieldType = TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "bar",
                    nameHsIdent = HsIdentifier
                      "Bar"}
                  NameOriginInSource),
              structFieldOffset = 64,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "opaque_declaration.h:4:8",
            declId = NamePair {
              nameC = Name "bar",
              nameHsIdent = HsIdentifier
                "Bar"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "opaque_declaration.h",
            declComment = Nothing},
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
                    nameC = Name "ptrA",
                    nameHsIdent = HsIdentifier
                      "bar_ptrA"},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "foo",
                        nameHsIdent = HsIdentifier
                          "Foo"}
                      NameOriginInSource),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
                StructField {
                  structFieldLoc =
                  "opaque_declaration.h:6:17",
                  structFieldName = NamePair {
                    nameC = Name "ptrB",
                    nameHsIdent = HsIdentifier
                      "bar_ptrB"},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "bar",
                        nameHsIdent = HsIdentifier
                          "Bar"}
                      NameOriginInSource),
                  structFieldOffset = 64,
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
                    nameC = Name "ptrA",
                    nameHsIdent = HsIdentifier
                      "bar_ptrA"},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "foo",
                        nameHsIdent = HsIdentifier
                          "Foo"}
                      NameOriginInSource),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing},
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
                    nameC = Name "ptrB",
                    nameHsIdent = HsIdentifier
                      "bar_ptrB"},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "bar",
                        nameHsIdent = HsIdentifier
                          "Bar"}
                      NameOriginInSource),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "opaque_declaration.h:4:8",
                declId = NamePair {
                  nameC = Name "bar",
                  nameHsIdent = HsIdentifier
                    "Bar"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader =
                "opaque_declaration.h",
                declComment = Nothing},
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
                        nameC = Name "ptrA",
                        nameHsIdent = HsIdentifier
                          "bar_ptrA"},
                      structFieldType = TypePointer
                        (TypeStruct
                          NamePair {
                            nameC = Name "foo",
                            nameHsIdent = HsIdentifier
                              "Foo"}
                          NameOriginInSource),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing},
                    StructField {
                      structFieldLoc =
                      "opaque_declaration.h:6:17",
                      structFieldName = NamePair {
                        nameC = Name "ptrB",
                        nameHsIdent = HsIdentifier
                          "bar_ptrB"},
                      structFieldType = TypePointer
                        (TypeStruct
                          NamePair {
                            nameC = Name "bar",
                            nameHsIdent = HsIdentifier
                              "Bar"}
                          NameOriginInSource),
                      structFieldOffset = 64,
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
                            nameC = Name "ptrA",
                            nameHsIdent = HsIdentifier
                              "bar_ptrA"},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "foo",
                                nameHsIdent = HsIdentifier
                                  "Foo"}
                              NameOriginInSource),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
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
                            nameC = Name "ptrB",
                            nameHsIdent = HsIdentifier
                              "bar_ptrB"},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "bar",
                                nameHsIdent = HsIdentifier
                                  "Bar"}
                              NameOriginInSource),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "opaque_declaration.h:4:8",
                        declId = NamePair {
                          nameC = Name "bar",
                          nameHsIdent = HsIdentifier
                            "Bar"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "opaque_declaration.h",
                        declComment = Nothing},
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
                                nameC = Name "ptrA",
                                nameHsIdent = HsIdentifier
                                  "bar_ptrA"},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "foo",
                                    nameHsIdent = HsIdentifier
                                      "Foo"}
                                  NameOriginInSource),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc =
                              "opaque_declaration.h:6:17",
                              structFieldName = NamePair {
                                nameC = Name "ptrB",
                                nameHsIdent = HsIdentifier
                                  "bar_ptrB"},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "bar",
                                    nameHsIdent = HsIdentifier
                                      "Bar"}
                                  NameOriginInSource),
                              structFieldOffset = 64,
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
                            nameC = Name "ptrA",
                            nameHsIdent = HsIdentifier
                              "bar_ptrA"},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "foo",
                                nameHsIdent = HsIdentifier
                                  "Foo"}
                              NameOriginInSource),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
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
                            nameC = Name "ptrB",
                            nameHsIdent = HsIdentifier
                              "bar_ptrB"},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "bar",
                                nameHsIdent = HsIdentifier
                                  "Bar"}
                              NameOriginInSource),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "opaque_declaration.h:4:8",
                        declId = NamePair {
                          nameC = Name "bar",
                          nameHsIdent = HsIdentifier
                            "Bar"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "opaque_declaration.h",
                        declComment = Nothing},
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
                                nameC = Name "ptrA",
                                nameHsIdent = HsIdentifier
                                  "bar_ptrA"},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "foo",
                                    nameHsIdent = HsIdentifier
                                      "Foo"}
                                  NameOriginInSource),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc =
                              "opaque_declaration.h:6:17",
                              structFieldName = NamePair {
                                nameC = Name "ptrB",
                                nameHsIdent = HsIdentifier
                                  "bar_ptrB"},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "bar",
                                    nameHsIdent = HsIdentifier
                                      "Bar"}
                                  NameOriginInSource),
                              structFieldOffset = 64,
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
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      8
                      (Idx 1)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Bar",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Bar",
      deriveInstanceComment =
      Nothing},
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
              nameC = Name "baz",
              nameHsIdent = HsIdentifier
                "Baz"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "opaque_declaration.h",
            declComment = Nothing},
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
        [Eq, Show, Storable],
      structComment = Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
                  nameC = Name "baz",
                  nameHsIdent = HsIdentifier
                    "Baz"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader =
                "opaque_declaration.h",
                declComment = Nothing},
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
            [Eq, Show, Storable],
          structComment = Nothing}
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
                          nameC = Name "baz",
                          nameHsIdent = HsIdentifier
                            "Baz"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "opaque_declaration.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Nothing})
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
                          nameC = Name "baz",
                          nameHsIdent = HsIdentifier
                            "Baz"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "opaque_declaration.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Nothing}
                (Add 0)
                (Seq [])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Baz",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Baz",
      deriveInstanceComment =
      Nothing},
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
            nameC = Name "quu",
            nameHsIdent = HsIdentifier
              "Quu"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "opaque_declaration.h",
          declComment = Nothing},
        declKind = OpaqueEnum,
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      emptyDataComment = Nothing},
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
            nameC = Name "opaque_union",
            nameHsIdent = HsIdentifier
              "Opaque_union"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "opaque_declaration.h",
          declComment = Nothing},
        declKind = OpaqueUnion,
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      emptyDataComment = Nothing}]
