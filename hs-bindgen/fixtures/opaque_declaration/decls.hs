[
  DeclEmpty
    EmptyData {
      emptyDataName = Name
        "@NsTypeConstr"
        "Foo",
      emptyDataOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "opaque_declaration.h:1:8",
          declId = NamePair {
            nameC = Name "foo",
            nameHsIdent = Identifier "Foo"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["opaque_declaration.h"],
              headerInclude =
              "opaque_declaration.h"},
          declComment = Nothing},
        declKind = Opaque
          (NameKindTagged TagKindStruct),
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      emptyDataComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "foo",
          commentLocation = Just
            "opaque_declaration.h:1:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["opaque_declaration.h"],
              headerInclude =
              "opaque_declaration.h"},
          commentChildren = []}},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Bar",
      structConstr = Name
        "@NsConstr"
        "Bar",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "bar_ptrA",
          fieldType = HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "Foo")),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "opaque_declaration.h:5:17",
                fieldName = NamePair {
                  nameC = Name "ptrA",
                  nameHsIdent = Identifier
                    "bar_ptrA"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "foo",
                    nameHsIdent = Identifier "Foo"}
                  NameOriginInSource),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "ptrA",
              commentLocation = Just
                "opaque_declaration.h:5:17",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["opaque_declaration.h"],
                  headerInclude =
                  "opaque_declaration.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "bar_ptrB",
          fieldType = HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "Bar")),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "opaque_declaration.h:6:17",
                fieldName = NamePair {
                  nameC = Name "ptrB",
                  nameHsIdent = Identifier
                    "bar_ptrB"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "bar",
                    nameHsIdent = Identifier "Bar"}
                  NameOriginInSource),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "ptrB",
              commentLocation = Just
                "opaque_declaration.h:6:17",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["opaque_declaration.h"],
                  headerInclude =
                  "opaque_declaration.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "opaque_declaration.h:4:8",
            declId = NamePair {
              nameC = Name "bar",
              nameHsIdent = Identifier "Bar"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["opaque_declaration.h"],
                headerInclude =
                "opaque_declaration.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Bar"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "opaque_declaration.h:5:17",
                    fieldName = NamePair {
                      nameC = Name "ptrA",
                      nameHsIdent = Identifier
                        "bar_ptrA"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "foo",
                        nameHsIdent = Identifier "Foo"}
                      NameOriginInSource),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "opaque_declaration.h:6:17",
                    fieldName = NamePair {
                      nameC = Name "ptrB",
                      nameHsIdent = Identifier
                        "bar_ptrB"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "bar",
                        nameHsIdent = Identifier "Bar"}
                      NameOriginInSource),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec {
            declSpecC = Nothing,
            declSpecHs = Nothing}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "bar",
          commentLocation = Just
            "opaque_declaration.h:4:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["opaque_declaration.h"],
              headerInclude =
              "opaque_declaration.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Bar",
          structConstr = Name
            "@NsConstr"
            "Bar",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "bar_ptrA",
              fieldType = HsPtr
                (HsTypRef
                  (Name "@NsTypeConstr" "Foo")),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "opaque_declaration.h:5:17",
                    fieldName = NamePair {
                      nameC = Name "ptrA",
                      nameHsIdent = Identifier
                        "bar_ptrA"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "foo",
                        nameHsIdent = Identifier "Foo"}
                      NameOriginInSource),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "ptrA",
                  commentLocation = Just
                    "opaque_declaration.h:5:17",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["opaque_declaration.h"],
                      headerInclude =
                      "opaque_declaration.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "bar_ptrB",
              fieldType = HsPtr
                (HsTypRef
                  (Name "@NsTypeConstr" "Bar")),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "opaque_declaration.h:6:17",
                    fieldName = NamePair {
                      nameC = Name "ptrB",
                      nameHsIdent = Identifier
                        "bar_ptrB"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "bar",
                        nameHsIdent = Identifier "Bar"}
                      NameOriginInSource),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "ptrB",
                  commentLocation = Just
                    "opaque_declaration.h:6:17",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["opaque_declaration.h"],
                      headerInclude =
                      "opaque_declaration.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "opaque_declaration.h:4:8",
                declId = NamePair {
                  nameC = Name "bar",
                  nameHsIdent = Identifier "Bar"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["opaque_declaration.h"],
                    headerInclude =
                    "opaque_declaration.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Bar"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "opaque_declaration.h:5:17",
                        fieldName = NamePair {
                          nameC = Name "ptrA",
                          nameHsIdent = Identifier
                            "bar_ptrA"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeStruct
                          NamePair {
                            nameC = Name "foo",
                            nameHsIdent = Identifier "Foo"}
                          NameOriginInSource),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "opaque_declaration.h:6:17",
                        fieldName = NamePair {
                          nameC = Name "ptrB",
                          nameHsIdent = Identifier
                            "bar_ptrB"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeStruct
                          NamePair {
                            nameC = Name "bar",
                            nameHsIdent = Identifier "Bar"}
                          NameOriginInSource),
                      structFieldOffset = 64,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec {
                declSpecC = Nothing,
                declSpecHs = Nothing}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "bar",
              commentLocation = Just
                "opaque_declaration.h:4:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["opaque_declaration.h"],
                  headerInclude =
                  "opaque_declaration.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 16,
          storableAlignment = 8,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Bar",
                  structConstr = Name
                    "@NsConstr"
                    "Bar",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "bar_ptrA",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name "@NsTypeConstr" "Foo")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "opaque_declaration.h:5:17",
                            fieldName = NamePair {
                              nameC = Name "ptrA",
                              nameHsIdent = Identifier
                                "bar_ptrA"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "foo",
                                nameHsIdent = Identifier "Foo"}
                              NameOriginInSource),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ptrA",
                          commentLocation = Just
                            "opaque_declaration.h:5:17",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["opaque_declaration.h"],
                              headerInclude =
                              "opaque_declaration.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "bar_ptrB",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name "@NsTypeConstr" "Bar")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "opaque_declaration.h:6:17",
                            fieldName = NamePair {
                              nameC = Name "ptrB",
                              nameHsIdent = Identifier
                                "bar_ptrB"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "bar",
                                nameHsIdent = Identifier "Bar"}
                              NameOriginInSource),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ptrB",
                          commentLocation = Just
                            "opaque_declaration.h:6:17",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["opaque_declaration.h"],
                              headerInclude =
                              "opaque_declaration.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "opaque_declaration.h:4:8",
                        declId = NamePair {
                          nameC = Name "bar",
                          nameHsIdent = Identifier "Bar"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["opaque_declaration.h"],
                            headerInclude =
                            "opaque_declaration.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Bar"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "opaque_declaration.h:5:17",
                                fieldName = NamePair {
                                  nameC = Name "ptrA",
                                  nameHsIdent = Identifier
                                    "bar_ptrA"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "foo",
                                    nameHsIdent = Identifier "Foo"}
                                  NameOriginInSource),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "opaque_declaration.h:6:17",
                                fieldName = NamePair {
                                  nameC = Name "ptrB",
                                  nameHsIdent = Identifier
                                    "bar_ptrB"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "bar",
                                    nameHsIdent = Identifier "Bar"}
                                  NameOriginInSource),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "bar",
                      commentLocation = Just
                        "opaque_declaration.h:4:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["opaque_declaration.h"],
                          headerInclude =
                          "opaque_declaration.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "bar_ptrA")
                  (Idx 0),
                PeekCField
                  (HsStrLit "bar_ptrB")
                  (Idx 0)]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Bar",
                  structConstr = Name
                    "@NsConstr"
                    "Bar",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "bar_ptrA",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name "@NsTypeConstr" "Foo")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "opaque_declaration.h:5:17",
                            fieldName = NamePair {
                              nameC = Name "ptrA",
                              nameHsIdent = Identifier
                                "bar_ptrA"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "foo",
                                nameHsIdent = Identifier "Foo"}
                              NameOriginInSource),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ptrA",
                          commentLocation = Just
                            "opaque_declaration.h:5:17",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["opaque_declaration.h"],
                              headerInclude =
                              "opaque_declaration.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "bar_ptrB",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name "@NsTypeConstr" "Bar")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "opaque_declaration.h:6:17",
                            fieldName = NamePair {
                              nameC = Name "ptrB",
                              nameHsIdent = Identifier
                                "bar_ptrB"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "bar",
                                nameHsIdent = Identifier "Bar"}
                              NameOriginInSource),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ptrB",
                          commentLocation = Just
                            "opaque_declaration.h:6:17",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["opaque_declaration.h"],
                              headerInclude =
                              "opaque_declaration.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "opaque_declaration.h:4:8",
                        declId = NamePair {
                          nameC = Name "bar",
                          nameHsIdent = Identifier "Bar"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["opaque_declaration.h"],
                            headerInclude =
                            "opaque_declaration.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Bar"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "opaque_declaration.h:5:17",
                                fieldName = NamePair {
                                  nameC = Name "ptrA",
                                  nameHsIdent = Identifier
                                    "bar_ptrA"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "foo",
                                    nameHsIdent = Identifier "Foo"}
                                  NameOriginInSource),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "opaque_declaration.h:6:17",
                                fieldName = NamePair {
                                  nameC = Name "ptrB",
                                  nameHsIdent = Identifier
                                    "bar_ptrB"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "bar",
                                    nameHsIdent = Identifier "Bar"}
                                  NameOriginInSource),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "bar",
                      commentLocation = Just
                        "opaque_declaration.h:4:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["opaque_declaration.h"],
                          headerInclude =
                          "opaque_declaration.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "bar_ptrA")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "bar_ptrB")
                      (Idx 3)
                      (Idx 1)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Bar",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Bar",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Bar"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "bar_ptrA",
          hasCFieldInstanceCFieldType =
          HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "Foo")),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Bar"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "bar_ptrA",
          hasFieldInstanceFieldType =
          HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "Foo")),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Bar"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "bar_ptrB",
          hasCFieldInstanceCFieldType =
          HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "Bar")),
          hasCFieldInstanceFieldOffset =
          8},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Bar"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "bar_ptrB",
          hasFieldInstanceFieldType =
          HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "Bar")),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Baz",
      structConstr = Name
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
              nameHsIdent = Identifier "Baz"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["opaque_declaration.h"],
                headerInclude =
                "opaque_declaration.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Baz"),
              structSizeof = 0,
              structAlignment = 1,
              structFields = [],
              structFlam = Nothing},
          declSpec = DeclSpec {
            declSpecC = Nothing,
            declSpecHs = Nothing}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "baz",
          commentLocation = Just
            "opaque_declaration.h:9:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["opaque_declaration.h"],
              headerInclude =
              "opaque_declaration.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Baz",
          structConstr = Name
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
                  nameHsIdent = Identifier "Baz"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["opaque_declaration.h"],
                    headerInclude =
                    "opaque_declaration.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Baz"),
                  structSizeof = 0,
                  structAlignment = 1,
                  structFields = [],
                  structFlam = Nothing},
              declSpec = DeclSpec {
                declSpecC = Nothing,
                declSpecHs = Nothing}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "baz",
              commentLocation = Just
                "opaque_declaration.h:9:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["opaque_declaration.h"],
                  headerInclude =
                  "opaque_declaration.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 0,
          storableAlignment = 1,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Baz",
                  structConstr = Name
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
                          nameHsIdent = Identifier "Baz"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["opaque_declaration.h"],
                            headerInclude =
                            "opaque_declaration.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Baz"),
                          structSizeof = 0,
                          structAlignment = 1,
                          structFields = [],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "baz",
                      commentLocation = Just
                        "opaque_declaration.h:9:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["opaque_declaration.h"],
                          headerInclude =
                          "opaque_declaration.h"},
                      commentChildren = []}})
              []),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Baz",
                  structConstr = Name
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
                          nameHsIdent = Identifier "Baz"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["opaque_declaration.h"],
                            headerInclude =
                            "opaque_declaration.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Baz"),
                          structSizeof = 0,
                          structAlignment = 1,
                          structFields = [],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "baz",
                      commentLocation = Just
                        "opaque_declaration.h:9:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["opaque_declaration.h"],
                          headerInclude =
                          "opaque_declaration.h"},
                      commentChildren = []}}
                (Add 0)
                (Seq [])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Baz",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Baz",
      deriveInstanceComment =
      Nothing},
  DeclEmpty
    EmptyData {
      emptyDataName = Name
        "@NsTypeConstr"
        "Quu",
      emptyDataOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "opaque_declaration.h:11:6",
          declId = NamePair {
            nameC = Name "quu",
            nameHsIdent = Identifier "Quu"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["opaque_declaration.h"],
              headerInclude =
              "opaque_declaration.h"},
          declComment = Nothing},
        declKind = Opaque
          (NameKindTagged TagKindEnum),
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      emptyDataComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "quu",
          commentLocation = Just
            "opaque_declaration.h:11:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["opaque_declaration.h"],
              headerInclude =
              "opaque_declaration.h"},
          commentChildren = []}},
  DeclEmpty
    EmptyData {
      emptyDataName = Name
        "@NsTypeConstr"
        "Opaque_union",
      emptyDataOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "opaque_declaration.h:13:7",
          declId = NamePair {
            nameC = Name "opaque_union",
            nameHsIdent = Identifier
              "Opaque_union"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["opaque_declaration.h"],
              headerInclude =
              "opaque_declaration.h"},
          declComment = Nothing},
        declKind = Opaque
          (NameKindTagged TagKindUnion),
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      emptyDataComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "opaque_union",
          commentLocation = Just
            "opaque_declaration.h:13:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["opaque_declaration.h"],
              headerInclude =
              "opaque_declaration.h"},
          commentChildren = []}}]
