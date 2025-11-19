[
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Foo",
      structConstr = Name
        "@NsConstr"
        "Foo",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "foo_i",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "nested_types.h:2:9",
                fieldName = NamePair {
                  nameC = Name "i",
                  nameHsIdent = Identifier
                    "foo_i"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "i",
              commentLocation = Just
                "nested_types.h:2:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["nested_types.h"],
                  headerInclude =
                  "nested_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "foo_c",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "nested_types.h:3:10",
                fieldName = NamePair {
                  nameC = Name "c",
                  nameHsIdent = Identifier
                    "foo_c"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "c",
              commentLocation = Just
                "nested_types.h:3:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["nested_types.h"],
                  headerInclude =
                  "nested_types.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "nested_types.h:1:8",
            declId = NamePair {
              nameC = Name "foo",
              nameHsIdent = Identifier "Foo"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["nested_types.h"],
                headerInclude =
                "nested_types.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Foo"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "nested_types.h:2:9",
                    fieldName = NamePair {
                      nameC = Name "i",
                      nameHsIdent = Identifier
                        "foo_i"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_types.h:3:10",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = Identifier
                        "foo_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 32,
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
          commentOrigin = Just "foo",
          commentLocation = Just
            "nested_types.h:1:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["nested_types.h"],
              headerInclude =
              "nested_types.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Foo",
          structConstr = Name
            "@NsConstr"
            "Foo",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "foo_i",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "nested_types.h:2:9",
                    fieldName = NamePair {
                      nameC = Name "i",
                      nameHsIdent = Identifier
                        "foo_i"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "i",
                  commentLocation = Just
                    "nested_types.h:2:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["nested_types.h"],
                      headerInclude =
                      "nested_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "foo_c",
              fieldType = HsPrimType
                HsPrimCChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_types.h:3:10",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = Identifier
                        "foo_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "c",
                  commentLocation = Just
                    "nested_types.h:3:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["nested_types.h"],
                      headerInclude =
                      "nested_types.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "nested_types.h:1:8",
                declId = NamePair {
                  nameC = Name "foo",
                  nameHsIdent = Identifier "Foo"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["nested_types.h"],
                    headerInclude =
                    "nested_types.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Foo"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "nested_types.h:2:9",
                        fieldName = NamePair {
                          nameC = Name "i",
                          nameHsIdent = Identifier
                            "foo_i"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "nested_types.h:3:10",
                        fieldName = NamePair {
                          nameC = Name "c",
                          nameHsIdent = Identifier
                            "foo_c"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 32,
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
              commentOrigin = Just "foo",
              commentLocation = Just
                "nested_types.h:1:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["nested_types.h"],
                  headerInclude =
                  "nested_types.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 8,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Foo",
                  structConstr = Name
                    "@NsConstr"
                    "Foo",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "foo_i",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "nested_types.h:2:9",
                            fieldName = NamePair {
                              nameC = Name "i",
                              nameHsIdent = Identifier
                                "foo_i"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "i",
                          commentLocation = Just
                            "nested_types.h:2:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "foo_c",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_types.h:3:10",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = Identifier
                                "foo_c"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "nested_types.h:3:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "nested_types.h:1:8",
                        declId = NamePair {
                          nameC = Name "foo",
                          nameHsIdent = Identifier "Foo"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["nested_types.h"],
                            headerInclude =
                            "nested_types.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Foo"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "nested_types.h:2:9",
                                fieldName = NamePair {
                                  nameC = Name "i",
                                  nameHsIdent = Identifier
                                    "foo_i"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_types.h:3:10",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = Identifier
                                    "foo_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 32,
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
                      commentOrigin = Just "foo",
                      commentLocation = Just
                        "nested_types.h:1:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["nested_types.h"],
                          headerInclude =
                          "nested_types.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "foo_i")
                  (Idx 0),
                PeekCField
                  (HsStrLit "foo_c")
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
                    "Foo",
                  structConstr = Name
                    "@NsConstr"
                    "Foo",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "foo_i",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "nested_types.h:2:9",
                            fieldName = NamePair {
                              nameC = Name "i",
                              nameHsIdent = Identifier
                                "foo_i"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "i",
                          commentLocation = Just
                            "nested_types.h:2:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "foo_c",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_types.h:3:10",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = Identifier
                                "foo_c"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "nested_types.h:3:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "nested_types.h:1:8",
                        declId = NamePair {
                          nameC = Name "foo",
                          nameHsIdent = Identifier "Foo"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["nested_types.h"],
                            headerInclude =
                            "nested_types.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Foo"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "nested_types.h:2:9",
                                fieldName = NamePair {
                                  nameC = Name "i",
                                  nameHsIdent = Identifier
                                    "foo_i"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_types.h:3:10",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = Identifier
                                    "foo_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 32,
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
                      commentOrigin = Just "foo",
                      commentLocation = Just
                        "nested_types.h:1:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["nested_types.h"],
                          headerInclude =
                          "nested_types.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "foo_i")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "foo_c")
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
        "Foo",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Foo"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "foo_i",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCInt,
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
            (Name "@NsTypeConstr" "Foo"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "foo_i",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
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
            (Name "@NsTypeConstr" "Foo"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "foo_c",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCChar,
          hasCFieldInstanceFieldOffset =
          4},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Foo"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "foo_c",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCChar,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
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
            "bar_foo1",
          fieldType = HsTypRef
            (Name "@NsTypeConstr" "Foo"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "nested_types.h:7:16",
                fieldName = NamePair {
                  nameC = Name "foo1",
                  nameHsIdent = Identifier
                    "bar_foo1"},
                fieldComment = Nothing},
              structFieldType = TypeStruct
                NamePair {
                  nameC = Name "foo",
                  nameHsIdent = Identifier "Foo"}
                NameOriginInSource,
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "foo1",
              commentLocation = Just
                "nested_types.h:7:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["nested_types.h"],
                  headerInclude =
                  "nested_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "bar_foo2",
          fieldType = HsTypRef
            (Name "@NsTypeConstr" "Foo"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "nested_types.h:8:16",
                fieldName = NamePair {
                  nameC = Name "foo2",
                  nameHsIdent = Identifier
                    "bar_foo2"},
                fieldComment = Nothing},
              structFieldType = TypeStruct
                NamePair {
                  nameC = Name "foo",
                  nameHsIdent = Identifier "Foo"}
                NameOriginInSource,
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "foo2",
              commentLocation = Just
                "nested_types.h:8:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["nested_types.h"],
                  headerInclude =
                  "nested_types.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "nested_types.h:6:8",
            declId = NamePair {
              nameC = Name "bar",
              nameHsIdent = Identifier "Bar"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["nested_types.h"],
                headerInclude =
                "nested_types.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Bar"),
              structSizeof = 16,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_types.h:7:16",
                    fieldName = NamePair {
                      nameC = Name "foo1",
                      nameHsIdent = Identifier
                        "bar_foo1"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "foo",
                      nameHsIdent = Identifier "Foo"}
                    NameOriginInSource,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_types.h:8:16",
                    fieldName = NamePair {
                      nameC = Name "foo2",
                      nameHsIdent = Identifier
                        "bar_foo2"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "foo",
                      nameHsIdent = Identifier "Foo"}
                    NameOriginInSource,
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
            "nested_types.h:6:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["nested_types.h"],
              headerInclude =
              "nested_types.h"},
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
                "bar_foo1",
              fieldType = HsTypRef
                (Name "@NsTypeConstr" "Foo"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_types.h:7:16",
                    fieldName = NamePair {
                      nameC = Name "foo1",
                      nameHsIdent = Identifier
                        "bar_foo1"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "foo",
                      nameHsIdent = Identifier "Foo"}
                    NameOriginInSource,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "foo1",
                  commentLocation = Just
                    "nested_types.h:7:16",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["nested_types.h"],
                      headerInclude =
                      "nested_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "bar_foo2",
              fieldType = HsTypRef
                (Name "@NsTypeConstr" "Foo"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_types.h:8:16",
                    fieldName = NamePair {
                      nameC = Name "foo2",
                      nameHsIdent = Identifier
                        "bar_foo2"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "foo",
                      nameHsIdent = Identifier "Foo"}
                    NameOriginInSource,
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "foo2",
                  commentLocation = Just
                    "nested_types.h:8:16",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["nested_types.h"],
                      headerInclude =
                      "nested_types.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "nested_types.h:6:8",
                declId = NamePair {
                  nameC = Name "bar",
                  nameHsIdent = Identifier "Bar"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["nested_types.h"],
                    headerInclude =
                    "nested_types.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Bar"),
                  structSizeof = 16,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "nested_types.h:7:16",
                        fieldName = NamePair {
                          nameC = Name "foo1",
                          nameHsIdent = Identifier
                            "bar_foo1"},
                        fieldComment = Nothing},
                      structFieldType = TypeStruct
                        NamePair {
                          nameC = Name "foo",
                          nameHsIdent = Identifier "Foo"}
                        NameOriginInSource,
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "nested_types.h:8:16",
                        fieldName = NamePair {
                          nameC = Name "foo2",
                          nameHsIdent = Identifier
                            "bar_foo2"},
                        fieldComment = Nothing},
                      structFieldType = TypeStruct
                        NamePair {
                          nameC = Name "foo",
                          nameHsIdent = Identifier "Foo"}
                        NameOriginInSource,
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
                "nested_types.h:6:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["nested_types.h"],
                  headerInclude =
                  "nested_types.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 16,
          storableAlignment = 4,
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
                        "bar_foo1",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "Foo"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_types.h:7:16",
                            fieldName = NamePair {
                              nameC = Name "foo1",
                              nameHsIdent = Identifier
                                "bar_foo1"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "foo",
                              nameHsIdent = Identifier "Foo"}
                            NameOriginInSource,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "foo1",
                          commentLocation = Just
                            "nested_types.h:7:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "bar_foo2",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "Foo"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_types.h:8:16",
                            fieldName = NamePair {
                              nameC = Name "foo2",
                              nameHsIdent = Identifier
                                "bar_foo2"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "foo",
                              nameHsIdent = Identifier "Foo"}
                            NameOriginInSource,
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "foo2",
                          commentLocation = Just
                            "nested_types.h:8:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "nested_types.h:6:8",
                        declId = NamePair {
                          nameC = Name "bar",
                          nameHsIdent = Identifier "Bar"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["nested_types.h"],
                            headerInclude =
                            "nested_types.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Bar"),
                          structSizeof = 16,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_types.h:7:16",
                                fieldName = NamePair {
                                  nameC = Name "foo1",
                                  nameHsIdent = Identifier
                                    "bar_foo1"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "foo",
                                  nameHsIdent = Identifier "Foo"}
                                NameOriginInSource,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_types.h:8:16",
                                fieldName = NamePair {
                                  nameC = Name "foo2",
                                  nameHsIdent = Identifier
                                    "bar_foo2"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "foo",
                                  nameHsIdent = Identifier "Foo"}
                                NameOriginInSource,
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
                        "nested_types.h:6:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["nested_types.h"],
                          headerInclude =
                          "nested_types.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "bar_foo1")
                  (Idx 0),
                PeekCField
                  (HsStrLit "bar_foo2")
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
                        "bar_foo1",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "Foo"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_types.h:7:16",
                            fieldName = NamePair {
                              nameC = Name "foo1",
                              nameHsIdent = Identifier
                                "bar_foo1"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "foo",
                              nameHsIdent = Identifier "Foo"}
                            NameOriginInSource,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "foo1",
                          commentLocation = Just
                            "nested_types.h:7:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "bar_foo2",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "Foo"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_types.h:8:16",
                            fieldName = NamePair {
                              nameC = Name "foo2",
                              nameHsIdent = Identifier
                                "bar_foo2"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "foo",
                              nameHsIdent = Identifier "Foo"}
                            NameOriginInSource,
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "foo2",
                          commentLocation = Just
                            "nested_types.h:8:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "nested_types.h:6:8",
                        declId = NamePair {
                          nameC = Name "bar",
                          nameHsIdent = Identifier "Bar"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["nested_types.h"],
                            headerInclude =
                            "nested_types.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Bar"),
                          structSizeof = 16,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_types.h:7:16",
                                fieldName = NamePair {
                                  nameC = Name "foo1",
                                  nameHsIdent = Identifier
                                    "bar_foo1"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "foo",
                                  nameHsIdent = Identifier "Foo"}
                                NameOriginInSource,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_types.h:8:16",
                                fieldName = NamePair {
                                  nameC = Name "foo2",
                                  nameHsIdent = Identifier
                                    "bar_foo2"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "foo",
                                  nameHsIdent = Identifier "Foo"}
                                NameOriginInSource,
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
                        "nested_types.h:6:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["nested_types.h"],
                          headerInclude =
                          "nested_types.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "bar_foo1")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "bar_foo2")
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
          Name "@NsVar" "bar_foo1",
          hasCFieldInstanceCFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "Foo"),
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
            "bar_foo1",
          hasFieldInstanceFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "Foo"),
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
          Name "@NsVar" "bar_foo2",
          hasCFieldInstanceCFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "Foo"),
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
            "bar_foo2",
          hasFieldInstanceFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "Foo"),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Ex3_ex3_struct",
      structConstr = Name
        "@NsConstr"
        "Ex3_ex3_struct",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "ex3_ex3_struct_ex3_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "nested_types.h:13:13",
                fieldName = NamePair {
                  nameC = Name "ex3_a",
                  nameHsIdent = Identifier
                    "ex3_ex3_struct_ex3_a"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "ex3_a",
              commentLocation = Just
                "nested_types.h:13:13",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["nested_types.h"],
                  headerInclude =
                  "nested_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "ex3_ex3_struct_ex3_b",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "nested_types.h:14:14",
                fieldName = NamePair {
                  nameC = Name "ex3_b",
                  nameHsIdent = Identifier
                    "ex3_ex3_struct_ex3_b"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "ex3_b",
              commentLocation = Just
                "nested_types.h:14:14",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["nested_types.h"],
                  headerInclude =
                  "nested_types.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "nested_types.h:12:5",
            declId = NamePair {
              nameC = Name "ex3_ex3_struct",
              nameHsIdent = Identifier
                "Ex3_ex3_struct"},
            declOrigin = NameOriginGenerated
              (AnonId "nested_types.h:12:5"),
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["nested_types.h"],
                headerInclude =
                "nested_types.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name
                  "@NsConstr"
                  "Ex3_ex3_struct"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_types.h:13:13",
                    fieldName = NamePair {
                      nameC = Name "ex3_a",
                      nameHsIdent = Identifier
                        "ex3_ex3_struct_ex3_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_types.h:14:14",
                    fieldName = NamePair {
                      nameC = Name "ex3_b",
                      nameHsIdent = Identifier
                        "ex3_ex3_struct_ex3_b"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 32,
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
          commentOrigin = Nothing,
          commentLocation = Just
            "nested_types.h:12:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["nested_types.h"],
              headerInclude =
              "nested_types.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Ex3_ex3_struct",
          structConstr = Name
            "@NsConstr"
            "Ex3_ex3_struct",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "ex3_ex3_struct_ex3_a",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_types.h:13:13",
                    fieldName = NamePair {
                      nameC = Name "ex3_a",
                      nameHsIdent = Identifier
                        "ex3_ex3_struct_ex3_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "ex3_a",
                  commentLocation = Just
                    "nested_types.h:13:13",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["nested_types.h"],
                      headerInclude =
                      "nested_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "ex3_ex3_struct_ex3_b",
              fieldType = HsPrimType
                HsPrimCChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_types.h:14:14",
                    fieldName = NamePair {
                      nameC = Name "ex3_b",
                      nameHsIdent = Identifier
                        "ex3_ex3_struct_ex3_b"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "ex3_b",
                  commentLocation = Just
                    "nested_types.h:14:14",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["nested_types.h"],
                      headerInclude =
                      "nested_types.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "nested_types.h:12:5",
                declId = NamePair {
                  nameC = Name "ex3_ex3_struct",
                  nameHsIdent = Identifier
                    "Ex3_ex3_struct"},
                declOrigin = NameOriginGenerated
                  (AnonId "nested_types.h:12:5"),
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["nested_types.h"],
                    headerInclude =
                    "nested_types.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name
                      "@NsConstr"
                      "Ex3_ex3_struct"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "nested_types.h:13:13",
                        fieldName = NamePair {
                          nameC = Name "ex3_a",
                          nameHsIdent = Identifier
                            "ex3_ex3_struct_ex3_a"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "nested_types.h:14:14",
                        fieldName = NamePair {
                          nameC = Name "ex3_b",
                          nameHsIdent = Identifier
                            "ex3_ex3_struct_ex3_b"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 32,
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
              commentOrigin = Nothing,
              commentLocation = Just
                "nested_types.h:12:5",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["nested_types.h"],
                  headerInclude =
                  "nested_types.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 8,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Ex3_ex3_struct",
                  structConstr = Name
                    "@NsConstr"
                    "Ex3_ex3_struct",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "ex3_ex3_struct_ex3_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_types.h:13:13",
                            fieldName = NamePair {
                              nameC = Name "ex3_a",
                              nameHsIdent = Identifier
                                "ex3_ex3_struct_ex3_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ex3_a",
                          commentLocation = Just
                            "nested_types.h:13:13",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "ex3_ex3_struct_ex3_b",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_types.h:14:14",
                            fieldName = NamePair {
                              nameC = Name "ex3_b",
                              nameHsIdent = Identifier
                                "ex3_ex3_struct_ex3_b"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ex3_b",
                          commentLocation = Just
                            "nested_types.h:14:14",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "nested_types.h:12:5",
                        declId = NamePair {
                          nameC = Name "ex3_ex3_struct",
                          nameHsIdent = Identifier
                            "Ex3_ex3_struct"},
                        declOrigin = NameOriginGenerated
                          (AnonId "nested_types.h:12:5"),
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["nested_types.h"],
                            headerInclude =
                            "nested_types.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Ex3_ex3_struct"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_types.h:13:13",
                                fieldName = NamePair {
                                  nameC = Name "ex3_a",
                                  nameHsIdent = Identifier
                                    "ex3_ex3_struct_ex3_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_types.h:14:14",
                                fieldName = NamePair {
                                  nameC = Name "ex3_b",
                                  nameHsIdent = Identifier
                                    "ex3_ex3_struct_ex3_b"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 32,
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
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "nested_types.h:12:5",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["nested_types.h"],
                          headerInclude =
                          "nested_types.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit
                    "ex3_ex3_struct_ex3_a")
                  (Idx 0),
                PeekCField
                  (HsStrLit
                    "ex3_ex3_struct_ex3_b")
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
                    "Ex3_ex3_struct",
                  structConstr = Name
                    "@NsConstr"
                    "Ex3_ex3_struct",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "ex3_ex3_struct_ex3_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_types.h:13:13",
                            fieldName = NamePair {
                              nameC = Name "ex3_a",
                              nameHsIdent = Identifier
                                "ex3_ex3_struct_ex3_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ex3_a",
                          commentLocation = Just
                            "nested_types.h:13:13",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "ex3_ex3_struct_ex3_b",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_types.h:14:14",
                            fieldName = NamePair {
                              nameC = Name "ex3_b",
                              nameHsIdent = Identifier
                                "ex3_ex3_struct_ex3_b"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ex3_b",
                          commentLocation = Just
                            "nested_types.h:14:14",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "nested_types.h:12:5",
                        declId = NamePair {
                          nameC = Name "ex3_ex3_struct",
                          nameHsIdent = Identifier
                            "Ex3_ex3_struct"},
                        declOrigin = NameOriginGenerated
                          (AnonId "nested_types.h:12:5"),
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["nested_types.h"],
                            headerInclude =
                            "nested_types.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Ex3_ex3_struct"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_types.h:13:13",
                                fieldName = NamePair {
                                  nameC = Name "ex3_a",
                                  nameHsIdent = Identifier
                                    "ex3_ex3_struct_ex3_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_types.h:14:14",
                                fieldName = NamePair {
                                  nameC = Name "ex3_b",
                                  nameHsIdent = Identifier
                                    "ex3_ex3_struct_ex3_b"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 32,
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
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "nested_types.h:12:5",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["nested_types.h"],
                          headerInclude =
                          "nested_types.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit
                        "ex3_ex3_struct_ex3_a")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit
                        "ex3_ex3_struct_ex3_b")
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
        "Ex3_ex3_struct",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Ex3_ex3_struct",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Ex3_ex3_struct"),
          hasCFieldInstanceFieldName =
          Name
            "@NsVar"
            "ex3_ex3_struct_ex3_a",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCInt,
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
            (Name
              "@NsTypeConstr"
              "Ex3_ex3_struct"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "ex3_ex3_struct_ex3_a",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
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
            (Name
              "@NsTypeConstr"
              "Ex3_ex3_struct"),
          hasCFieldInstanceFieldName =
          Name
            "@NsVar"
            "ex3_ex3_struct_ex3_b",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCChar,
          hasCFieldInstanceFieldOffset =
          4},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Ex3_ex3_struct"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "ex3_ex3_struct_ex3_b",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCChar,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Ex3",
      structConstr = Name
        "@NsConstr"
        "Ex3",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "ex3_ex3_struct",
          fieldType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Ex3_ex3_struct"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "nested_types.h:15:7",
                fieldName = NamePair {
                  nameC = Name "ex3_struct",
                  nameHsIdent = Identifier
                    "ex3_ex3_struct"},
                fieldComment = Nothing},
              structFieldType = TypeStruct
                NamePair {
                  nameC = Name "ex3_ex3_struct",
                  nameHsIdent = Identifier
                    "Ex3_ex3_struct"}
                (NameOriginGenerated
                  (AnonId "nested_types.h:12:5")),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "ex3_struct",
              commentLocation = Just
                "nested_types.h:15:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["nested_types.h"],
                  headerInclude =
                  "nested_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "ex3_ex3_c",
          fieldType = HsPrimType
            HsPrimCFloat,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "nested_types.h:16:11",
                fieldName = NamePair {
                  nameC = Name "ex3_c",
                  nameHsIdent = Identifier
                    "ex3_ex3_c"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimFloating PrimFloat),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "ex3_c",
              commentLocation = Just
                "nested_types.h:16:11",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["nested_types.h"],
                  headerInclude =
                  "nested_types.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "nested_types.h:11:8",
            declId = NamePair {
              nameC = Name "ex3",
              nameHsIdent = Identifier "Ex3"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["nested_types.h"],
                headerInclude =
                "nested_types.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Ex3"),
              structSizeof = 12,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_types.h:15:7",
                    fieldName = NamePair {
                      nameC = Name "ex3_struct",
                      nameHsIdent = Identifier
                        "ex3_ex3_struct"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "ex3_ex3_struct",
                      nameHsIdent = Identifier
                        "Ex3_ex3_struct"}
                    (NameOriginGenerated
                      (AnonId "nested_types.h:12:5")),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_types.h:16:11",
                    fieldName = NamePair {
                      nameC = Name "ex3_c",
                      nameHsIdent = Identifier
                        "ex3_ex3_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimFloat),
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
          commentOrigin = Just "ex3",
          commentLocation = Just
            "nested_types.h:11:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["nested_types.h"],
              headerInclude =
              "nested_types.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Ex3",
          structConstr = Name
            "@NsConstr"
            "Ex3",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "ex3_ex3_struct",
              fieldType = HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Ex3_ex3_struct"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_types.h:15:7",
                    fieldName = NamePair {
                      nameC = Name "ex3_struct",
                      nameHsIdent = Identifier
                        "ex3_ex3_struct"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "ex3_ex3_struct",
                      nameHsIdent = Identifier
                        "Ex3_ex3_struct"}
                    (NameOriginGenerated
                      (AnonId "nested_types.h:12:5")),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just
                    "ex3_struct",
                  commentLocation = Just
                    "nested_types.h:15:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["nested_types.h"],
                      headerInclude =
                      "nested_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "ex3_ex3_c",
              fieldType = HsPrimType
                HsPrimCFloat,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_types.h:16:11",
                    fieldName = NamePair {
                      nameC = Name "ex3_c",
                      nameHsIdent = Identifier
                        "ex3_ex3_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimFloat),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "ex3_c",
                  commentLocation = Just
                    "nested_types.h:16:11",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["nested_types.h"],
                      headerInclude =
                      "nested_types.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "nested_types.h:11:8",
                declId = NamePair {
                  nameC = Name "ex3",
                  nameHsIdent = Identifier "Ex3"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["nested_types.h"],
                    headerInclude =
                    "nested_types.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Ex3"),
                  structSizeof = 12,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "nested_types.h:15:7",
                        fieldName = NamePair {
                          nameC = Name "ex3_struct",
                          nameHsIdent = Identifier
                            "ex3_ex3_struct"},
                        fieldComment = Nothing},
                      structFieldType = TypeStruct
                        NamePair {
                          nameC = Name "ex3_ex3_struct",
                          nameHsIdent = Identifier
                            "Ex3_ex3_struct"}
                        (NameOriginGenerated
                          (AnonId "nested_types.h:12:5")),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "nested_types.h:16:11",
                        fieldName = NamePair {
                          nameC = Name "ex3_c",
                          nameHsIdent = Identifier
                            "ex3_ex3_c"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimFloating PrimFloat),
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
              commentOrigin = Just "ex3",
              commentLocation = Just
                "nested_types.h:11:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["nested_types.h"],
                  headerInclude =
                  "nested_types.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 12,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Ex3",
                  structConstr = Name
                    "@NsConstr"
                    "Ex3",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "ex3_ex3_struct",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Ex3_ex3_struct"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_types.h:15:7",
                            fieldName = NamePair {
                              nameC = Name "ex3_struct",
                              nameHsIdent = Identifier
                                "ex3_ex3_struct"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "ex3_ex3_struct",
                              nameHsIdent = Identifier
                                "Ex3_ex3_struct"}
                            (NameOriginGenerated
                              (AnonId "nested_types.h:12:5")),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "ex3_struct",
                          commentLocation = Just
                            "nested_types.h:15:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "ex3_ex3_c",
                      fieldType = HsPrimType
                        HsPrimCFloat,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_types.h:16:11",
                            fieldName = NamePair {
                              nameC = Name "ex3_c",
                              nameHsIdent = Identifier
                                "ex3_ex3_c"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimFloat),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ex3_c",
                          commentLocation = Just
                            "nested_types.h:16:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "nested_types.h:11:8",
                        declId = NamePair {
                          nameC = Name "ex3",
                          nameHsIdent = Identifier "Ex3"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["nested_types.h"],
                            headerInclude =
                            "nested_types.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Ex3"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_types.h:15:7",
                                fieldName = NamePair {
                                  nameC = Name "ex3_struct",
                                  nameHsIdent = Identifier
                                    "ex3_ex3_struct"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "ex3_ex3_struct",
                                  nameHsIdent = Identifier
                                    "Ex3_ex3_struct"}
                                (NameOriginGenerated
                                  (AnonId "nested_types.h:12:5")),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_types.h:16:11",
                                fieldName = NamePair {
                                  nameC = Name "ex3_c",
                                  nameHsIdent = Identifier
                                    "ex3_ex3_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimFloat),
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
                      commentOrigin = Just "ex3",
                      commentLocation = Just
                        "nested_types.h:11:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["nested_types.h"],
                          headerInclude =
                          "nested_types.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "ex3_ex3_struct")
                  (Idx 0),
                PeekCField
                  (HsStrLit "ex3_ex3_c")
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
                    "Ex3",
                  structConstr = Name
                    "@NsConstr"
                    "Ex3",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "ex3_ex3_struct",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Ex3_ex3_struct"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_types.h:15:7",
                            fieldName = NamePair {
                              nameC = Name "ex3_struct",
                              nameHsIdent = Identifier
                                "ex3_ex3_struct"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "ex3_ex3_struct",
                              nameHsIdent = Identifier
                                "Ex3_ex3_struct"}
                            (NameOriginGenerated
                              (AnonId "nested_types.h:12:5")),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "ex3_struct",
                          commentLocation = Just
                            "nested_types.h:15:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "ex3_ex3_c",
                      fieldType = HsPrimType
                        HsPrimCFloat,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_types.h:16:11",
                            fieldName = NamePair {
                              nameC = Name "ex3_c",
                              nameHsIdent = Identifier
                                "ex3_ex3_c"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimFloat),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ex3_c",
                          commentLocation = Just
                            "nested_types.h:16:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "nested_types.h:11:8",
                        declId = NamePair {
                          nameC = Name "ex3",
                          nameHsIdent = Identifier "Ex3"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["nested_types.h"],
                            headerInclude =
                            "nested_types.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Ex3"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_types.h:15:7",
                                fieldName = NamePair {
                                  nameC = Name "ex3_struct",
                                  nameHsIdent = Identifier
                                    "ex3_ex3_struct"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "ex3_ex3_struct",
                                  nameHsIdent = Identifier
                                    "Ex3_ex3_struct"}
                                (NameOriginGenerated
                                  (AnonId "nested_types.h:12:5")),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_types.h:16:11",
                                fieldName = NamePair {
                                  nameC = Name "ex3_c",
                                  nameHsIdent = Identifier
                                    "ex3_ex3_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimFloat),
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
                      commentOrigin = Just "ex3",
                      commentLocation = Just
                        "nested_types.h:11:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["nested_types.h"],
                          headerInclude =
                          "nested_types.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "ex3_ex3_struct")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "ex3_ex3_c")
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
        "Ex3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Ex3",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Ex3"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "ex3_ex3_struct",
          hasCFieldInstanceCFieldType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Ex3_ex3_struct"),
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
            (Name "@NsTypeConstr" "Ex3"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "ex3_ex3_struct",
          hasFieldInstanceFieldType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Ex3_ex3_struct"),
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
            (Name "@NsTypeConstr" "Ex3"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "ex3_ex3_c",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCFloat,
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
            (Name "@NsTypeConstr" "Ex3"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "ex3_ex3_c",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCFloat,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Ex4_odd",
      structConstr = Name
        "@NsConstr"
        "Ex4_odd",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "ex4_odd_value",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "nested_types.h:23:9",
                fieldName = NamePair {
                  nameC = Name "value",
                  nameHsIdent = Identifier
                    "ex4_odd_value"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "value",
              commentLocation = Just
                "nested_types.h:23:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["nested_types.h"],
                  headerInclude =
                  "nested_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "ex4_odd_next",
          fieldType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Ex4_even")),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "nested_types.h:27:8",
                fieldName = NamePair {
                  nameC = Name "next",
                  nameHsIdent = Identifier
                    "ex4_odd_next"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "ex4_even",
                    nameHsIdent = Identifier
                      "Ex4_even"}
                  NameOriginInSource),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "next",
              commentLocation = Just
                "nested_types.h:27:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["nested_types.h"],
                  headerInclude =
                  "nested_types.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "nested_types.h:22:8",
            declId = NamePair {
              nameC = Name "ex4_odd",
              nameHsIdent = Identifier
                "Ex4_odd"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["nested_types.h"],
                headerInclude =
                "nested_types.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Ex4_odd"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_types.h:23:9",
                    fieldName = NamePair {
                      nameC = Name "value",
                      nameHsIdent = Identifier
                        "ex4_odd_value"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_types.h:27:8",
                    fieldName = NamePair {
                      nameC = Name "next",
                      nameHsIdent = Identifier
                        "ex4_odd_next"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "ex4_even",
                        nameHsIdent = Identifier
                          "Ex4_even"}
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
          commentOrigin = Just "ex4_odd",
          commentLocation = Just
            "nested_types.h:22:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["nested_types.h"],
              headerInclude =
              "nested_types.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Ex4_odd",
          structConstr = Name
            "@NsConstr"
            "Ex4_odd",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "ex4_odd_value",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_types.h:23:9",
                    fieldName = NamePair {
                      nameC = Name "value",
                      nameHsIdent = Identifier
                        "ex4_odd_value"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "value",
                  commentLocation = Just
                    "nested_types.h:23:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["nested_types.h"],
                      headerInclude =
                      "nested_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "ex4_odd_next",
              fieldType = HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Ex4_even")),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_types.h:27:8",
                    fieldName = NamePair {
                      nameC = Name "next",
                      nameHsIdent = Identifier
                        "ex4_odd_next"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "ex4_even",
                        nameHsIdent = Identifier
                          "Ex4_even"}
                      NameOriginInSource),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "next",
                  commentLocation = Just
                    "nested_types.h:27:8",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["nested_types.h"],
                      headerInclude =
                      "nested_types.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "nested_types.h:22:8",
                declId = NamePair {
                  nameC = Name "ex4_odd",
                  nameHsIdent = Identifier
                    "Ex4_odd"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["nested_types.h"],
                    headerInclude =
                    "nested_types.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Ex4_odd"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "nested_types.h:23:9",
                        fieldName = NamePair {
                          nameC = Name "value",
                          nameHsIdent = Identifier
                            "ex4_odd_value"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "nested_types.h:27:8",
                        fieldName = NamePair {
                          nameC = Name "next",
                          nameHsIdent = Identifier
                            "ex4_odd_next"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeStruct
                          NamePair {
                            nameC = Name "ex4_even",
                            nameHsIdent = Identifier
                              "Ex4_even"}
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
              commentOrigin = Just "ex4_odd",
              commentLocation = Just
                "nested_types.h:22:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["nested_types.h"],
                  headerInclude =
                  "nested_types.h"},
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
                    "Ex4_odd",
                  structConstr = Name
                    "@NsConstr"
                    "Ex4_odd",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "ex4_odd_value",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_types.h:23:9",
                            fieldName = NamePair {
                              nameC = Name "value",
                              nameHsIdent = Identifier
                                "ex4_odd_value"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "value",
                          commentLocation = Just
                            "nested_types.h:23:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "ex4_odd_next",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name
                            "@NsTypeConstr"
                            "Ex4_even")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_types.h:27:8",
                            fieldName = NamePair {
                              nameC = Name "next",
                              nameHsIdent = Identifier
                                "ex4_odd_next"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "ex4_even",
                                nameHsIdent = Identifier
                                  "Ex4_even"}
                              NameOriginInSource),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "next",
                          commentLocation = Just
                            "nested_types.h:27:8",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "nested_types.h:22:8",
                        declId = NamePair {
                          nameC = Name "ex4_odd",
                          nameHsIdent = Identifier
                            "Ex4_odd"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["nested_types.h"],
                            headerInclude =
                            "nested_types.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Ex4_odd"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_types.h:23:9",
                                fieldName = NamePair {
                                  nameC = Name "value",
                                  nameHsIdent = Identifier
                                    "ex4_odd_value"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_types.h:27:8",
                                fieldName = NamePair {
                                  nameC = Name "next",
                                  nameHsIdent = Identifier
                                    "ex4_odd_next"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "ex4_even",
                                    nameHsIdent = Identifier
                                      "Ex4_even"}
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
                      commentOrigin = Just "ex4_odd",
                      commentLocation = Just
                        "nested_types.h:22:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["nested_types.h"],
                          headerInclude =
                          "nested_types.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "ex4_odd_value")
                  (Idx 0),
                PeekCField
                  (HsStrLit "ex4_odd_next")
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
                    "Ex4_odd",
                  structConstr = Name
                    "@NsConstr"
                    "Ex4_odd",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "ex4_odd_value",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_types.h:23:9",
                            fieldName = NamePair {
                              nameC = Name "value",
                              nameHsIdent = Identifier
                                "ex4_odd_value"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "value",
                          commentLocation = Just
                            "nested_types.h:23:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "ex4_odd_next",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name
                            "@NsTypeConstr"
                            "Ex4_even")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_types.h:27:8",
                            fieldName = NamePair {
                              nameC = Name "next",
                              nameHsIdent = Identifier
                                "ex4_odd_next"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "ex4_even",
                                nameHsIdent = Identifier
                                  "Ex4_even"}
                              NameOriginInSource),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "next",
                          commentLocation = Just
                            "nested_types.h:27:8",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "nested_types.h:22:8",
                        declId = NamePair {
                          nameC = Name "ex4_odd",
                          nameHsIdent = Identifier
                            "Ex4_odd"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["nested_types.h"],
                            headerInclude =
                            "nested_types.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Ex4_odd"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_types.h:23:9",
                                fieldName = NamePair {
                                  nameC = Name "value",
                                  nameHsIdent = Identifier
                                    "ex4_odd_value"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_types.h:27:8",
                                fieldName = NamePair {
                                  nameC = Name "next",
                                  nameHsIdent = Identifier
                                    "ex4_odd_next"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "ex4_even",
                                    nameHsIdent = Identifier
                                      "Ex4_even"}
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
                      commentOrigin = Just "ex4_odd",
                      commentLocation = Just
                        "nested_types.h:22:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["nested_types.h"],
                          headerInclude =
                          "nested_types.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "ex4_odd_value")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "ex4_odd_next")
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
        "Ex4_odd",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Ex4_odd",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Ex4_odd"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "ex4_odd_value",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCInt,
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
            (Name
              "@NsTypeConstr"
              "Ex4_odd"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "ex4_odd_value",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
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
            (Name
              "@NsTypeConstr"
              "Ex4_odd"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "ex4_odd_next",
          hasCFieldInstanceCFieldType =
          HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Ex4_even")),
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
            (Name
              "@NsTypeConstr"
              "Ex4_odd"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "ex4_odd_next",
          hasFieldInstanceFieldType =
          HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Ex4_even")),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Ex4_even",
      structConstr = Name
        "@NsConstr"
        "Ex4_even",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "ex4_even_value",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "nested_types.h:25:16",
                fieldName = NamePair {
                  nameC = Name "value",
                  nameHsIdent = Identifier
                    "ex4_even_value"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "value",
              commentLocation = Just
                "nested_types.h:25:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["nested_types.h"],
                  headerInclude =
                  "nested_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "ex4_even_next",
          fieldType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Ex4_odd")),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "nested_types.h:26:25",
                fieldName = NamePair {
                  nameC = Name "next",
                  nameHsIdent = Identifier
                    "ex4_even_next"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "ex4_odd",
                    nameHsIdent = Identifier
                      "Ex4_odd"}
                  NameOriginInSource),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "next",
              commentLocation = Just
                "nested_types.h:26:25",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["nested_types.h"],
                  headerInclude =
                  "nested_types.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "nested_types.h:24:12",
            declId = NamePair {
              nameC = Name "ex4_even",
              nameHsIdent = Identifier
                "Ex4_even"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["nested_types.h"],
                headerInclude =
                "nested_types.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Ex4_even"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_types.h:25:16",
                    fieldName = NamePair {
                      nameC = Name "value",
                      nameHsIdent = Identifier
                        "ex4_even_value"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_types.h:26:25",
                    fieldName = NamePair {
                      nameC = Name "next",
                      nameHsIdent = Identifier
                        "ex4_even_next"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "ex4_odd",
                        nameHsIdent = Identifier
                          "Ex4_odd"}
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
          commentOrigin = Just "ex4_even",
          commentLocation = Just
            "nested_types.h:24:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["nested_types.h"],
              headerInclude =
              "nested_types.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Ex4_even",
          structConstr = Name
            "@NsConstr"
            "Ex4_even",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "ex4_even_value",
              fieldType = HsPrimType
                HsPrimCDouble,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_types.h:25:16",
                    fieldName = NamePair {
                      nameC = Name "value",
                      nameHsIdent = Identifier
                        "ex4_even_value"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "value",
                  commentLocation = Just
                    "nested_types.h:25:16",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["nested_types.h"],
                      headerInclude =
                      "nested_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "ex4_even_next",
              fieldType = HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Ex4_odd")),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_types.h:26:25",
                    fieldName = NamePair {
                      nameC = Name "next",
                      nameHsIdent = Identifier
                        "ex4_even_next"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "ex4_odd",
                        nameHsIdent = Identifier
                          "Ex4_odd"}
                      NameOriginInSource),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "next",
                  commentLocation = Just
                    "nested_types.h:26:25",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["nested_types.h"],
                      headerInclude =
                      "nested_types.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "nested_types.h:24:12",
                declId = NamePair {
                  nameC = Name "ex4_even",
                  nameHsIdent = Identifier
                    "Ex4_even"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["nested_types.h"],
                    headerInclude =
                    "nested_types.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Ex4_even"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "nested_types.h:25:16",
                        fieldName = NamePair {
                          nameC = Name "value",
                          nameHsIdent = Identifier
                            "ex4_even_value"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimFloating PrimDouble),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "nested_types.h:26:25",
                        fieldName = NamePair {
                          nameC = Name "next",
                          nameHsIdent = Identifier
                            "ex4_even_next"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeStruct
                          NamePair {
                            nameC = Name "ex4_odd",
                            nameHsIdent = Identifier
                              "Ex4_odd"}
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
              commentOrigin = Just "ex4_even",
              commentLocation = Just
                "nested_types.h:24:12",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["nested_types.h"],
                  headerInclude =
                  "nested_types.h"},
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
                    "Ex4_even",
                  structConstr = Name
                    "@NsConstr"
                    "Ex4_even",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "ex4_even_value",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_types.h:25:16",
                            fieldName = NamePair {
                              nameC = Name "value",
                              nameHsIdent = Identifier
                                "ex4_even_value"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "value",
                          commentLocation = Just
                            "nested_types.h:25:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "ex4_even_next",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name
                            "@NsTypeConstr"
                            "Ex4_odd")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_types.h:26:25",
                            fieldName = NamePair {
                              nameC = Name "next",
                              nameHsIdent = Identifier
                                "ex4_even_next"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "ex4_odd",
                                nameHsIdent = Identifier
                                  "Ex4_odd"}
                              NameOriginInSource),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "next",
                          commentLocation = Just
                            "nested_types.h:26:25",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "nested_types.h:24:12",
                        declId = NamePair {
                          nameC = Name "ex4_even",
                          nameHsIdent = Identifier
                            "Ex4_even"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["nested_types.h"],
                            headerInclude =
                            "nested_types.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Ex4_even"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_types.h:25:16",
                                fieldName = NamePair {
                                  nameC = Name "value",
                                  nameHsIdent = Identifier
                                    "ex4_even_value"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_types.h:26:25",
                                fieldName = NamePair {
                                  nameC = Name "next",
                                  nameHsIdent = Identifier
                                    "ex4_even_next"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "ex4_odd",
                                    nameHsIdent = Identifier
                                      "Ex4_odd"}
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
                      commentOrigin = Just "ex4_even",
                      commentLocation = Just
                        "nested_types.h:24:12",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["nested_types.h"],
                          headerInclude =
                          "nested_types.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "ex4_even_value")
                  (Idx 0),
                PeekCField
                  (HsStrLit "ex4_even_next")
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
                    "Ex4_even",
                  structConstr = Name
                    "@NsConstr"
                    "Ex4_even",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "ex4_even_value",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_types.h:25:16",
                            fieldName = NamePair {
                              nameC = Name "value",
                              nameHsIdent = Identifier
                                "ex4_even_value"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "value",
                          commentLocation = Just
                            "nested_types.h:25:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "ex4_even_next",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name
                            "@NsTypeConstr"
                            "Ex4_odd")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_types.h:26:25",
                            fieldName = NamePair {
                              nameC = Name "next",
                              nameHsIdent = Identifier
                                "ex4_even_next"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "ex4_odd",
                                nameHsIdent = Identifier
                                  "Ex4_odd"}
                              NameOriginInSource),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "next",
                          commentLocation = Just
                            "nested_types.h:26:25",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["nested_types.h"],
                              headerInclude =
                              "nested_types.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "nested_types.h:24:12",
                        declId = NamePair {
                          nameC = Name "ex4_even",
                          nameHsIdent = Identifier
                            "Ex4_even"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["nested_types.h"],
                            headerInclude =
                            "nested_types.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Ex4_even"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_types.h:25:16",
                                fieldName = NamePair {
                                  nameC = Name "value",
                                  nameHsIdent = Identifier
                                    "ex4_even_value"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_types.h:26:25",
                                fieldName = NamePair {
                                  nameC = Name "next",
                                  nameHsIdent = Identifier
                                    "ex4_even_next"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "ex4_odd",
                                    nameHsIdent = Identifier
                                      "Ex4_odd"}
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
                      commentOrigin = Just "ex4_even",
                      commentLocation = Just
                        "nested_types.h:24:12",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["nested_types.h"],
                          headerInclude =
                          "nested_types.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "ex4_even_value")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "ex4_even_next")
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
        "Ex4_even",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Ex4_even",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Ex4_even"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "ex4_even_value",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCDouble,
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
            (Name
              "@NsTypeConstr"
              "Ex4_even"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "ex4_even_value",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCDouble,
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
            (Name
              "@NsTypeConstr"
              "Ex4_even"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "ex4_even_next",
          hasCFieldInstanceCFieldType =
          HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Ex4_odd")),
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
            (Name
              "@NsTypeConstr"
              "Ex4_even"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "ex4_even_next",
          hasFieldInstanceFieldType =
          HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Ex4_odd")),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing}]
