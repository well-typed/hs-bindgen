[
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "UnionA",
      newtypeConstr = Name
        "@NsConstr"
        "UnionA",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_UnionA",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "nested_unions.h:2:15",
          declId = NamePair {
            nameC = Name "unionA",
            nameHsIdent = Identifier
              "UnionA"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["types/nested_unions.h"],
              headerInclude =
              "types/nested_unions.h"},
          declComment = Nothing},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "UnionA",
              newtypeField = Name
                "@NsVar"
                "un_UnionA"},
            unionSizeof = 4,
            unionAlignment = 4,
            unionFields = [
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc =
                  "nested_unions.h:3:21",
                  fieldName = NamePair {
                    nameC = Name "a",
                    nameHsIdent = Identifier
                      "unionA_a"},
                  fieldComment = Nothing},
                unionFieldType = TypePrim
                  (PrimIntegral PrimInt Signed)},
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc =
                  "nested_unions.h:4:22",
                  fieldName = NamePair {
                    nameC = Name "b",
                    nameHsIdent = Identifier
                      "unionA_b"},
                  fieldComment = Nothing},
                unionFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))}]},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "unionA",
          commentLocation = Just
            "nested_unions.h:2:15",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["types/nested_unions.h"],
              headerInclude =
              "types/nested_unions.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveVia
        (HsSizedByteArray 4 4),
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "UnionA",
      deriveInstanceComment =
      Nothing},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = Name
        "@NsVar"
        "get_unionA_a",
      unionGetterType = HsPrimType
        HsPrimCInt,
      unionGetterConstr = Name
        "@NsTypeConstr"
        "UnionA",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "a",
          commentLocation = Just
            "nested_unions.h:3:21",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["types/nested_unions.h"],
              headerInclude =
              "types/nested_unions.h"},
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier "set_unionA_a"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = Name
        "@NsVar"
        "set_unionA_a",
      unionSetterType = HsPrimType
        HsPrimCInt,
      unionSetterConstr = Name
        "@NsTypeConstr"
        "UnionA",
      unionSetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier "get_unionA_a"]]}},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = Name
        "@NsVar"
        "get_unionA_b",
      unionGetterType = HsPrimType
        HsPrimCChar,
      unionGetterConstr = Name
        "@NsTypeConstr"
        "UnionA",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "b",
          commentLocation = Just
            "nested_unions.h:4:22",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["types/nested_unions.h"],
              headerInclude =
              "types/nested_unions.h"},
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier "set_unionA_b"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = Name
        "@NsVar"
        "set_unionA_b",
      unionSetterType = HsPrimType
        HsPrimCChar,
      unionSetterConstr = Name
        "@NsTypeConstr"
        "UnionA",
      unionSetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier "get_unionA_b"]]}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "UnionA"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "unionA_a",
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
            (Name "@NsTypeConstr" "UnionA"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "unionA_a",
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
            (Name "@NsTypeConstr" "UnionA"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "unionA_b",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCChar,
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
            (Name "@NsTypeConstr" "UnionA"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "unionA_b",
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
        "ExA",
      structConstr = Name
        "@NsConstr"
        "ExA",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "exA_fieldA1",
          fieldType = HsTypRef
            (Name "@NsTypeConstr" "UnionA"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "nested_unions.h:5:11",
                fieldName = NamePair {
                  nameC = Name "fieldA1",
                  nameHsIdent = Identifier
                    "exA_fieldA1"},
                fieldComment = Nothing},
              structFieldType = TypeUnion
                NamePair {
                  nameC = Name "unionA",
                  nameHsIdent = Identifier
                    "UnionA"}
                NameOriginInSource,
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "fieldA1",
              commentLocation = Just
                "nested_unions.h:5:11",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["types/nested_unions.h"],
                  headerInclude =
                  "types/nested_unions.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "nested_unions.h:1:8",
            declId = NamePair {
              nameC = Name "exA",
              nameHsIdent = Identifier "ExA"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["types/nested_unions.h"],
                headerInclude =
                "types/nested_unions.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "ExA"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_unions.h:5:11",
                    fieldName = NamePair {
                      nameC = Name "fieldA1",
                      nameHsIdent = Identifier
                        "exA_fieldA1"},
                    fieldComment = Nothing},
                  structFieldType = TypeUnion
                    NamePair {
                      nameC = Name "unionA",
                      nameHsIdent = Identifier
                        "UnionA"}
                    NameOriginInSource,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec {
            declSpecC = Nothing,
            declSpecHs = Nothing}},
      structInstances = Set.fromList
        [Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "exA",
          commentLocation = Just
            "nested_unions.h:1:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["types/nested_unions.h"],
              headerInclude =
              "types/nested_unions.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "ExA",
          structConstr = Name
            "@NsConstr"
            "ExA",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "exA_fieldA1",
              fieldType = HsTypRef
                (Name "@NsTypeConstr" "UnionA"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_unions.h:5:11",
                    fieldName = NamePair {
                      nameC = Name "fieldA1",
                      nameHsIdent = Identifier
                        "exA_fieldA1"},
                    fieldComment = Nothing},
                  structFieldType = TypeUnion
                    NamePair {
                      nameC = Name "unionA",
                      nameHsIdent = Identifier
                        "UnionA"}
                    NameOriginInSource,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "fieldA1",
                  commentLocation = Just
                    "nested_unions.h:5:11",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["types/nested_unions.h"],
                      headerInclude =
                      "types/nested_unions.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "nested_unions.h:1:8",
                declId = NamePair {
                  nameC = Name "exA",
                  nameHsIdent = Identifier "ExA"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["types/nested_unions.h"],
                    headerInclude =
                    "types/nested_unions.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "ExA"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "nested_unions.h:5:11",
                        fieldName = NamePair {
                          nameC = Name "fieldA1",
                          nameHsIdent = Identifier
                            "exA_fieldA1"},
                        fieldComment = Nothing},
                      structFieldType = TypeUnion
                        NamePair {
                          nameC = Name "unionA",
                          nameHsIdent = Identifier
                            "UnionA"}
                        NameOriginInSource,
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec {
                declSpecC = Nothing,
                declSpecHs = Nothing}},
          structInstances = Set.fromList
            [Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "exA",
              commentLocation = Just
                "nested_unions.h:1:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["types/nested_unions.h"],
                  headerInclude =
                  "types/nested_unions.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 4,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "ExA",
                  structConstr = Name
                    "@NsConstr"
                    "ExA",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "exA_fieldA1",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "UnionA"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_unions.h:5:11",
                            fieldName = NamePair {
                              nameC = Name "fieldA1",
                              nameHsIdent = Identifier
                                "exA_fieldA1"},
                            fieldComment = Nothing},
                          structFieldType = TypeUnion
                            NamePair {
                              nameC = Name "unionA",
                              nameHsIdent = Identifier
                                "UnionA"}
                            NameOriginInSource,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "fieldA1",
                          commentLocation = Just
                            "nested_unions.h:5:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["types/nested_unions.h"],
                              headerInclude =
                              "types/nested_unions.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "nested_unions.h:1:8",
                        declId = NamePair {
                          nameC = Name "exA",
                          nameHsIdent = Identifier "ExA"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["types/nested_unions.h"],
                            headerInclude =
                            "types/nested_unions.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "ExA"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_unions.h:5:11",
                                fieldName = NamePair {
                                  nameC = Name "fieldA1",
                                  nameHsIdent = Identifier
                                    "exA_fieldA1"},
                                fieldComment = Nothing},
                              structFieldType = TypeUnion
                                NamePair {
                                  nameC = Name "unionA",
                                  nameHsIdent = Identifier
                                    "UnionA"}
                                NameOriginInSource,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "exA",
                      commentLocation = Just
                        "nested_unions.h:1:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["types/nested_unions.h"],
                          headerInclude =
                          "types/nested_unions.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "exA_fieldA1")
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
                    "ExA",
                  structConstr = Name
                    "@NsConstr"
                    "ExA",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "exA_fieldA1",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "UnionA"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_unions.h:5:11",
                            fieldName = NamePair {
                              nameC = Name "fieldA1",
                              nameHsIdent = Identifier
                                "exA_fieldA1"},
                            fieldComment = Nothing},
                          structFieldType = TypeUnion
                            NamePair {
                              nameC = Name "unionA",
                              nameHsIdent = Identifier
                                "UnionA"}
                            NameOriginInSource,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "fieldA1",
                          commentLocation = Just
                            "nested_unions.h:5:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["types/nested_unions.h"],
                              headerInclude =
                              "types/nested_unions.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "nested_unions.h:1:8",
                        declId = NamePair {
                          nameC = Name "exA",
                          nameHsIdent = Identifier "ExA"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["types/nested_unions.h"],
                            headerInclude =
                            "types/nested_unions.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "ExA"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_unions.h:5:11",
                                fieldName = NamePair {
                                  nameC = Name "fieldA1",
                                  nameHsIdent = Identifier
                                    "exA_fieldA1"},
                                fieldComment = Nothing},
                              structFieldType = TypeUnion
                                NamePair {
                                  nameC = Name "unionA",
                                  nameHsIdent = Identifier
                                    "UnionA"}
                                NameOriginInSource,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "exA",
                      commentLocation = Just
                        "nested_unions.h:1:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["types/nested_unions.h"],
                          headerInclude =
                          "types/nested_unions.h"},
                      commentChildren = []}}
                (Add 1)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "exA_fieldA1")
                      (Idx 2)
                      (Idx 0)])))},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "ExA"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "exA_fieldA1",
          hasCFieldInstanceCFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "UnionA"),
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
            (Name "@NsTypeConstr" "ExA"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "exA_fieldA1",
          hasFieldInstanceFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "UnionA"),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "ExB_fieldB1",
      newtypeConstr = Name
        "@NsConstr"
        "ExB_fieldB1",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_ExB_fieldB1",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "nested_unions.h:9:9",
          declId = NamePair {
            nameC = Name "exB_fieldB1",
            nameHsIdent = Identifier
              "ExB_fieldB1"},
          declOrigin = NameOriginGenerated
            (AnonId "nested_unions.h:9:9"),
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["types/nested_unions.h"],
              headerInclude =
              "types/nested_unions.h"},
          declComment = Nothing},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "ExB_fieldB1",
              newtypeField = Name
                "@NsVar"
                "un_ExB_fieldB1"},
            unionSizeof = 4,
            unionAlignment = 4,
            unionFields = [
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc =
                  "nested_unions.h:10:21",
                  fieldName = NamePair {
                    nameC = Name "a",
                    nameHsIdent = Identifier
                      "exB_fieldB1_a"},
                  fieldComment = Nothing},
                unionFieldType = TypePrim
                  (PrimIntegral PrimInt Signed)},
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc =
                  "nested_unions.h:11:22",
                  fieldName = NamePair {
                    nameC = Name "b",
                    nameHsIdent = Identifier
                      "exB_fieldB1_b"},
                  fieldComment = Nothing},
                unionFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))}]},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Just
            "nested_unions.h:9:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["types/nested_unions.h"],
              headerInclude =
              "types/nested_unions.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveVia
        (HsSizedByteArray 4 4),
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ExB_fieldB1",
      deriveInstanceComment =
      Nothing},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = Name
        "@NsVar"
        "get_exB_fieldB1_a",
      unionGetterType = HsPrimType
        HsPrimCInt,
      unionGetterConstr = Name
        "@NsTypeConstr"
        "ExB_fieldB1",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "a",
          commentLocation = Just
            "nested_unions.h:10:21",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["types/nested_unions.h"],
              headerInclude =
              "types/nested_unions.h"},
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_exB_fieldB1_a"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = Name
        "@NsVar"
        "set_exB_fieldB1_a",
      unionSetterType = HsPrimType
        HsPrimCInt,
      unionSetterConstr = Name
        "@NsTypeConstr"
        "ExB_fieldB1",
      unionSetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "get_exB_fieldB1_a"]]}},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = Name
        "@NsVar"
        "get_exB_fieldB1_b",
      unionGetterType = HsPrimType
        HsPrimCChar,
      unionGetterConstr = Name
        "@NsTypeConstr"
        "ExB_fieldB1",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "b",
          commentLocation = Just
            "nested_unions.h:11:22",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["types/nested_unions.h"],
              headerInclude =
              "types/nested_unions.h"},
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_exB_fieldB1_b"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = Name
        "@NsVar"
        "set_exB_fieldB1_b",
      unionSetterType = HsPrimType
        HsPrimCChar,
      unionSetterConstr = Name
        "@NsTypeConstr"
        "ExB_fieldB1",
      unionSetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "get_exB_fieldB1_b"]]}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "ExB_fieldB1"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "exB_fieldB1_a",
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
              "ExB_fieldB1"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "exB_fieldB1_a",
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
              "ExB_fieldB1"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "exB_fieldB1_b",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCChar,
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
              "ExB_fieldB1"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "exB_fieldB1_b",
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
        "ExB",
      structConstr = Name
        "@NsConstr"
        "ExB",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "exB_fieldB1",
          fieldType = HsTypRef
            (Name
              "@NsTypeConstr"
              "ExB_fieldB1"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "nested_unions.h:12:11",
                fieldName = NamePair {
                  nameC = Name "fieldB1",
                  nameHsIdent = Identifier
                    "exB_fieldB1"},
                fieldComment = Nothing},
              structFieldType = TypeUnion
                NamePair {
                  nameC = Name "exB_fieldB1",
                  nameHsIdent = Identifier
                    "ExB_fieldB1"}
                (NameOriginGenerated
                  (AnonId "nested_unions.h:9:9")),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "fieldB1",
              commentLocation = Just
                "nested_unions.h:12:11",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["types/nested_unions.h"],
                  headerInclude =
                  "types/nested_unions.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "nested_unions.h:8:8",
            declId = NamePair {
              nameC = Name "exB",
              nameHsIdent = Identifier "ExB"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["types/nested_unions.h"],
                headerInclude =
                "types/nested_unions.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "ExB"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_unions.h:12:11",
                    fieldName = NamePair {
                      nameC = Name "fieldB1",
                      nameHsIdent = Identifier
                        "exB_fieldB1"},
                    fieldComment = Nothing},
                  structFieldType = TypeUnion
                    NamePair {
                      nameC = Name "exB_fieldB1",
                      nameHsIdent = Identifier
                        "ExB_fieldB1"}
                    (NameOriginGenerated
                      (AnonId "nested_unions.h:9:9")),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec {
            declSpecC = Nothing,
            declSpecHs = Nothing}},
      structInstances = Set.fromList
        [Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "exB",
          commentLocation = Just
            "nested_unions.h:8:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["types/nested_unions.h"],
              headerInclude =
              "types/nested_unions.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "ExB",
          structConstr = Name
            "@NsConstr"
            "ExB",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "exB_fieldB1",
              fieldType = HsTypRef
                (Name
                  "@NsTypeConstr"
                  "ExB_fieldB1"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_unions.h:12:11",
                    fieldName = NamePair {
                      nameC = Name "fieldB1",
                      nameHsIdent = Identifier
                        "exB_fieldB1"},
                    fieldComment = Nothing},
                  structFieldType = TypeUnion
                    NamePair {
                      nameC = Name "exB_fieldB1",
                      nameHsIdent = Identifier
                        "ExB_fieldB1"}
                    (NameOriginGenerated
                      (AnonId "nested_unions.h:9:9")),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "fieldB1",
                  commentLocation = Just
                    "nested_unions.h:12:11",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["types/nested_unions.h"],
                      headerInclude =
                      "types/nested_unions.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "nested_unions.h:8:8",
                declId = NamePair {
                  nameC = Name "exB",
                  nameHsIdent = Identifier "ExB"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["types/nested_unions.h"],
                    headerInclude =
                    "types/nested_unions.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "ExB"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "nested_unions.h:12:11",
                        fieldName = NamePair {
                          nameC = Name "fieldB1",
                          nameHsIdent = Identifier
                            "exB_fieldB1"},
                        fieldComment = Nothing},
                      structFieldType = TypeUnion
                        NamePair {
                          nameC = Name "exB_fieldB1",
                          nameHsIdent = Identifier
                            "ExB_fieldB1"}
                        (NameOriginGenerated
                          (AnonId "nested_unions.h:9:9")),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec {
                declSpecC = Nothing,
                declSpecHs = Nothing}},
          structInstances = Set.fromList
            [Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "exB",
              commentLocation = Just
                "nested_unions.h:8:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["types/nested_unions.h"],
                  headerInclude =
                  "types/nested_unions.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 4,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "ExB",
                  structConstr = Name
                    "@NsConstr"
                    "ExB",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "exB_fieldB1",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "ExB_fieldB1"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_unions.h:12:11",
                            fieldName = NamePair {
                              nameC = Name "fieldB1",
                              nameHsIdent = Identifier
                                "exB_fieldB1"},
                            fieldComment = Nothing},
                          structFieldType = TypeUnion
                            NamePair {
                              nameC = Name "exB_fieldB1",
                              nameHsIdent = Identifier
                                "ExB_fieldB1"}
                            (NameOriginGenerated
                              (AnonId "nested_unions.h:9:9")),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "fieldB1",
                          commentLocation = Just
                            "nested_unions.h:12:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["types/nested_unions.h"],
                              headerInclude =
                              "types/nested_unions.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "nested_unions.h:8:8",
                        declId = NamePair {
                          nameC = Name "exB",
                          nameHsIdent = Identifier "ExB"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["types/nested_unions.h"],
                            headerInclude =
                            "types/nested_unions.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "ExB"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_unions.h:12:11",
                                fieldName = NamePair {
                                  nameC = Name "fieldB1",
                                  nameHsIdent = Identifier
                                    "exB_fieldB1"},
                                fieldComment = Nothing},
                              structFieldType = TypeUnion
                                NamePair {
                                  nameC = Name "exB_fieldB1",
                                  nameHsIdent = Identifier
                                    "ExB_fieldB1"}
                                (NameOriginGenerated
                                  (AnonId "nested_unions.h:9:9")),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "exB",
                      commentLocation = Just
                        "nested_unions.h:8:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["types/nested_unions.h"],
                          headerInclude =
                          "types/nested_unions.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "exB_fieldB1")
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
                    "ExB",
                  structConstr = Name
                    "@NsConstr"
                    "ExB",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "exB_fieldB1",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "ExB_fieldB1"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_unions.h:12:11",
                            fieldName = NamePair {
                              nameC = Name "fieldB1",
                              nameHsIdent = Identifier
                                "exB_fieldB1"},
                            fieldComment = Nothing},
                          structFieldType = TypeUnion
                            NamePair {
                              nameC = Name "exB_fieldB1",
                              nameHsIdent = Identifier
                                "ExB_fieldB1"}
                            (NameOriginGenerated
                              (AnonId "nested_unions.h:9:9")),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "fieldB1",
                          commentLocation = Just
                            "nested_unions.h:12:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["types/nested_unions.h"],
                              headerInclude =
                              "types/nested_unions.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "nested_unions.h:8:8",
                        declId = NamePair {
                          nameC = Name "exB",
                          nameHsIdent = Identifier "ExB"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["types/nested_unions.h"],
                            headerInclude =
                            "types/nested_unions.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "ExB"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "nested_unions.h:12:11",
                                fieldName = NamePair {
                                  nameC = Name "fieldB1",
                                  nameHsIdent = Identifier
                                    "exB_fieldB1"},
                                fieldComment = Nothing},
                              structFieldType = TypeUnion
                                NamePair {
                                  nameC = Name "exB_fieldB1",
                                  nameHsIdent = Identifier
                                    "ExB_fieldB1"}
                                (NameOriginGenerated
                                  (AnonId "nested_unions.h:9:9")),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "exB",
                      commentLocation = Just
                        "nested_unions.h:8:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["types/nested_unions.h"],
                          headerInclude =
                          "types/nested_unions.h"},
                      commentChildren = []}}
                (Add 1)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "exB_fieldB1")
                      (Idx 2)
                      (Idx 0)])))},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "ExB"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "exB_fieldB1",
          hasCFieldInstanceCFieldType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "ExB_fieldB1"),
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
            (Name "@NsTypeConstr" "ExB"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "exB_fieldB1",
          hasFieldInstanceFieldType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "ExB_fieldB1"),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing}]
