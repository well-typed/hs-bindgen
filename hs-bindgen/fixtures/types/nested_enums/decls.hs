[
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "EnumA",
      newtypeConstr = Name
        "@NsConstr"
        "EnumA",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_EnumA",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "nested_enums.h:2:14",
          declId = NamePair {
            nameC = Name "enumA",
            nameHsIdent = Identifier
              "EnumA"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["types/nested_enums.h"],
              headerInclude =
              "types/nested_enums.h"},
          declComment = Nothing},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "EnumA",
              newtypeField = Name
                "@NsVar"
                "un_EnumA"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "nested_enums.h:3:17",
                  fieldName = NamePair {
                    nameC = Name "VALA_1",
                    nameHsIdent = Identifier
                      "VALA_1"},
                  fieldComment = Nothing},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "nested_enums.h:4:17",
                  fieldName = NamePair {
                    nameC = Name "VALA_2",
                    nameHsIdent = Identifier
                      "VALA_2"},
                  fieldComment = Nothing},
                enumConstantValue = 1}]},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Read, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "enumA",
          commentLocation = Just
            "nested_enums.h:2:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["types/nested_enums.h"],
              headerInclude =
              "types/nested_enums.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "EnumA",
          structConstr = Name
            "@NsConstr"
            "EnumA",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_EnumA",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
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
                    "EnumA",
                  structConstr = Name
                    "@NsConstr"
                    "EnumA",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "un_EnumA",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = GeneratedField,
                      fieldComment = Nothing}],
                  structOrigin = Nothing,
                  structInstances = Set.fromList
                    [Eq, Ord, Read, Show, Storable],
                  structComment = Nothing})
              [PeekByteOff (Idx 0) 0]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "EnumA",
                  structConstr = Name
                    "@NsConstr"
                    "EnumA",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "un_EnumA",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = GeneratedField,
                      fieldComment = Nothing}],
                  structOrigin = Nothing,
                  structInstances = Set.fromList
                    [Eq, Ord, Read, Show, Storable],
                  structComment = Nothing}
                (Add 1)
                (Seq
                  [
                    PokeByteOff
                      (Idx 2)
                      0
                      (Idx 0)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "EnumA",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "EnumA",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
        Struct {
          structName = Name
            "@NsTypeConstr"
            "EnumA",
          structConstr = Name
            "@NsConstr"
            "EnumA",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_EnumA",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (HsPrimType HsPrimCUInt)
        (Map.fromList
          [
            _×_ 0 (NE.fromList ["VALA_1"]),
            _×_ 1 (NE.fromList ["VALA_2"])])
        True,
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceSequentialCEnum
        Struct {
          structName = Name
            "@NsTypeConstr"
            "EnumA",
          structConstr = Name
            "@NsConstr"
            "EnumA",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_EnumA",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (Name "@NsConstr" "VALA_1")
        (Name "@NsConstr" "VALA_2"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
        Struct {
          structName = Name
            "@NsTypeConstr"
            "EnumA",
          structConstr = Name
            "@NsConstr"
            "EnumA",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_EnumA",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumRead
        Struct {
          structName = Name
            "@NsTypeConstr"
            "EnumA",
          structConstr = Name
            "@NsConstr"
            "EnumA",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_EnumA",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing},
      defineInstanceComment =
      Nothing},
  DeclPatSyn
    PatSyn {
      patSynName = Name
        "@NsConstr"
        "VALA_1",
      patSynType = Name
        "@NsTypeConstr"
        "EnumA",
      patSynConstr = Name
        "@NsConstr"
        "EnumA",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "nested_enums.h:3:17",
            fieldName = NamePair {
              nameC = Name "VALA_1",
              nameHsIdent = Identifier
                "VALA_1"},
            fieldComment = Nothing},
          enumConstantValue = 0},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "VALA_1",
          commentLocation = Just
            "nested_enums.h:3:17",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["types/nested_enums.h"],
              headerInclude =
              "types/nested_enums.h"},
          commentChildren = []}},
  DeclPatSyn
    PatSyn {
      patSynName = Name
        "@NsConstr"
        "VALA_2",
      patSynType = Name
        "@NsTypeConstr"
        "EnumA",
      patSynConstr = Name
        "@NsConstr"
        "EnumA",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "nested_enums.h:4:17",
            fieldName = NamePair {
              nameC = Name "VALA_2",
              nameHsIdent = Identifier
                "VALA_2"},
            fieldComment = Nothing},
          enumConstantValue = 1},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "VALA_2",
          commentLocation = Just
            "nested_enums.h:4:17",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["types/nested_enums.h"],
              headerInclude =
              "types/nested_enums.h"},
          commentChildren = []}},
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
            (Name "@NsTypeConstr" "EnumA"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "nested_enums.h:5:11",
                fieldName = NamePair {
                  nameC = Name "fieldA1",
                  nameHsIdent = Identifier
                    "exA_fieldA1"},
                fieldComment = Nothing},
              structFieldType = TypeEnum
                NamePair {
                  nameC = Name "enumA",
                  nameHsIdent = Identifier
                    "EnumA"}
                NameOriginInSource,
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "fieldA1",
              commentLocation = Just
                "nested_enums.h:5:11",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["types/nested_enums.h"],
                  headerInclude =
                  "types/nested_enums.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "nested_enums.h:1:8",
            declId = NamePair {
              nameC = Name "exA",
              nameHsIdent = Identifier "ExA"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["types/nested_enums.h"],
                headerInclude =
                "types/nested_enums.h"},
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
                    "nested_enums.h:5:11",
                    fieldName = NamePair {
                      nameC = Name "fieldA1",
                      nameHsIdent = Identifier
                        "exA_fieldA1"},
                    fieldComment = Nothing},
                  structFieldType = TypeEnum
                    NamePair {
                      nameC = Name "enumA",
                      nameHsIdent = Identifier
                        "EnumA"}
                    NameOriginInSource,
                  structFieldOffset = 0,
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
          commentOrigin = Just "exA",
          commentLocation = Just
            "nested_enums.h:1:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["types/nested_enums.h"],
              headerInclude =
              "types/nested_enums.h"},
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
                (Name "@NsTypeConstr" "EnumA"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "nested_enums.h:5:11",
                    fieldName = NamePair {
                      nameC = Name "fieldA1",
                      nameHsIdent = Identifier
                        "exA_fieldA1"},
                    fieldComment = Nothing},
                  structFieldType = TypeEnum
                    NamePair {
                      nameC = Name "enumA",
                      nameHsIdent = Identifier
                        "EnumA"}
                    NameOriginInSource,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "fieldA1",
                  commentLocation = Just
                    "nested_enums.h:5:11",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["types/nested_enums.h"],
                      headerInclude =
                      "types/nested_enums.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "nested_enums.h:1:8",
                declId = NamePair {
                  nameC = Name "exA",
                  nameHsIdent = Identifier "ExA"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["types/nested_enums.h"],
                    headerInclude =
                    "types/nested_enums.h"},
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
                        "nested_enums.h:5:11",
                        fieldName = NamePair {
                          nameC = Name "fieldA1",
                          nameHsIdent = Identifier
                            "exA_fieldA1"},
                        fieldComment = Nothing},
                      structFieldType = TypeEnum
                        NamePair {
                          nameC = Name "enumA",
                          nameHsIdent = Identifier
                            "EnumA"}
                        NameOriginInSource,
                      structFieldOffset = 0,
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
              commentOrigin = Just "exA",
              commentLocation = Just
                "nested_enums.h:1:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["types/nested_enums.h"],
                  headerInclude =
                  "types/nested_enums.h"},
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
                        (Name "@NsTypeConstr" "EnumA"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_enums.h:5:11",
                            fieldName = NamePair {
                              nameC = Name "fieldA1",
                              nameHsIdent = Identifier
                                "exA_fieldA1"},
                            fieldComment = Nothing},
                          structFieldType = TypeEnum
                            NamePair {
                              nameC = Name "enumA",
                              nameHsIdent = Identifier
                                "EnumA"}
                            NameOriginInSource,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "fieldA1",
                          commentLocation = Just
                            "nested_enums.h:5:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["types/nested_enums.h"],
                              headerInclude =
                              "types/nested_enums.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "nested_enums.h:1:8",
                        declId = NamePair {
                          nameC = Name "exA",
                          nameHsIdent = Identifier "ExA"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["types/nested_enums.h"],
                            headerInclude =
                            "types/nested_enums.h"},
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
                                "nested_enums.h:5:11",
                                fieldName = NamePair {
                                  nameC = Name "fieldA1",
                                  nameHsIdent = Identifier
                                    "exA_fieldA1"},
                                fieldComment = Nothing},
                              structFieldType = TypeEnum
                                NamePair {
                                  nameC = Name "enumA",
                                  nameHsIdent = Identifier
                                    "EnumA"}
                                NameOriginInSource,
                              structFieldOffset = 0,
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
                      commentOrigin = Just "exA",
                      commentLocation = Just
                        "nested_enums.h:1:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["types/nested_enums.h"],
                          headerInclude =
                          "types/nested_enums.h"},
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
                        (Name "@NsTypeConstr" "EnumA"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "nested_enums.h:5:11",
                            fieldName = NamePair {
                              nameC = Name "fieldA1",
                              nameHsIdent = Identifier
                                "exA_fieldA1"},
                            fieldComment = Nothing},
                          structFieldType = TypeEnum
                            NamePair {
                              nameC = Name "enumA",
                              nameHsIdent = Identifier
                                "EnumA"}
                            NameOriginInSource,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "fieldA1",
                          commentLocation = Just
                            "nested_enums.h:5:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["types/nested_enums.h"],
                              headerInclude =
                              "types/nested_enums.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "nested_enums.h:1:8",
                        declId = NamePair {
                          nameC = Name "exA",
                          nameHsIdent = Identifier "ExA"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["types/nested_enums.h"],
                            headerInclude =
                            "types/nested_enums.h"},
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
                                "nested_enums.h:5:11",
                                fieldName = NamePair {
                                  nameC = Name "fieldA1",
                                  nameHsIdent = Identifier
                                    "exA_fieldA1"},
                                fieldComment = Nothing},
                              structFieldType = TypeEnum
                                NamePair {
                                  nameC = Name "enumA",
                                  nameHsIdent = Identifier
                                    "EnumA"}
                                NameOriginInSource,
                              structFieldOffset = 0,
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
                      commentOrigin = Just "exA",
                      commentLocation = Just
                        "nested_enums.h:1:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["types/nested_enums.h"],
                          headerInclude =
                          "types/nested_enums.h"},
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
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ExA",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ExA",
      deriveInstanceComment =
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
            (Name "@NsTypeConstr" "EnumA"),
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
            (Name "@NsTypeConstr" "EnumA"),
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
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "nested_enums.h:9:9",
          declId = NamePair {
            nameC = Name "exB_fieldB1",
            nameHsIdent = Identifier
              "ExB_fieldB1"},
          declOrigin = NameOriginGenerated
            (AnonId "nested_enums.h:9:9"),
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["types/nested_enums.h"],
              headerInclude =
              "types/nested_enums.h"},
          declComment = Nothing},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "ExB_fieldB1",
              newtypeField = Name
                "@NsVar"
                "un_ExB_fieldB1"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "nested_enums.h:10:17",
                  fieldName = NamePair {
                    nameC = Name "VALB_1",
                    nameHsIdent = Identifier
                      "VALB_1"},
                  fieldComment = Nothing},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "nested_enums.h:11:17",
                  fieldName = NamePair {
                    nameC = Name "VALB_2",
                    nameHsIdent = Identifier
                      "VALB_2"},
                  fieldComment = Nothing},
                enumConstantValue = 1}]},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Read, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Just
            "nested_enums.h:9:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["types/nested_enums.h"],
              headerInclude =
              "types/nested_enums.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "ExB_fieldB1",
          structConstr = Name
            "@NsConstr"
            "ExB_fieldB1",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_ExB_fieldB1",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
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
                    "ExB_fieldB1",
                  structConstr = Name
                    "@NsConstr"
                    "ExB_fieldB1",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "un_ExB_fieldB1",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = GeneratedField,
                      fieldComment = Nothing}],
                  structOrigin = Nothing,
                  structInstances = Set.fromList
                    [Eq, Ord, Read, Show, Storable],
                  structComment = Nothing})
              [PeekByteOff (Idx 0) 0]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "ExB_fieldB1",
                  structConstr = Name
                    "@NsConstr"
                    "ExB_fieldB1",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "un_ExB_fieldB1",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = GeneratedField,
                      fieldComment = Nothing}],
                  structOrigin = Nothing,
                  structInstances = Set.fromList
                    [Eq, Ord, Read, Show, Storable],
                  structComment = Nothing}
                (Add 1)
                (Seq
                  [
                    PokeByteOff
                      (Idx 2)
                      0
                      (Idx 0)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ExB_fieldB1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ExB_fieldB1",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
        Struct {
          structName = Name
            "@NsTypeConstr"
            "ExB_fieldB1",
          structConstr = Name
            "@NsConstr"
            "ExB_fieldB1",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_ExB_fieldB1",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (HsPrimType HsPrimCUInt)
        (Map.fromList
          [
            _×_ 0 (NE.fromList ["VALB_1"]),
            _×_ 1 (NE.fromList ["VALB_2"])])
        True,
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceSequentialCEnum
        Struct {
          structName = Name
            "@NsTypeConstr"
            "ExB_fieldB1",
          structConstr = Name
            "@NsConstr"
            "ExB_fieldB1",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_ExB_fieldB1",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (Name "@NsConstr" "VALB_1")
        (Name "@NsConstr" "VALB_2"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
        Struct {
          structName = Name
            "@NsTypeConstr"
            "ExB_fieldB1",
          structConstr = Name
            "@NsConstr"
            "ExB_fieldB1",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_ExB_fieldB1",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumRead
        Struct {
          structName = Name
            "@NsTypeConstr"
            "ExB_fieldB1",
          structConstr = Name
            "@NsConstr"
            "ExB_fieldB1",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_ExB_fieldB1",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing},
      defineInstanceComment =
      Nothing},
  DeclPatSyn
    PatSyn {
      patSynName = Name
        "@NsConstr"
        "VALB_1",
      patSynType = Name
        "@NsTypeConstr"
        "ExB_fieldB1",
      patSynConstr = Name
        "@NsConstr"
        "ExB_fieldB1",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "nested_enums.h:10:17",
            fieldName = NamePair {
              nameC = Name "VALB_1",
              nameHsIdent = Identifier
                "VALB_1"},
            fieldComment = Nothing},
          enumConstantValue = 0},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "VALB_1",
          commentLocation = Just
            "nested_enums.h:10:17",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["types/nested_enums.h"],
              headerInclude =
              "types/nested_enums.h"},
          commentChildren = []}},
  DeclPatSyn
    PatSyn {
      patSynName = Name
        "@NsConstr"
        "VALB_2",
      patSynType = Name
        "@NsTypeConstr"
        "ExB_fieldB1",
      patSynConstr = Name
        "@NsConstr"
        "ExB_fieldB1",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "nested_enums.h:11:17",
            fieldName = NamePair {
              nameC = Name "VALB_2",
              nameHsIdent = Identifier
                "VALB_2"},
            fieldComment = Nothing},
          enumConstantValue = 1},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "VALB_2",
          commentLocation = Just
            "nested_enums.h:11:17",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["types/nested_enums.h"],
              headerInclude =
              "types/nested_enums.h"},
          commentChildren = []}},
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
                "nested_enums.h:12:11",
                fieldName = NamePair {
                  nameC = Name "fieldB1",
                  nameHsIdent = Identifier
                    "exB_fieldB1"},
                fieldComment = Nothing},
              structFieldType = TypeEnum
                NamePair {
                  nameC = Name "exB_fieldB1",
                  nameHsIdent = Identifier
                    "ExB_fieldB1"}
                (NameOriginGenerated
                  (AnonId "nested_enums.h:9:9")),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "fieldB1",
              commentLocation = Just
                "nested_enums.h:12:11",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["types/nested_enums.h"],
                  headerInclude =
                  "types/nested_enums.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "nested_enums.h:8:8",
            declId = NamePair {
              nameC = Name "exB",
              nameHsIdent = Identifier "ExB"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["types/nested_enums.h"],
                headerInclude =
                "types/nested_enums.h"},
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
                    "nested_enums.h:12:11",
                    fieldName = NamePair {
                      nameC = Name "fieldB1",
                      nameHsIdent = Identifier
                        "exB_fieldB1"},
                    fieldComment = Nothing},
                  structFieldType = TypeEnum
                    NamePair {
                      nameC = Name "exB_fieldB1",
                      nameHsIdent = Identifier
                        "ExB_fieldB1"}
                    (NameOriginGenerated
                      (AnonId "nested_enums.h:9:9")),
                  structFieldOffset = 0,
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
          commentOrigin = Just "exB",
          commentLocation = Just
            "nested_enums.h:8:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["types/nested_enums.h"],
              headerInclude =
              "types/nested_enums.h"},
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
                    "nested_enums.h:12:11",
                    fieldName = NamePair {
                      nameC = Name "fieldB1",
                      nameHsIdent = Identifier
                        "exB_fieldB1"},
                    fieldComment = Nothing},
                  structFieldType = TypeEnum
                    NamePair {
                      nameC = Name "exB_fieldB1",
                      nameHsIdent = Identifier
                        "ExB_fieldB1"}
                    (NameOriginGenerated
                      (AnonId "nested_enums.h:9:9")),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "fieldB1",
                  commentLocation = Just
                    "nested_enums.h:12:11",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["types/nested_enums.h"],
                      headerInclude =
                      "types/nested_enums.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "nested_enums.h:8:8",
                declId = NamePair {
                  nameC = Name "exB",
                  nameHsIdent = Identifier "ExB"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["types/nested_enums.h"],
                    headerInclude =
                    "types/nested_enums.h"},
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
                        "nested_enums.h:12:11",
                        fieldName = NamePair {
                          nameC = Name "fieldB1",
                          nameHsIdent = Identifier
                            "exB_fieldB1"},
                        fieldComment = Nothing},
                      structFieldType = TypeEnum
                        NamePair {
                          nameC = Name "exB_fieldB1",
                          nameHsIdent = Identifier
                            "ExB_fieldB1"}
                        (NameOriginGenerated
                          (AnonId "nested_enums.h:9:9")),
                      structFieldOffset = 0,
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
              commentOrigin = Just "exB",
              commentLocation = Just
                "nested_enums.h:8:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["types/nested_enums.h"],
                  headerInclude =
                  "types/nested_enums.h"},
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
                            "nested_enums.h:12:11",
                            fieldName = NamePair {
                              nameC = Name "fieldB1",
                              nameHsIdent = Identifier
                                "exB_fieldB1"},
                            fieldComment = Nothing},
                          structFieldType = TypeEnum
                            NamePair {
                              nameC = Name "exB_fieldB1",
                              nameHsIdent = Identifier
                                "ExB_fieldB1"}
                            (NameOriginGenerated
                              (AnonId "nested_enums.h:9:9")),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "fieldB1",
                          commentLocation = Just
                            "nested_enums.h:12:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["types/nested_enums.h"],
                              headerInclude =
                              "types/nested_enums.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "nested_enums.h:8:8",
                        declId = NamePair {
                          nameC = Name "exB",
                          nameHsIdent = Identifier "ExB"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["types/nested_enums.h"],
                            headerInclude =
                            "types/nested_enums.h"},
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
                                "nested_enums.h:12:11",
                                fieldName = NamePair {
                                  nameC = Name "fieldB1",
                                  nameHsIdent = Identifier
                                    "exB_fieldB1"},
                                fieldComment = Nothing},
                              structFieldType = TypeEnum
                                NamePair {
                                  nameC = Name "exB_fieldB1",
                                  nameHsIdent = Identifier
                                    "ExB_fieldB1"}
                                (NameOriginGenerated
                                  (AnonId "nested_enums.h:9:9")),
                              structFieldOffset = 0,
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
                      commentOrigin = Just "exB",
                      commentLocation = Just
                        "nested_enums.h:8:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["types/nested_enums.h"],
                          headerInclude =
                          "types/nested_enums.h"},
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
                            "nested_enums.h:12:11",
                            fieldName = NamePair {
                              nameC = Name "fieldB1",
                              nameHsIdent = Identifier
                                "exB_fieldB1"},
                            fieldComment = Nothing},
                          structFieldType = TypeEnum
                            NamePair {
                              nameC = Name "exB_fieldB1",
                              nameHsIdent = Identifier
                                "ExB_fieldB1"}
                            (NameOriginGenerated
                              (AnonId "nested_enums.h:9:9")),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "fieldB1",
                          commentLocation = Just
                            "nested_enums.h:12:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["types/nested_enums.h"],
                              headerInclude =
                              "types/nested_enums.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "nested_enums.h:8:8",
                        declId = NamePair {
                          nameC = Name "exB",
                          nameHsIdent = Identifier "ExB"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["types/nested_enums.h"],
                            headerInclude =
                            "types/nested_enums.h"},
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
                                "nested_enums.h:12:11",
                                fieldName = NamePair {
                                  nameC = Name "fieldB1",
                                  nameHsIdent = Identifier
                                    "exB_fieldB1"},
                                fieldComment = Nothing},
                              structFieldType = TypeEnum
                                NamePair {
                                  nameC = Name "exB_fieldB1",
                                  nameHsIdent = Identifier
                                    "ExB_fieldB1"}
                                (NameOriginGenerated
                                  (AnonId "nested_enums.h:9:9")),
                              structFieldOffset = 0,
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
                      commentOrigin = Just "exB",
                      commentLocation = Just
                        "nested_enums.h:8:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["types/nested_enums.h"],
                          headerInclude =
                          "types/nested_enums.h"},
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
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ExB",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ExB",
      deriveInstanceComment =
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
