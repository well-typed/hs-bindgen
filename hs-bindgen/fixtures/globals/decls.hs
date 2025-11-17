[
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Config",
      structConstr = Name
        "@NsConstr"
        "Config",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "config_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:13:7",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "config_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Just
                "globals.h:13:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["globals.h"],
                  headerInclude = "globals.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "config_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:14:7",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier
                    "config_y"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Just
                "globals.h:14:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["globals.h"],
                  headerInclude = "globals.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "globals.h:12:8",
            declId = NamePair {
              nameC = Name "config",
              nameHsIdent = Identifier
                "Config"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["globals.h"],
                headerInclude = "globals.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Config"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:13:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "config_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:14:7",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "config_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
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
          commentOrigin = Just "config",
          commentLocation = Just
            "globals.h:12:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["globals.h"],
              headerInclude = "globals.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Config",
          structConstr = Name
            "@NsConstr"
            "Config",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "config_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:13:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "config_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "x",
                  commentLocation = Just
                    "globals.h:13:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["globals.h"],
                      headerInclude = "globals.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "config_y",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:14:7",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "config_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "y",
                  commentLocation = Just
                    "globals.h:14:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["globals.h"],
                      headerInclude = "globals.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "globals.h:12:8",
                declId = NamePair {
                  nameC = Name "config",
                  nameHsIdent = Identifier
                    "Config"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["globals.h"],
                    headerInclude = "globals.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Config"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:13:7",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "config_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:14:7",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = Identifier
                            "config_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
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
              commentOrigin = Just "config",
              commentLocation = Just
                "globals.h:12:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["globals.h"],
                  headerInclude = "globals.h"},
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
                    "Config",
                  structConstr = Name
                    "@NsConstr"
                    "Config",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "config_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:13:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "config_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "globals.h:13:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "config_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:14:7",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "config_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "globals.h:14:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:12:8",
                        declId = NamePair {
                          nameC = Name "config",
                          nameHsIdent = Identifier
                            "Config"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["globals.h"],
                            headerInclude = "globals.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Config"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:13:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "config_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:14:7",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "config_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
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
                      commentOrigin = Just "config",
                      commentLocation = Just
                        "globals.h:12:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["globals.h"],
                          headerInclude = "globals.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "config_x")
                  (Idx 0),
                PeekCField
                  (HsStrLit "config_y")
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
                    "Config",
                  structConstr = Name
                    "@NsConstr"
                    "Config",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "config_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:13:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "config_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "globals.h:13:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "config_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:14:7",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "config_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "globals.h:14:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:12:8",
                        declId = NamePair {
                          nameC = Name "config",
                          nameHsIdent = Identifier
                            "Config"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["globals.h"],
                            headerInclude = "globals.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Config"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:13:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "config_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:14:7",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "config_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
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
                      commentOrigin = Just "config",
                      commentLocation = Just
                        "globals.h:12:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["globals.h"],
                          headerInclude = "globals.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "config_x")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "config_y")
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
        "Config",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Config",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Config"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "config_x",
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
            (Name "@NsTypeConstr" "Config"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "config_x",
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
            (Name "@NsTypeConstr" "Config"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "config_y",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCInt,
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
            (Name "@NsTypeConstr" "Config"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "config_y",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Inline_struct",
      structConstr = Name
        "@NsConstr"
        "Inline_struct",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "inline_struct_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:19:35",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "inline_struct_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Just
                "globals.h:19:35",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["globals.h"],
                  headerInclude = "globals.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "inline_struct_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:19:42",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier
                    "inline_struct_y"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Just
                "globals.h:19:42",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["globals.h"],
                  headerInclude = "globals.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "globals.h:19:15",
            declId = NamePair {
              nameC = Name "inline_struct",
              nameHsIdent = Identifier
                "Inline_struct"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["globals.h"],
                headerInclude = "globals.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name
                  "@NsConstr"
                  "Inline_struct"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:19:35",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "inline_struct_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:19:42",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "inline_struct_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
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
          commentOrigin = Just
            "inline_struct",
          commentLocation = Just
            "globals.h:19:15",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["globals.h"],
              headerInclude = "globals.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Inline_struct",
          structConstr = Name
            "@NsConstr"
            "Inline_struct",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "inline_struct_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:19:35",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "inline_struct_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "x",
                  commentLocation = Just
                    "globals.h:19:35",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["globals.h"],
                      headerInclude = "globals.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "inline_struct_y",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:19:42",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "inline_struct_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "y",
                  commentLocation = Just
                    "globals.h:19:42",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["globals.h"],
                      headerInclude = "globals.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "globals.h:19:15",
                declId = NamePair {
                  nameC = Name "inline_struct",
                  nameHsIdent = Identifier
                    "Inline_struct"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["globals.h"],
                    headerInclude = "globals.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name
                      "@NsConstr"
                      "Inline_struct"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:19:35",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "inline_struct_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:19:42",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = Identifier
                            "inline_struct_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
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
              commentOrigin = Just
                "inline_struct",
              commentLocation = Just
                "globals.h:19:15",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["globals.h"],
                  headerInclude = "globals.h"},
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
                    "Inline_struct",
                  structConstr = Name
                    "@NsConstr"
                    "Inline_struct",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "inline_struct_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:19:35",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "inline_struct_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "globals.h:19:35",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "inline_struct_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:19:42",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "inline_struct_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "globals.h:19:42",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:19:15",
                        declId = NamePair {
                          nameC = Name "inline_struct",
                          nameHsIdent = Identifier
                            "Inline_struct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["globals.h"],
                            headerInclude = "globals.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Inline_struct"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:19:35",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "inline_struct_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:19:42",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "inline_struct_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
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
                      commentOrigin = Just
                        "inline_struct",
                      commentLocation = Just
                        "globals.h:19:15",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["globals.h"],
                          headerInclude = "globals.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "inline_struct_x")
                  (Idx 0),
                PeekCField
                  (HsStrLit "inline_struct_y")
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
                    "Inline_struct",
                  structConstr = Name
                    "@NsConstr"
                    "Inline_struct",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "inline_struct_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:19:35",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "inline_struct_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "globals.h:19:35",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "inline_struct_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:19:42",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "inline_struct_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "globals.h:19:42",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:19:15",
                        declId = NamePair {
                          nameC = Name "inline_struct",
                          nameHsIdent = Identifier
                            "Inline_struct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["globals.h"],
                            headerInclude = "globals.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Inline_struct"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:19:35",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "inline_struct_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:19:42",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "inline_struct_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
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
                      commentOrigin = Just
                        "inline_struct",
                      commentLocation = Just
                        "globals.h:19:15",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["globals.h"],
                          headerInclude = "globals.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "inline_struct_x")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "inline_struct_y")
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
        "Inline_struct",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Inline_struct",
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
              "Inline_struct"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "inline_struct_x",
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
              "Inline_struct"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "inline_struct_x",
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
              "Inline_struct"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "inline_struct_y",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCInt,
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
              "Inline_struct"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "inline_struct_y",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Version_t",
      structConstr = Name
        "@NsConstr"
        "Version_t",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "version_t_major",
          fieldType = HsExtBinding
            ExtRef {
              extRefModule = ModuleName
                "HsBindgen.Runtime.Prelude",
              extRefIdentifier = Identifier
                "Word8"}
            (CTypeSpec
              (Just (Identifier "Word8")))
            (Just
              (HsTypeSpec
                (Map.fromList
                  [
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]))),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:408:12",
                fieldName = NamePair {
                  nameC = Name "major",
                  nameHsIdent = Identifier
                    "version_t_major"},
                fieldComment = Nothing},
              structFieldType = TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "uint8_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtRef {
                    extRefModule = ModuleName
                      "HsBindgen.Runtime.Prelude",
                    extRefIdentifier = Identifier
                      "Word8"},
                  extCSpec = CTypeSpec
                    (Just (Identifier "Word8")),
                  extHsSpec = Just
                    (HsTypeSpec
                      (Map.fromList
                        [
                          _×_
                            Bits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bounded
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Enum
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Eq
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            FiniteBits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Integral
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ix
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Num
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ord
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Read
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            ReadRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Real
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Show
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            StaticSize
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Storable
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            WriteRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = [
                                  ]})]))},
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "major",
              commentLocation = Just
                "globals.h:408:12",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["globals.h"],
                  headerInclude = "globals.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "version_t_minor",
          fieldType = HsExtBinding
            ExtRef {
              extRefModule = ModuleName
                "HsBindgen.Runtime.Prelude",
              extRefIdentifier = Identifier
                "Word16"}
            (CTypeSpec
              (Just (Identifier "Word16")))
            (Just
              (HsTypeSpec
                (Map.fromList
                  [
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]))),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:409:12",
                fieldName = NamePair {
                  nameC = Name "minor",
                  nameHsIdent = Identifier
                    "version_t_minor"},
                fieldComment = Nothing},
              structFieldType = TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "uint16_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtRef {
                    extRefModule = ModuleName
                      "HsBindgen.Runtime.Prelude",
                    extRefIdentifier = Identifier
                      "Word16"},
                  extCSpec = CTypeSpec
                    (Just (Identifier "Word16")),
                  extHsSpec = Just
                    (HsTypeSpec
                      (Map.fromList
                        [
                          _×_
                            Bits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bounded
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Enum
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Eq
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            FiniteBits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Integral
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ix
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Num
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ord
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Read
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            ReadRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Real
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Show
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            StaticSize
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Storable
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            WriteRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = [
                                  ]})]))},
              structFieldOffset = 16,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "minor",
              commentLocation = Just
                "globals.h:409:12",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["globals.h"],
                  headerInclude = "globals.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "version_t_patch",
          fieldType = HsExtBinding
            ExtRef {
              extRefModule = ModuleName
                "HsBindgen.Runtime.Prelude",
              extRefIdentifier = Identifier
                "Word8"}
            (CTypeSpec
              (Just (Identifier "Word8")))
            (Just
              (HsTypeSpec
                (Map.fromList
                  [
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]))),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:410:12",
                fieldName = NamePair {
                  nameC = Name "patch",
                  nameHsIdent = Identifier
                    "version_t_patch"},
                fieldComment = Nothing},
              structFieldType = TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "uint8_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtRef {
                    extRefModule = ModuleName
                      "HsBindgen.Runtime.Prelude",
                    extRefIdentifier = Identifier
                      "Word8"},
                  extCSpec = CTypeSpec
                    (Just (Identifier "Word8")),
                  extHsSpec = Just
                    (HsTypeSpec
                      (Map.fromList
                        [
                          _×_
                            Bits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bounded
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Enum
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Eq
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            FiniteBits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Integral
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ix
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Num
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ord
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Read
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            ReadRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Real
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Show
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            StaticSize
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Storable
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            WriteRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = [
                                  ]})]))},
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "patch",
              commentLocation = Just
                "globals.h:410:12",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["globals.h"],
                  headerInclude = "globals.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "globals.h:406:9",
            declId = NamePair {
              nameC = Name "version_t",
              nameHsIdent = Identifier
                "Version_t"},
            declOrigin = NameOriginGenerated
              (AnonId "globals.h:406:9"),
            declAliases = [
              Name "version_t"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["globals.h"],
                headerInclude = "globals.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Version_t"),
              structSizeof = 6,
              structAlignment = 2,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:408:12",
                    fieldName = NamePair {
                      nameC = Name "major",
                      nameHsIdent = Identifier
                        "version_t_major"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint8_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtRef {
                        extRefModule = ModuleName
                          "HsBindgen.Runtime.Prelude",
                        extRefIdentifier = Identifier
                          "Word8"},
                      extCSpec = CTypeSpec
                        (Just (Identifier "Word8")),
                      extHsSpec = Just
                        (HsTypeSpec
                          (Map.fromList
                            [
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]))},
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:409:12",
                    fieldName = NamePair {
                      nameC = Name "minor",
                      nameHsIdent = Identifier
                        "version_t_minor"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint16_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtRef {
                        extRefModule = ModuleName
                          "HsBindgen.Runtime.Prelude",
                        extRefIdentifier = Identifier
                          "Word16"},
                      extCSpec = CTypeSpec
                        (Just (Identifier "Word16")),
                      extHsSpec = Just
                        (HsTypeSpec
                          (Map.fromList
                            [
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]))},
                  structFieldOffset = 16,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:410:12",
                    fieldName = NamePair {
                      nameC = Name "patch",
                      nameHsIdent = Identifier
                        "version_t_patch"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint8_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtRef {
                        extRefModule = ModuleName
                          "HsBindgen.Runtime.Prelude",
                        extRefIdentifier = Identifier
                          "Word8"},
                      extCSpec = CTypeSpec
                        (Just (Identifier "Word8")),
                      extHsSpec = Just
                        (HsTypeSpec
                          (Map.fromList
                            [
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]))},
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
            "globals.h:406:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["globals.h"],
              headerInclude = "globals.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Version_t",
          structConstr = Name
            "@NsConstr"
            "Version_t",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "version_t_major",
              fieldType = HsExtBinding
                ExtRef {
                  extRefModule = ModuleName
                    "HsBindgen.Runtime.Prelude",
                  extRefIdentifier = Identifier
                    "Word8"}
                (CTypeSpec
                  (Just (Identifier "Word8")))
                (Just
                  (HsTypeSpec
                    (Map.fromList
                      [
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]))),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:408:12",
                    fieldName = NamePair {
                      nameC = Name "major",
                      nameHsIdent = Identifier
                        "version_t_major"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint8_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtRef {
                        extRefModule = ModuleName
                          "HsBindgen.Runtime.Prelude",
                        extRefIdentifier = Identifier
                          "Word8"},
                      extCSpec = CTypeSpec
                        (Just (Identifier "Word8")),
                      extHsSpec = Just
                        (HsTypeSpec
                          (Map.fromList
                            [
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]))},
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "major",
                  commentLocation = Just
                    "globals.h:408:12",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["globals.h"],
                      headerInclude = "globals.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "version_t_minor",
              fieldType = HsExtBinding
                ExtRef {
                  extRefModule = ModuleName
                    "HsBindgen.Runtime.Prelude",
                  extRefIdentifier = Identifier
                    "Word16"}
                (CTypeSpec
                  (Just (Identifier "Word16")))
                (Just
                  (HsTypeSpec
                    (Map.fromList
                      [
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]))),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:409:12",
                    fieldName = NamePair {
                      nameC = Name "minor",
                      nameHsIdent = Identifier
                        "version_t_minor"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint16_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtRef {
                        extRefModule = ModuleName
                          "HsBindgen.Runtime.Prelude",
                        extRefIdentifier = Identifier
                          "Word16"},
                      extCSpec = CTypeSpec
                        (Just (Identifier "Word16")),
                      extHsSpec = Just
                        (HsTypeSpec
                          (Map.fromList
                            [
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]))},
                  structFieldOffset = 16,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "minor",
                  commentLocation = Just
                    "globals.h:409:12",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["globals.h"],
                      headerInclude = "globals.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "version_t_patch",
              fieldType = HsExtBinding
                ExtRef {
                  extRefModule = ModuleName
                    "HsBindgen.Runtime.Prelude",
                  extRefIdentifier = Identifier
                    "Word8"}
                (CTypeSpec
                  (Just (Identifier "Word8")))
                (Just
                  (HsTypeSpec
                    (Map.fromList
                      [
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]))),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:410:12",
                    fieldName = NamePair {
                      nameC = Name "patch",
                      nameHsIdent = Identifier
                        "version_t_patch"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint8_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtRef {
                        extRefModule = ModuleName
                          "HsBindgen.Runtime.Prelude",
                        extRefIdentifier = Identifier
                          "Word8"},
                      extCSpec = CTypeSpec
                        (Just (Identifier "Word8")),
                      extHsSpec = Just
                        (HsTypeSpec
                          (Map.fromList
                            [
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]))},
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "patch",
                  commentLocation = Just
                    "globals.h:410:12",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["globals.h"],
                      headerInclude = "globals.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "globals.h:406:9",
                declId = NamePair {
                  nameC = Name "version_t",
                  nameHsIdent = Identifier
                    "Version_t"},
                declOrigin = NameOriginGenerated
                  (AnonId "globals.h:406:9"),
                declAliases = [
                  Name "version_t"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["globals.h"],
                    headerInclude = "globals.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Version_t"),
                  structSizeof = 6,
                  structAlignment = 2,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:408:12",
                        fieldName = NamePair {
                          nameC = Name "major",
                          nameHsIdent = Identifier
                            "version_t_major"},
                        fieldComment = Nothing},
                      structFieldType = TypeExtBinding
                        ResolvedExtBinding {
                          extCName = QualName {
                            qualNameName = Name "uint8_t",
                            qualNameKind =
                            NameKindOrdinary},
                          extHsRef = ExtRef {
                            extRefModule = ModuleName
                              "HsBindgen.Runtime.Prelude",
                            extRefIdentifier = Identifier
                              "Word8"},
                          extCSpec = CTypeSpec
                            (Just (Identifier "Word8")),
                          extHsSpec = Just
                            (HsTypeSpec
                              (Map.fromList
                                [
                                  _×_
                                    Bits
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Bounded
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Enum
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Eq
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    FiniteBits
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Integral
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Ix
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Num
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Ord
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Read
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    ReadRaw
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Real
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Show
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    StaticSize
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Storable
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    WriteRaw
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = [
                                          ]})]))},
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:409:12",
                        fieldName = NamePair {
                          nameC = Name "minor",
                          nameHsIdent = Identifier
                            "version_t_minor"},
                        fieldComment = Nothing},
                      structFieldType = TypeExtBinding
                        ResolvedExtBinding {
                          extCName = QualName {
                            qualNameName = Name "uint16_t",
                            qualNameKind =
                            NameKindOrdinary},
                          extHsRef = ExtRef {
                            extRefModule = ModuleName
                              "HsBindgen.Runtime.Prelude",
                            extRefIdentifier = Identifier
                              "Word16"},
                          extCSpec = CTypeSpec
                            (Just (Identifier "Word16")),
                          extHsSpec = Just
                            (HsTypeSpec
                              (Map.fromList
                                [
                                  _×_
                                    Bits
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Bounded
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Enum
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Eq
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    FiniteBits
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Integral
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Ix
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Num
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Ord
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Read
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    ReadRaw
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Real
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Show
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    StaticSize
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Storable
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    WriteRaw
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = [
                                          ]})]))},
                      structFieldOffset = 16,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:410:12",
                        fieldName = NamePair {
                          nameC = Name "patch",
                          nameHsIdent = Identifier
                            "version_t_patch"},
                        fieldComment = Nothing},
                      structFieldType = TypeExtBinding
                        ResolvedExtBinding {
                          extCName = QualName {
                            qualNameName = Name "uint8_t",
                            qualNameKind =
                            NameKindOrdinary},
                          extHsRef = ExtRef {
                            extRefModule = ModuleName
                              "HsBindgen.Runtime.Prelude",
                            extRefIdentifier = Identifier
                              "Word8"},
                          extCSpec = CTypeSpec
                            (Just (Identifier "Word8")),
                          extHsSpec = Just
                            (HsTypeSpec
                              (Map.fromList
                                [
                                  _×_
                                    Bits
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Bounded
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Enum
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Eq
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    FiniteBits
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Integral
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Ix
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Num
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Ord
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Read
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    ReadRaw
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Real
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Show
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    StaticSize
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Storable
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    WriteRaw
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = [
                                          ]})]))},
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
                "globals.h:406:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["globals.h"],
                  headerInclude = "globals.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 6,
          storableAlignment = 2,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Version_t",
                  structConstr = Name
                    "@NsConstr"
                    "Version_t",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "version_t_major",
                      fieldType = HsExtBinding
                        ExtRef {
                          extRefModule = ModuleName
                            "HsBindgen.Runtime.Prelude",
                          extRefIdentifier = Identifier
                            "Word8"}
                        (CTypeSpec
                          (Just (Identifier "Word8")))
                        (Just
                          (HsTypeSpec
                            (Map.fromList
                              [
                                _×_
                                  Bits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bounded
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Enum
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Eq
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  FiniteBits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Integral
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ix
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Num
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ord
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Read
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  ReadRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Real
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  StaticSize
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:408:12",
                            fieldName = NamePair {
                              nameC = Name "major",
                              nameHsIdent = Identifier
                                "version_t_major"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint8_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtRef {
                                extRefModule = ModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extRefIdentifier = Identifier
                                  "Word8"},
                              extCSpec = CTypeSpec
                                (Just (Identifier "Word8")),
                              extHsSpec = Just
                                (HsTypeSpec
                                  (Map.fromList
                                    [
                                      _×_
                                        Bits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Bounded
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Enum
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Eq
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        FiniteBits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Integral
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ix
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Num
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ord
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Read
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        ReadRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Real
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Show
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        StaticSize
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Storable
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        WriteRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = [
                                              ]})]))},
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "major",
                          commentLocation = Just
                            "globals.h:408:12",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "version_t_minor",
                      fieldType = HsExtBinding
                        ExtRef {
                          extRefModule = ModuleName
                            "HsBindgen.Runtime.Prelude",
                          extRefIdentifier = Identifier
                            "Word16"}
                        (CTypeSpec
                          (Just (Identifier "Word16")))
                        (Just
                          (HsTypeSpec
                            (Map.fromList
                              [
                                _×_
                                  Bits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bounded
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Enum
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Eq
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  FiniteBits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Integral
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ix
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Num
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ord
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Read
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  ReadRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Real
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  StaticSize
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:409:12",
                            fieldName = NamePair {
                              nameC = Name "minor",
                              nameHsIdent = Identifier
                                "version_t_minor"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint16_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtRef {
                                extRefModule = ModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extRefIdentifier = Identifier
                                  "Word16"},
                              extCSpec = CTypeSpec
                                (Just (Identifier "Word16")),
                              extHsSpec = Just
                                (HsTypeSpec
                                  (Map.fromList
                                    [
                                      _×_
                                        Bits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Bounded
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Enum
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Eq
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        FiniteBits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Integral
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ix
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Num
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ord
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Read
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        ReadRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Real
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Show
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        StaticSize
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Storable
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        WriteRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = [
                                              ]})]))},
                          structFieldOffset = 16,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "minor",
                          commentLocation = Just
                            "globals.h:409:12",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "version_t_patch",
                      fieldType = HsExtBinding
                        ExtRef {
                          extRefModule = ModuleName
                            "HsBindgen.Runtime.Prelude",
                          extRefIdentifier = Identifier
                            "Word8"}
                        (CTypeSpec
                          (Just (Identifier "Word8")))
                        (Just
                          (HsTypeSpec
                            (Map.fromList
                              [
                                _×_
                                  Bits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bounded
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Enum
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Eq
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  FiniteBits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Integral
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ix
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Num
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ord
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Read
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  ReadRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Real
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  StaticSize
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:410:12",
                            fieldName = NamePair {
                              nameC = Name "patch",
                              nameHsIdent = Identifier
                                "version_t_patch"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint8_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtRef {
                                extRefModule = ModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extRefIdentifier = Identifier
                                  "Word8"},
                              extCSpec = CTypeSpec
                                (Just (Identifier "Word8")),
                              extHsSpec = Just
                                (HsTypeSpec
                                  (Map.fromList
                                    [
                                      _×_
                                        Bits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Bounded
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Enum
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Eq
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        FiniteBits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Integral
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ix
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Num
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ord
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Read
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        ReadRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Real
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Show
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        StaticSize
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Storable
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        WriteRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = [
                                              ]})]))},
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "patch",
                          commentLocation = Just
                            "globals.h:410:12",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:406:9",
                        declId = NamePair {
                          nameC = Name "version_t",
                          nameHsIdent = Identifier
                            "Version_t"},
                        declOrigin = NameOriginGenerated
                          (AnonId "globals.h:406:9"),
                        declAliases = [
                          Name "version_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["globals.h"],
                            headerInclude = "globals.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Version_t"),
                          structSizeof = 6,
                          structAlignment = 2,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:408:12",
                                fieldName = NamePair {
                                  nameC = Name "major",
                                  nameHsIdent = Identifier
                                    "version_t_major"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint8_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtRef {
                                    extRefModule = ModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extRefIdentifier = Identifier
                                      "Word8"},
                                  extCSpec = CTypeSpec
                                    (Just (Identifier "Word8")),
                                  extHsSpec = Just
                                    (HsTypeSpec
                                      (Map.fromList
                                        [
                                          _×_
                                            Bits
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Bounded
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Enum
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Eq
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            FiniteBits
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Integral
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Ix
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Num
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Ord
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Read
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            ReadRaw
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Real
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Show
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            StaticSize
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Storable
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            WriteRaw
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = [
                                                  ]})]))},
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:409:12",
                                fieldName = NamePair {
                                  nameC = Name "minor",
                                  nameHsIdent = Identifier
                                    "version_t_minor"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint16_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtRef {
                                    extRefModule = ModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extRefIdentifier = Identifier
                                      "Word16"},
                                  extCSpec = CTypeSpec
                                    (Just (Identifier "Word16")),
                                  extHsSpec = Just
                                    (HsTypeSpec
                                      (Map.fromList
                                        [
                                          _×_
                                            Bits
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Bounded
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Enum
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Eq
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            FiniteBits
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Integral
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Ix
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Num
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Ord
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Read
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            ReadRaw
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Real
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Show
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            StaticSize
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Storable
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            WriteRaw
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = [
                                                  ]})]))},
                              structFieldOffset = 16,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:410:12",
                                fieldName = NamePair {
                                  nameC = Name "patch",
                                  nameHsIdent = Identifier
                                    "version_t_patch"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint8_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtRef {
                                    extRefModule = ModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extRefIdentifier = Identifier
                                      "Word8"},
                                  extCSpec = CTypeSpec
                                    (Just (Identifier "Word8")),
                                  extHsSpec = Just
                                    (HsTypeSpec
                                      (Map.fromList
                                        [
                                          _×_
                                            Bits
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Bounded
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Enum
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Eq
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            FiniteBits
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Integral
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Ix
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Num
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Ord
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Read
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            ReadRaw
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Real
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Show
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            StaticSize
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Storable
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            WriteRaw
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = [
                                                  ]})]))},
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
                        "globals.h:406:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["globals.h"],
                          headerInclude = "globals.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "version_t_major")
                  (Idx 0),
                PeekCField
                  (HsStrLit "version_t_minor")
                  (Idx 0),
                PeekCField
                  (HsStrLit "version_t_patch")
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
                    "Version_t",
                  structConstr = Name
                    "@NsConstr"
                    "Version_t",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "version_t_major",
                      fieldType = HsExtBinding
                        ExtRef {
                          extRefModule = ModuleName
                            "HsBindgen.Runtime.Prelude",
                          extRefIdentifier = Identifier
                            "Word8"}
                        (CTypeSpec
                          (Just (Identifier "Word8")))
                        (Just
                          (HsTypeSpec
                            (Map.fromList
                              [
                                _×_
                                  Bits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bounded
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Enum
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Eq
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  FiniteBits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Integral
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ix
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Num
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ord
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Read
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  ReadRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Real
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  StaticSize
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:408:12",
                            fieldName = NamePair {
                              nameC = Name "major",
                              nameHsIdent = Identifier
                                "version_t_major"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint8_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtRef {
                                extRefModule = ModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extRefIdentifier = Identifier
                                  "Word8"},
                              extCSpec = CTypeSpec
                                (Just (Identifier "Word8")),
                              extHsSpec = Just
                                (HsTypeSpec
                                  (Map.fromList
                                    [
                                      _×_
                                        Bits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Bounded
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Enum
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Eq
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        FiniteBits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Integral
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ix
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Num
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ord
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Read
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        ReadRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Real
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Show
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        StaticSize
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Storable
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        WriteRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = [
                                              ]})]))},
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "major",
                          commentLocation = Just
                            "globals.h:408:12",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "version_t_minor",
                      fieldType = HsExtBinding
                        ExtRef {
                          extRefModule = ModuleName
                            "HsBindgen.Runtime.Prelude",
                          extRefIdentifier = Identifier
                            "Word16"}
                        (CTypeSpec
                          (Just (Identifier "Word16")))
                        (Just
                          (HsTypeSpec
                            (Map.fromList
                              [
                                _×_
                                  Bits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bounded
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Enum
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Eq
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  FiniteBits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Integral
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ix
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Num
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ord
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Read
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  ReadRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Real
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  StaticSize
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:409:12",
                            fieldName = NamePair {
                              nameC = Name "minor",
                              nameHsIdent = Identifier
                                "version_t_minor"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint16_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtRef {
                                extRefModule = ModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extRefIdentifier = Identifier
                                  "Word16"},
                              extCSpec = CTypeSpec
                                (Just (Identifier "Word16")),
                              extHsSpec = Just
                                (HsTypeSpec
                                  (Map.fromList
                                    [
                                      _×_
                                        Bits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Bounded
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Enum
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Eq
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        FiniteBits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Integral
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ix
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Num
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ord
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Read
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        ReadRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Real
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Show
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        StaticSize
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Storable
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        WriteRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = [
                                              ]})]))},
                          structFieldOffset = 16,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "minor",
                          commentLocation = Just
                            "globals.h:409:12",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "version_t_patch",
                      fieldType = HsExtBinding
                        ExtRef {
                          extRefModule = ModuleName
                            "HsBindgen.Runtime.Prelude",
                          extRefIdentifier = Identifier
                            "Word8"}
                        (CTypeSpec
                          (Just (Identifier "Word8")))
                        (Just
                          (HsTypeSpec
                            (Map.fromList
                              [
                                _×_
                                  Bits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bounded
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Enum
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Eq
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  FiniteBits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Integral
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ix
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Num
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ord
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Read
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  ReadRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Real
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  StaticSize
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:410:12",
                            fieldName = NamePair {
                              nameC = Name "patch",
                              nameHsIdent = Identifier
                                "version_t_patch"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint8_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtRef {
                                extRefModule = ModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extRefIdentifier = Identifier
                                  "Word8"},
                              extCSpec = CTypeSpec
                                (Just (Identifier "Word8")),
                              extHsSpec = Just
                                (HsTypeSpec
                                  (Map.fromList
                                    [
                                      _×_
                                        Bits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Bounded
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Enum
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Eq
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        FiniteBits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Integral
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ix
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Num
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ord
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Read
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        ReadRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Real
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Show
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        StaticSize
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Storable
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        WriteRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = [
                                              ]})]))},
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "patch",
                          commentLocation = Just
                            "globals.h:410:12",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:406:9",
                        declId = NamePair {
                          nameC = Name "version_t",
                          nameHsIdent = Identifier
                            "Version_t"},
                        declOrigin = NameOriginGenerated
                          (AnonId "globals.h:406:9"),
                        declAliases = [
                          Name "version_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["globals.h"],
                            headerInclude = "globals.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Version_t"),
                          structSizeof = 6,
                          structAlignment = 2,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:408:12",
                                fieldName = NamePair {
                                  nameC = Name "major",
                                  nameHsIdent = Identifier
                                    "version_t_major"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint8_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtRef {
                                    extRefModule = ModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extRefIdentifier = Identifier
                                      "Word8"},
                                  extCSpec = CTypeSpec
                                    (Just (Identifier "Word8")),
                                  extHsSpec = Just
                                    (HsTypeSpec
                                      (Map.fromList
                                        [
                                          _×_
                                            Bits
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Bounded
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Enum
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Eq
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            FiniteBits
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Integral
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Ix
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Num
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Ord
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Read
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            ReadRaw
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Real
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Show
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            StaticSize
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Storable
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            WriteRaw
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = [
                                                  ]})]))},
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:409:12",
                                fieldName = NamePair {
                                  nameC = Name "minor",
                                  nameHsIdent = Identifier
                                    "version_t_minor"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint16_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtRef {
                                    extRefModule = ModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extRefIdentifier = Identifier
                                      "Word16"},
                                  extCSpec = CTypeSpec
                                    (Just (Identifier "Word16")),
                                  extHsSpec = Just
                                    (HsTypeSpec
                                      (Map.fromList
                                        [
                                          _×_
                                            Bits
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Bounded
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Enum
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Eq
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            FiniteBits
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Integral
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Ix
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Num
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Ord
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Read
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            ReadRaw
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Real
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Show
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            StaticSize
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Storable
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            WriteRaw
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = [
                                                  ]})]))},
                              structFieldOffset = 16,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:410:12",
                                fieldName = NamePair {
                                  nameC = Name "patch",
                                  nameHsIdent = Identifier
                                    "version_t_patch"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint8_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtRef {
                                    extRefModule = ModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extRefIdentifier = Identifier
                                      "Word8"},
                                  extCSpec = CTypeSpec
                                    (Just (Identifier "Word8")),
                                  extHsSpec = Just
                                    (HsTypeSpec
                                      (Map.fromList
                                        [
                                          _×_
                                            Bits
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Bounded
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Enum
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Eq
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            FiniteBits
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Integral
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Ix
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Num
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Ord
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Read
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            ReadRaw
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Real
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Show
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            StaticSize
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Storable
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            WriteRaw
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = [
                                                  ]})]))},
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
                        "globals.h:406:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["globals.h"],
                          headerInclude = "globals.h"},
                      commentChildren = []}}
                (Add 3)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "version_t_major")
                      (Idx 4)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "version_t_minor")
                      (Idx 4)
                      (Idx 1),
                    PokeCField
                      (HsStrLit "version_t_patch")
                      (Idx 4)
                      (Idx 2)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Version_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Version_t",
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
              "Version_t"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "version_t_major",
          hasCFieldInstanceCFieldType =
          HsExtBinding
            ExtRef {
              extRefModule = ModuleName
                "HsBindgen.Runtime.Prelude",
              extRefIdentifier = Identifier
                "Word8"}
            (CTypeSpec
              (Just (Identifier "Word8")))
            (Just
              (HsTypeSpec
                (Map.fromList
                  [
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]))),
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
              "Version_t"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "version_t_major",
          hasFieldInstanceFieldType =
          HsExtBinding
            ExtRef {
              extRefModule = ModuleName
                "HsBindgen.Runtime.Prelude",
              extRefIdentifier = Identifier
                "Word8"}
            (CTypeSpec
              (Just (Identifier "Word8")))
            (Just
              (HsTypeSpec
                (Map.fromList
                  [
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]))),
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
              "Version_t"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "version_t_minor",
          hasCFieldInstanceCFieldType =
          HsExtBinding
            ExtRef {
              extRefModule = ModuleName
                "HsBindgen.Runtime.Prelude",
              extRefIdentifier = Identifier
                "Word16"}
            (CTypeSpec
              (Just (Identifier "Word16")))
            (Just
              (HsTypeSpec
                (Map.fromList
                  [
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]))),
          hasCFieldInstanceFieldOffset =
          2},
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
              "Version_t"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "version_t_minor",
          hasFieldInstanceFieldType =
          HsExtBinding
            ExtRef {
              extRefModule = ModuleName
                "HsBindgen.Runtime.Prelude",
              extRefIdentifier = Identifier
                "Word16"}
            (CTypeSpec
              (Just (Identifier "Word16")))
            (Just
              (HsTypeSpec
                (Map.fromList
                  [
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]))),
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
              "Version_t"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "version_t_patch",
          hasCFieldInstanceCFieldType =
          HsExtBinding
            ExtRef {
              extRefModule = ModuleName
                "HsBindgen.Runtime.Prelude",
              extRefIdentifier = Identifier
                "Word8"}
            (CTypeSpec
              (Just (Identifier "Word8")))
            (Just
              (HsTypeSpec
                (Map.fromList
                  [
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]))),
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
              "Version_t"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "version_t_patch",
          hasFieldInstanceFieldType =
          HsExtBinding
            ExtRef {
              extRefModule = ModuleName
                "HsBindgen.Runtime.Prelude",
              extRefIdentifier = Identifier
                "Word8"}
            (CTypeSpec
              (Just (Identifier "Word8")))
            (Just
              (HsTypeSpec
                (Map.fromList
                  [
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]))),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Struct1_t",
      structConstr = Name
        "@NsConstr"
        "Struct1_t",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "struct1_t_x",
          fieldType = HsExtBinding
            ExtRef {
              extRefModule = ModuleName
                "HsBindgen.Runtime.Prelude",
              extRefIdentifier = Identifier
                "Word16"}
            (CTypeSpec
              (Just (Identifier "Word16")))
            (Just
              (HsTypeSpec
                (Map.fromList
                  [
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]))),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:415:13",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "struct1_t_x"},
                fieldComment = Nothing},
              structFieldType = TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "uint16_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtRef {
                    extRefModule = ModuleName
                      "HsBindgen.Runtime.Prelude",
                    extRefIdentifier = Identifier
                      "Word16"},
                  extCSpec = CTypeSpec
                    (Just (Identifier "Word16")),
                  extHsSpec = Just
                    (HsTypeSpec
                      (Map.fromList
                        [
                          _×_
                            Bits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bounded
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Enum
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Eq
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            FiniteBits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Integral
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ix
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Num
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ord
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Read
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            ReadRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Real
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Show
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            StaticSize
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Storable
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            WriteRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = [
                                  ]})]))},
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Just
                "globals.h:415:13",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["globals.h"],
                  headerInclude = "globals.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "struct1_t_y",
          fieldType = HsPrimType
            HsPrimCBool,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:416:13",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier
                    "struct1_t_y"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                PrimBool,
              structFieldOffset = 16,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Just
                "globals.h:416:13",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["globals.h"],
                  headerInclude = "globals.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "struct1_t_version",
          fieldType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Version_t"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:417:13",
                fieldName = NamePair {
                  nameC = Name "version",
                  nameHsIdent = Identifier
                    "struct1_t_version"},
                fieldComment = Nothing},
              structFieldType = TypeTypedef
                (TypedefSquashed
                  (Name "version_t")
                  (TypeStruct
                    NamePair {
                      nameC = Name "version_t",
                      nameHsIdent = Identifier
                        "Version_t"}
                    (NameOriginGenerated
                      (AnonId "globals.h:406:9")))),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "version",
              commentLocation = Just
                "globals.h:417:13",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["globals.h"],
                  headerInclude = "globals.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "globals.h:413:9",
            declId = NamePair {
              nameC = Name "struct1_t",
              nameHsIdent = Identifier
                "Struct1_t"},
            declOrigin = NameOriginGenerated
              (AnonId "globals.h:413:9"),
            declAliases = [
              Name "struct1_t"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["globals.h"],
                headerInclude = "globals.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Struct1_t"),
              structSizeof = 10,
              structAlignment = 2,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:415:13",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "struct1_t_x"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint16_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtRef {
                        extRefModule = ModuleName
                          "HsBindgen.Runtime.Prelude",
                        extRefIdentifier = Identifier
                          "Word16"},
                      extCSpec = CTypeSpec
                        (Just (Identifier "Word16")),
                      extHsSpec = Just
                        (HsTypeSpec
                          (Map.fromList
                            [
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]))},
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:416:13",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "struct1_t_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 16,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:417:13",
                    fieldName = NamePair {
                      nameC = Name "version",
                      nameHsIdent = Identifier
                        "struct1_t_version"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name "version_t")
                      (TypeStruct
                        NamePair {
                          nameC = Name "version_t",
                          nameHsIdent = Identifier
                            "Version_t"}
                        (NameOriginGenerated
                          (AnonId "globals.h:406:9")))),
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
            "globals.h:413:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["globals.h"],
              headerInclude = "globals.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Struct1_t",
          structConstr = Name
            "@NsConstr"
            "Struct1_t",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "struct1_t_x",
              fieldType = HsExtBinding
                ExtRef {
                  extRefModule = ModuleName
                    "HsBindgen.Runtime.Prelude",
                  extRefIdentifier = Identifier
                    "Word16"}
                (CTypeSpec
                  (Just (Identifier "Word16")))
                (Just
                  (HsTypeSpec
                    (Map.fromList
                      [
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]))),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:415:13",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "struct1_t_x"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint16_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtRef {
                        extRefModule = ModuleName
                          "HsBindgen.Runtime.Prelude",
                        extRefIdentifier = Identifier
                          "Word16"},
                      extCSpec = CTypeSpec
                        (Just (Identifier "Word16")),
                      extHsSpec = Just
                        (HsTypeSpec
                          (Map.fromList
                            [
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]))},
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "x",
                  commentLocation = Just
                    "globals.h:415:13",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["globals.h"],
                      headerInclude = "globals.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "struct1_t_y",
              fieldType = HsPrimType
                HsPrimCBool,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:416:13",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "struct1_t_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 16,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "y",
                  commentLocation = Just
                    "globals.h:416:13",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["globals.h"],
                      headerInclude = "globals.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "struct1_t_version",
              fieldType = HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Version_t"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:417:13",
                    fieldName = NamePair {
                      nameC = Name "version",
                      nameHsIdent = Identifier
                        "struct1_t_version"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name "version_t")
                      (TypeStruct
                        NamePair {
                          nameC = Name "version_t",
                          nameHsIdent = Identifier
                            "Version_t"}
                        (NameOriginGenerated
                          (AnonId "globals.h:406:9")))),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "version",
                  commentLocation = Just
                    "globals.h:417:13",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["globals.h"],
                      headerInclude = "globals.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "globals.h:413:9",
                declId = NamePair {
                  nameC = Name "struct1_t",
                  nameHsIdent = Identifier
                    "Struct1_t"},
                declOrigin = NameOriginGenerated
                  (AnonId "globals.h:413:9"),
                declAliases = [
                  Name "struct1_t"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["globals.h"],
                    headerInclude = "globals.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Struct1_t"),
                  structSizeof = 10,
                  structAlignment = 2,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:415:13",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "struct1_t_x"},
                        fieldComment = Nothing},
                      structFieldType = TypeExtBinding
                        ResolvedExtBinding {
                          extCName = QualName {
                            qualNameName = Name "uint16_t",
                            qualNameKind =
                            NameKindOrdinary},
                          extHsRef = ExtRef {
                            extRefModule = ModuleName
                              "HsBindgen.Runtime.Prelude",
                            extRefIdentifier = Identifier
                              "Word16"},
                          extCSpec = CTypeSpec
                            (Just (Identifier "Word16")),
                          extHsSpec = Just
                            (HsTypeSpec
                              (Map.fromList
                                [
                                  _×_
                                    Bits
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Bounded
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Enum
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Eq
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    FiniteBits
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Integral
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Ix
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Num
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Ord
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Read
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    ReadRaw
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Real
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Show
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    StaticSize
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Storable
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    WriteRaw
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = [
                                          ]})]))},
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:416:13",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = Identifier
                            "struct1_t_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        PrimBool,
                      structFieldOffset = 16,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:417:13",
                        fieldName = NamePair {
                          nameC = Name "version",
                          nameHsIdent = Identifier
                            "struct1_t_version"},
                        fieldComment = Nothing},
                      structFieldType = TypeTypedef
                        (TypedefSquashed
                          (Name "version_t")
                          (TypeStruct
                            NamePair {
                              nameC = Name "version_t",
                              nameHsIdent = Identifier
                                "Version_t"}
                            (NameOriginGenerated
                              (AnonId "globals.h:406:9")))),
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
                "globals.h:413:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["globals.h"],
                  headerInclude = "globals.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 10,
          storableAlignment = 2,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Struct1_t",
                  structConstr = Name
                    "@NsConstr"
                    "Struct1_t",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "struct1_t_x",
                      fieldType = HsExtBinding
                        ExtRef {
                          extRefModule = ModuleName
                            "HsBindgen.Runtime.Prelude",
                          extRefIdentifier = Identifier
                            "Word16"}
                        (CTypeSpec
                          (Just (Identifier "Word16")))
                        (Just
                          (HsTypeSpec
                            (Map.fromList
                              [
                                _×_
                                  Bits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bounded
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Enum
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Eq
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  FiniteBits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Integral
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ix
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Num
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ord
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Read
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  ReadRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Real
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  StaticSize
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:415:13",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "struct1_t_x"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint16_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtRef {
                                extRefModule = ModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extRefIdentifier = Identifier
                                  "Word16"},
                              extCSpec = CTypeSpec
                                (Just (Identifier "Word16")),
                              extHsSpec = Just
                                (HsTypeSpec
                                  (Map.fromList
                                    [
                                      _×_
                                        Bits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Bounded
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Enum
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Eq
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        FiniteBits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Integral
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ix
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Num
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ord
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Read
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        ReadRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Real
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Show
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        StaticSize
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Storable
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        WriteRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = [
                                              ]})]))},
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "globals.h:415:13",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "struct1_t_y",
                      fieldType = HsPrimType
                        HsPrimCBool,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:416:13",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "struct1_t_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            PrimBool,
                          structFieldOffset = 16,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "globals.h:416:13",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "struct1_t_version",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Version_t"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:417:13",
                            fieldName = NamePair {
                              nameC = Name "version",
                              nameHsIdent = Identifier
                                "struct1_t_version"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefSquashed
                              (Name "version_t")
                              (TypeStruct
                                NamePair {
                                  nameC = Name "version_t",
                                  nameHsIdent = Identifier
                                    "Version_t"}
                                (NameOriginGenerated
                                  (AnonId "globals.h:406:9")))),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "version",
                          commentLocation = Just
                            "globals.h:417:13",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:413:9",
                        declId = NamePair {
                          nameC = Name "struct1_t",
                          nameHsIdent = Identifier
                            "Struct1_t"},
                        declOrigin = NameOriginGenerated
                          (AnonId "globals.h:413:9"),
                        declAliases = [
                          Name "struct1_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["globals.h"],
                            headerInclude = "globals.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Struct1_t"),
                          structSizeof = 10,
                          structAlignment = 2,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:415:13",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "struct1_t_x"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint16_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtRef {
                                    extRefModule = ModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extRefIdentifier = Identifier
                                      "Word16"},
                                  extCSpec = CTypeSpec
                                    (Just (Identifier "Word16")),
                                  extHsSpec = Just
                                    (HsTypeSpec
                                      (Map.fromList
                                        [
                                          _×_
                                            Bits
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Bounded
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Enum
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Eq
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            FiniteBits
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Integral
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Ix
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Num
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Ord
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Read
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            ReadRaw
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Real
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Show
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            StaticSize
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Storable
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            WriteRaw
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = [
                                                  ]})]))},
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:416:13",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "struct1_t_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                PrimBool,
                              structFieldOffset = 16,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:417:13",
                                fieldName = NamePair {
                                  nameC = Name "version",
                                  nameHsIdent = Identifier
                                    "struct1_t_version"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefSquashed
                                  (Name "version_t")
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "version_t",
                                      nameHsIdent = Identifier
                                        "Version_t"}
                                    (NameOriginGenerated
                                      (AnonId "globals.h:406:9")))),
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
                        "globals.h:413:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["globals.h"],
                          headerInclude = "globals.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "struct1_t_x")
                  (Idx 0),
                PeekCField
                  (HsStrLit "struct1_t_y")
                  (Idx 0),
                PeekCField
                  (HsStrLit "struct1_t_version")
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
                    "Struct1_t",
                  structConstr = Name
                    "@NsConstr"
                    "Struct1_t",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "struct1_t_x",
                      fieldType = HsExtBinding
                        ExtRef {
                          extRefModule = ModuleName
                            "HsBindgen.Runtime.Prelude",
                          extRefIdentifier = Identifier
                            "Word16"}
                        (CTypeSpec
                          (Just (Identifier "Word16")))
                        (Just
                          (HsTypeSpec
                            (Map.fromList
                              [
                                _×_
                                  Bits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bounded
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Enum
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Eq
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  FiniteBits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Integral
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ix
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Num
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ord
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Read
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  ReadRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Real
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  StaticSize
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:415:13",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "struct1_t_x"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint16_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtRef {
                                extRefModule = ModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extRefIdentifier = Identifier
                                  "Word16"},
                              extCSpec = CTypeSpec
                                (Just (Identifier "Word16")),
                              extHsSpec = Just
                                (HsTypeSpec
                                  (Map.fromList
                                    [
                                      _×_
                                        Bits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Bounded
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Enum
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Eq
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        FiniteBits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Integral
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ix
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Num
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ord
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Read
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        ReadRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Real
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Show
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        StaticSize
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Storable
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        WriteRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = [
                                              ]})]))},
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "globals.h:415:13",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "struct1_t_y",
                      fieldType = HsPrimType
                        HsPrimCBool,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:416:13",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "struct1_t_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            PrimBool,
                          structFieldOffset = 16,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "globals.h:416:13",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "struct1_t_version",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Version_t"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:417:13",
                            fieldName = NamePair {
                              nameC = Name "version",
                              nameHsIdent = Identifier
                                "struct1_t_version"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefSquashed
                              (Name "version_t")
                              (TypeStruct
                                NamePair {
                                  nameC = Name "version_t",
                                  nameHsIdent = Identifier
                                    "Version_t"}
                                (NameOriginGenerated
                                  (AnonId "globals.h:406:9")))),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "version",
                          commentLocation = Just
                            "globals.h:417:13",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:413:9",
                        declId = NamePair {
                          nameC = Name "struct1_t",
                          nameHsIdent = Identifier
                            "Struct1_t"},
                        declOrigin = NameOriginGenerated
                          (AnonId "globals.h:413:9"),
                        declAliases = [
                          Name "struct1_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["globals.h"],
                            headerInclude = "globals.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Struct1_t"),
                          structSizeof = 10,
                          structAlignment = 2,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:415:13",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "struct1_t_x"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint16_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtRef {
                                    extRefModule = ModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extRefIdentifier = Identifier
                                      "Word16"},
                                  extCSpec = CTypeSpec
                                    (Just (Identifier "Word16")),
                                  extHsSpec = Just
                                    (HsTypeSpec
                                      (Map.fromList
                                        [
                                          _×_
                                            Bits
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Bounded
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Enum
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Eq
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            FiniteBits
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Integral
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Ix
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Num
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Ord
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Read
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            ReadRaw
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Real
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Show
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            StaticSize
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Storable
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            WriteRaw
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = [
                                                  ]})]))},
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:416:13",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "struct1_t_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                PrimBool,
                              structFieldOffset = 16,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:417:13",
                                fieldName = NamePair {
                                  nameC = Name "version",
                                  nameHsIdent = Identifier
                                    "struct1_t_version"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefSquashed
                                  (Name "version_t")
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "version_t",
                                      nameHsIdent = Identifier
                                        "Version_t"}
                                    (NameOriginGenerated
                                      (AnonId "globals.h:406:9")))),
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
                        "globals.h:413:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["globals.h"],
                          headerInclude = "globals.h"},
                      commentChildren = []}}
                (Add 3)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "struct1_t_x")
                      (Idx 4)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "struct1_t_y")
                      (Idx 4)
                      (Idx 1),
                    PokeCField
                      (HsStrLit "struct1_t_version")
                      (Idx 4)
                      (Idx 2)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Struct1_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Struct1_t",
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
              "Struct1_t"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "struct1_t_x",
          hasCFieldInstanceCFieldType =
          HsExtBinding
            ExtRef {
              extRefModule = ModuleName
                "HsBindgen.Runtime.Prelude",
              extRefIdentifier = Identifier
                "Word16"}
            (CTypeSpec
              (Just (Identifier "Word16")))
            (Just
              (HsTypeSpec
                (Map.fromList
                  [
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]))),
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
              "Struct1_t"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "struct1_t_x",
          hasFieldInstanceFieldType =
          HsExtBinding
            ExtRef {
              extRefModule = ModuleName
                "HsBindgen.Runtime.Prelude",
              extRefIdentifier = Identifier
                "Word16"}
            (CTypeSpec
              (Just (Identifier "Word16")))
            (Just
              (HsTypeSpec
                (Map.fromList
                  [
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]))),
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
              "Struct1_t"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "struct1_t_y",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCBool,
          hasCFieldInstanceFieldOffset =
          2},
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
              "Struct1_t"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "struct1_t_y",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCBool,
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
              "Struct1_t"),
          hasCFieldInstanceFieldName =
          Name
            "@NsVar"
            "struct1_t_version",
          hasCFieldInstanceCFieldType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Version_t"),
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
              "Struct1_t"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "struct1_t_version",
          hasFieldInstanceFieldType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Version_t"),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Struct2_t",
      structConstr = Name
        "@NsConstr"
        "Struct2_t",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "struct2_t_field1",
          fieldType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Struct1_t"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:422:13",
                fieldName = NamePair {
                  nameC = Name "field1",
                  nameHsIdent = Identifier
                    "struct2_t_field1"},
                fieldComment = Nothing},
              structFieldType = TypeTypedef
                (TypedefSquashed
                  (Name "struct1_t")
                  (TypeStruct
                    NamePair {
                      nameC = Name "struct1_t",
                      nameHsIdent = Identifier
                        "Struct1_t"}
                    (NameOriginGenerated
                      (AnonId "globals.h:413:9")))),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "field1",
              commentLocation = Just
                "globals.h:422:13",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["globals.h"],
                  headerInclude = "globals.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "globals.h:420:9",
            declId = NamePair {
              nameC = Name "struct2_t",
              nameHsIdent = Identifier
                "Struct2_t"},
            declOrigin = NameOriginGenerated
              (AnonId "globals.h:420:9"),
            declAliases = [
              Name "struct2_t"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["globals.h"],
                headerInclude = "globals.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Struct2_t"),
              structSizeof = 10,
              structAlignment = 2,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:422:13",
                    fieldName = NamePair {
                      nameC = Name "field1",
                      nameHsIdent = Identifier
                        "struct2_t_field1"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name "struct1_t")
                      (TypeStruct
                        NamePair {
                          nameC = Name "struct1_t",
                          nameHsIdent = Identifier
                            "Struct1_t"}
                        (NameOriginGenerated
                          (AnonId "globals.h:413:9")))),
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
          commentOrigin = Nothing,
          commentLocation = Just
            "globals.h:420:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["globals.h"],
              headerInclude = "globals.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Struct2_t",
          structConstr = Name
            "@NsConstr"
            "Struct2_t",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "struct2_t_field1",
              fieldType = HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Struct1_t"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:422:13",
                    fieldName = NamePair {
                      nameC = Name "field1",
                      nameHsIdent = Identifier
                        "struct2_t_field1"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name "struct1_t")
                      (TypeStruct
                        NamePair {
                          nameC = Name "struct1_t",
                          nameHsIdent = Identifier
                            "Struct1_t"}
                        (NameOriginGenerated
                          (AnonId "globals.h:413:9")))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "field1",
                  commentLocation = Just
                    "globals.h:422:13",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["globals.h"],
                      headerInclude = "globals.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "globals.h:420:9",
                declId = NamePair {
                  nameC = Name "struct2_t",
                  nameHsIdent = Identifier
                    "Struct2_t"},
                declOrigin = NameOriginGenerated
                  (AnonId "globals.h:420:9"),
                declAliases = [
                  Name "struct2_t"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["globals.h"],
                    headerInclude = "globals.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Struct2_t"),
                  structSizeof = 10,
                  structAlignment = 2,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:422:13",
                        fieldName = NamePair {
                          nameC = Name "field1",
                          nameHsIdent = Identifier
                            "struct2_t_field1"},
                        fieldComment = Nothing},
                      structFieldType = TypeTypedef
                        (TypedefSquashed
                          (Name "struct1_t")
                          (TypeStruct
                            NamePair {
                              nameC = Name "struct1_t",
                              nameHsIdent = Identifier
                                "Struct1_t"}
                            (NameOriginGenerated
                              (AnonId "globals.h:413:9")))),
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
              commentOrigin = Nothing,
              commentLocation = Just
                "globals.h:420:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["globals.h"],
                  headerInclude = "globals.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 10,
          storableAlignment = 2,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Struct2_t",
                  structConstr = Name
                    "@NsConstr"
                    "Struct2_t",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "struct2_t_field1",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Struct1_t"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:422:13",
                            fieldName = NamePair {
                              nameC = Name "field1",
                              nameHsIdent = Identifier
                                "struct2_t_field1"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefSquashed
                              (Name "struct1_t")
                              (TypeStruct
                                NamePair {
                                  nameC = Name "struct1_t",
                                  nameHsIdent = Identifier
                                    "Struct1_t"}
                                (NameOriginGenerated
                                  (AnonId "globals.h:413:9")))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field1",
                          commentLocation = Just
                            "globals.h:422:13",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:420:9",
                        declId = NamePair {
                          nameC = Name "struct2_t",
                          nameHsIdent = Identifier
                            "Struct2_t"},
                        declOrigin = NameOriginGenerated
                          (AnonId "globals.h:420:9"),
                        declAliases = [
                          Name "struct2_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["globals.h"],
                            headerInclude = "globals.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Struct2_t"),
                          structSizeof = 10,
                          structAlignment = 2,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:422:13",
                                fieldName = NamePair {
                                  nameC = Name "field1",
                                  nameHsIdent = Identifier
                                    "struct2_t_field1"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefSquashed
                                  (Name "struct1_t")
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "struct1_t",
                                      nameHsIdent = Identifier
                                        "Struct1_t"}
                                    (NameOriginGenerated
                                      (AnonId "globals.h:413:9")))),
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
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "globals.h:420:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["globals.h"],
                          headerInclude = "globals.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "struct2_t_field1")
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
                    "Struct2_t",
                  structConstr = Name
                    "@NsConstr"
                    "Struct2_t",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "struct2_t_field1",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Struct1_t"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:422:13",
                            fieldName = NamePair {
                              nameC = Name "field1",
                              nameHsIdent = Identifier
                                "struct2_t_field1"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefSquashed
                              (Name "struct1_t")
                              (TypeStruct
                                NamePair {
                                  nameC = Name "struct1_t",
                                  nameHsIdent = Identifier
                                    "Struct1_t"}
                                (NameOriginGenerated
                                  (AnonId "globals.h:413:9")))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field1",
                          commentLocation = Just
                            "globals.h:422:13",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:420:9",
                        declId = NamePair {
                          nameC = Name "struct2_t",
                          nameHsIdent = Identifier
                            "Struct2_t"},
                        declOrigin = NameOriginGenerated
                          (AnonId "globals.h:420:9"),
                        declAliases = [
                          Name "struct2_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["globals.h"],
                            headerInclude = "globals.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Struct2_t"),
                          structSizeof = 10,
                          structAlignment = 2,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:422:13",
                                fieldName = NamePair {
                                  nameC = Name "field1",
                                  nameHsIdent = Identifier
                                    "struct2_t_field1"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefSquashed
                                  (Name "struct1_t")
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "struct1_t",
                                      nameHsIdent = Identifier
                                        "Struct1_t"}
                                    (NameOriginGenerated
                                      (AnonId "globals.h:413:9")))),
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
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "globals.h:420:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["globals.h"],
                          headerInclude = "globals.h"},
                      commentChildren = []}}
                (Add 1)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "struct2_t_field1")
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
        "Struct2_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Struct2_t",
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
              "Struct2_t"),
          hasCFieldInstanceFieldName =
          Name
            "@NsVar"
            "struct2_t_field1",
          hasCFieldInstanceCFieldType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Struct1_t"),
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
              "Struct2_t"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "struct2_t_field1",
          hasFieldInstanceFieldType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Struct1_t"),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "ConstInt",
      newtypeConstr = Name
        "@NsConstr"
        "ConstInt",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_ConstInt",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "globals.h:448:19",
          declId = NamePair {
            nameC = Name "ConstInt",
            nameHsIdent = Identifier
              "ConstInt"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["globals.h"],
              headerInclude = "globals.h"},
          declComment = Just
            (Comment
              [
                Paragraph
                  [
                    TextContent
                      "Constant, through typedef"]])},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "ConstInt",
              newtypeField = Name
                "@NsVar"
                "un_ConstInt"},
            typedefType = TypeQualified
              TypeQualifierConst
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [
          Bits,
          Bounded,
          Enum,
          Eq,
          FiniteBits,
          Integral,
          Ix,
          Num,
          Ord,
          Read,
          Real,
          Show,
          Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Constant, through typedef"],
          commentOrigin = Just "ConstInt",
          commentLocation = Just
            "globals.h:448:19",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["globals.h"],
              headerInclude = "globals.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass =
      FiniteBits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
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
              "ConstInt"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_ConstInt",
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
              "ConstInt"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_ConstInt",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCInt,
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "ConstIntArray",
      newtypeConstr = Name
        "@NsConstr"
        "ConstIntArray",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_ConstIntArray",
        fieldType = HsIncompleteArray
          (HsPrimType HsPrimCInt),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin =
      Decl {
        declInfo =
        DeclInfo {
          declLoc = "globals.h:463:19",
          declId = NamePair {
            nameC = Name "ConstIntArray",
            nameHsIdent = Identifier
              "ConstIntArray"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["globals.h"],
              headerInclude = "globals.h"},
          declComment =
          Just
            (Comment
              [
                Paragraph
                  [
                    TextContent
                      "An array of uknown size containing constant integers"]])},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "ConstIntArray",
              newtypeField = Name
                "@NsVar"
                "un_ConstIntArray"},
            typedefType =
            TypeIncompleteArray
              (TypeQualified
                TypeQualifierConst
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [Eq, Show],
      newtypeComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "An array of uknown size containing constant integers"],
          commentOrigin = Just
            "ConstIntArray",
          commentLocation = Just
            "globals.h:463:19",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["globals.h"],
              headerInclude = "globals.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ConstIntArray",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ConstIntArray",
      deriveInstanceComment =
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
              "ConstIntArray"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_ConstIntArray",
          hasFieldInstanceFieldType =
          HsIncompleteArray
            (HsPrimType HsPrimCInt),
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
              "ConstIntArray"),
          hasCFieldInstanceFieldName =
          Name
            "@NsVar"
            "un_ConstIntArray",
          hasCFieldInstanceCFieldType =
          HsIncompleteArray
            (HsPrimType HsPrimCInt),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Tuple",
      structConstr = Name
        "@NsConstr"
        "Tuple",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "tuple_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:466:20",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "tuple_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Just
                "globals.h:466:20",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["globals.h"],
                  headerInclude = "globals.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "tuple_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:466:33",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier
                    "tuple_y"},
                fieldComment = Nothing},
              structFieldType = TypeQualified
                TypeQualifierConst
                (TypePrim
                  (PrimIntegral PrimInt Signed)),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Just
                "globals.h:466:33",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["globals.h"],
                  headerInclude = "globals.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "globals.h:466:8",
            declId = NamePair {
              nameC = Name "tuple",
              nameHsIdent = Identifier
                "Tuple"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["globals.h"],
                headerInclude = "globals.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Tuple"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:466:20",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "tuple_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:466:33",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "tuple_y"},
                    fieldComment = Nothing},
                  structFieldType = TypeQualified
                    TypeQualifierConst
                    (TypePrim
                      (PrimIntegral PrimInt Signed)),
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
          commentOrigin = Just "tuple",
          commentLocation = Just
            "globals.h:466:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["globals.h"],
              headerInclude = "globals.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Tuple",
          structConstr = Name
            "@NsConstr"
            "Tuple",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "tuple_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:466:20",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "tuple_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "x",
                  commentLocation = Just
                    "globals.h:466:20",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["globals.h"],
                      headerInclude = "globals.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "tuple_y",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:466:33",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "tuple_y"},
                    fieldComment = Nothing},
                  structFieldType = TypeQualified
                    TypeQualifierConst
                    (TypePrim
                      (PrimIntegral PrimInt Signed)),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "y",
                  commentLocation = Just
                    "globals.h:466:33",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["globals.h"],
                      headerInclude = "globals.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "globals.h:466:8",
                declId = NamePair {
                  nameC = Name "tuple",
                  nameHsIdent = Identifier
                    "Tuple"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["globals.h"],
                    headerInclude = "globals.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Tuple"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:466:20",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "tuple_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:466:33",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = Identifier
                            "tuple_y"},
                        fieldComment = Nothing},
                      structFieldType = TypeQualified
                        TypeQualifierConst
                        (TypePrim
                          (PrimIntegral PrimInt Signed)),
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
              commentOrigin = Just "tuple",
              commentLocation = Just
                "globals.h:466:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["globals.h"],
                  headerInclude = "globals.h"},
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
                    "Tuple",
                  structConstr = Name
                    "@NsConstr"
                    "Tuple",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "tuple_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:466:20",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "tuple_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "globals.h:466:20",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "tuple_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:466:33",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "tuple_y"},
                            fieldComment = Nothing},
                          structFieldType = TypeQualified
                            TypeQualifierConst
                            (TypePrim
                              (PrimIntegral PrimInt Signed)),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "globals.h:466:33",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:466:8",
                        declId = NamePair {
                          nameC = Name "tuple",
                          nameHsIdent = Identifier
                            "Tuple"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["globals.h"],
                            headerInclude = "globals.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Tuple"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:466:20",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "tuple_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:466:33",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "tuple_y"},
                                fieldComment = Nothing},
                              structFieldType = TypeQualified
                                TypeQualifierConst
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
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
                      commentOrigin = Just "tuple",
                      commentLocation = Just
                        "globals.h:466:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["globals.h"],
                          headerInclude = "globals.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "tuple_x")
                  (Idx 0),
                PeekCField
                  (HsStrLit "tuple_y")
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
                    "Tuple",
                  structConstr = Name
                    "@NsConstr"
                    "Tuple",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "tuple_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:466:20",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "tuple_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "globals.h:466:20",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "tuple_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:466:33",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "tuple_y"},
                            fieldComment = Nothing},
                          structFieldType = TypeQualified
                            TypeQualifierConst
                            (TypePrim
                              (PrimIntegral PrimInt Signed)),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "globals.h:466:33",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["globals.h"],
                              headerInclude = "globals.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:466:8",
                        declId = NamePair {
                          nameC = Name "tuple",
                          nameHsIdent = Identifier
                            "Tuple"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["globals.h"],
                            headerInclude = "globals.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Tuple"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:466:20",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "tuple_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:466:33",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "tuple_y"},
                                fieldComment = Nothing},
                              structFieldType = TypeQualified
                                TypeQualifierConst
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
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
                      commentOrigin = Just "tuple",
                      commentLocation = Just
                        "globals.h:466:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["globals.h"],
                          headerInclude = "globals.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "tuple_x")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "tuple_y")
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
        "Tuple",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Tuple",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Tuple"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "tuple_x",
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
            (Name "@NsTypeConstr" "Tuple"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "tuple_x",
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
            (Name "@NsTypeConstr" "Tuple"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "tuple_y",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCInt,
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
            (Name "@NsTypeConstr" "Tuple"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "tuple_y",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_454b1c1a1b7d5b60",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_454b1c1a1b7d5b60",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_simpleGlobal_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int *hs_bindgen_test_globals_454b1c1a1b7d5b60 (void)\n",
              "{\n",
              "  return &simpleGlobal;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypePrim
          (PrimIntegral PrimInt Signed)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_89495849c6dfe834",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Config")))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_89495849c6dfe834",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_compoundGlobal1_ptr */\n",
              "__attribute__ ((const))\n",
              "struct config *hs_bindgen_test_globals_89495849c6dfe834 (void)\n",
              "{\n",
              "  return &compoundGlobal1;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypeStruct
          NamePair {
            nameC = Name "config",
            nameHsIdent = Identifier
              "Config"}
          NameOriginInSource),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_4b79fc96cf429b3a",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Inline_struct")))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_4b79fc96cf429b3a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_compoundGlobal2_ptr */\n",
              "__attribute__ ((const))\n",
              "struct inline_struct *hs_bindgen_test_globals_4b79fc96cf429b3a (void)\n",
              "{\n",
              "  return &compoundGlobal2;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypeStruct
          NamePair {
            nameC = Name "inline_struct",
            nameHsIdent = Identifier
              "Inline_struct"}
          NameOriginInSource),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_e408a99291c13956",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_e408a99291c13956",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_nesInteger_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int *hs_bindgen_test_globals_e408a99291c13956 (void)\n",
              "{\n",
              "  return &nesInteger;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypePrim
          (PrimIntegral PrimInt Signed)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_572819a2ae538f9e",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCFloat))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_572819a2ae538f9e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_nesFloating_ptr */\n",
              "__attribute__ ((const))\n",
              "float *hs_bindgen_test_globals_572819a2ae538f9e (void)\n",
              "{\n",
              "  return &nesFloating;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypePrim
          (PrimFloating PrimFloat)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_b9c5b2253881699a",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPtr
              (HsPrimType HsPrimCChar)))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_b9c5b2253881699a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_nesString1_ptr */\n",
              "__attribute__ ((const))\n",
              "char **hs_bindgen_test_globals_b9c5b2253881699a (void)\n",
              "{\n",
              "  return &nesString1;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypePointer
          (TypePrim
            (PrimChar
              (PrimSignImplicit
                (Just Signed))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_cea7b6d95dd81460",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCChar)))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_cea7b6d95dd81460",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_nesString2_ptr */\n",
              "__attribute__ ((const))\n",
              "char (*hs_bindgen_test_globals_cea7b6d95dd81460 (void))[3]\n",
              "{\n",
              "  return &nesString2;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypeConstArray
          3
          (TypePrim
            (PrimChar
              (PrimSignImplicit
                (Just Signed))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_1e474dd7b5a92e6a",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCChar))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_1e474dd7b5a92e6a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_nesCharacter_ptr */\n",
              "__attribute__ ((const))\n",
              "char *hs_bindgen_test_globals_1e474dd7b5a92e6a (void)\n",
              "{\n",
              "  return &nesCharacter;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypePrim
          (PrimChar
            (PrimSignImplicit
              (Just Signed)))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_5e7165450285240d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_5e7165450285240d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_nesParen_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int *hs_bindgen_test_globals_5e7165450285240d (void)\n",
              "{\n",
              "  return &nesParen;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypePrim
          (PrimIntegral PrimInt Signed)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_f00692845b6c8efd",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_f00692845b6c8efd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_nesUnary_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int *hs_bindgen_test_globals_f00692845b6c8efd (void)\n",
              "{\n",
              "  return &nesUnary;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypePrim
          (PrimIntegral PrimInt Signed)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_d737cdc355737c2b",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_d737cdc355737c2b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_nesBinary_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int *hs_bindgen_test_globals_d737cdc355737c2b (void)\n",
              "{\n",
              "  return &nesBinary;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypePrim
          (PrimIntegral PrimInt Signed)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_a8a7382ba67a6c7d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_a8a7382ba67a6c7d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_nesConditional_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int *hs_bindgen_test_globals_a8a7382ba67a6c7d (void)\n",
              "{\n",
              "  return &nesConditional;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypePrim
          (PrimIntegral PrimInt Signed)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_ea6b1f97253ae23c",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCFloat))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_ea6b1f97253ae23c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_nesCast_ptr */\n",
              "__attribute__ ((const))\n",
              "float *hs_bindgen_test_globals_ea6b1f97253ae23c (void)\n",
              "{\n",
              "  return &nesCast;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypePrim
          (PrimFloating PrimFloat)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_34ed05675f69bccd",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPtr
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_34ed05675f69bccd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_nesCompound_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int **hs_bindgen_test_globals_34ed05675f69bccd (void)\n",
              "{\n",
              "  return &nesCompound;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypePointer
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_9e7c70b133f8e175",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              4
              (HsExtBinding
                ExtRef {
                  extRefModule = ModuleName
                    "HsBindgen.Runtime.Prelude",
                  extRefIdentifier = Identifier
                    "Word8"}
                (CTypeSpec
                  (Just (Identifier "Word8")))
                (Just
                  (HsTypeSpec
                    (Map.fromList
                      [
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]))))))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_9e7c70b133f8e175",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_nesInitList_ptr */\n",
              "__attribute__ ((const))\n",
              "uint8_t (*hs_bindgen_test_globals_9e7c70b133f8e175 (void))[4]\n",
              "{\n",
              "  return &nesInitList;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypeConstArray
          4
          (TypeExtBinding
            ResolvedExtBinding {
              extCName = QualName {
                qualNameName = Name "uint8_t",
                qualNameKind =
                NameKindOrdinary},
              extHsRef = ExtRef {
                extRefModule = ModuleName
                  "HsBindgen.Runtime.Prelude",
                extRefIdentifier = Identifier
                  "Word8"},
              extCSpec = CTypeSpec
                (Just (Identifier "Word8")),
              extHsSpec = Just
                (HsTypeSpec
                  (Map.fromList
                    [
                      _×_
                        Bits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bounded
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Enum
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Eq
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        FiniteBits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Integral
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ix
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Num
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ord
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Read
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        ReadRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Real
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        StaticSize
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]))})),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_bd8b34eff62c9b12",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCBool))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_bd8b34eff62c9b12",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_nesBool_ptr */\n",
              "__attribute__ ((const))\n",
              "_Bool *hs_bindgen_test_globals_bd8b34eff62c9b12 (void)\n",
              "{\n",
              "  return &nesBool;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypePrim PrimBool),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_235c0c54701b0df5",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              4096
              (HsExtBinding
                ExtRef {
                  extRefModule = ModuleName
                    "HsBindgen.Runtime.Prelude",
                  extRefIdentifier = Identifier
                    "Word8"}
                (CTypeSpec
                  (Just (Identifier "Word8")))
                (Just
                  (HsTypeSpec
                    (Map.fromList
                      [
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]))))))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_235c0c54701b0df5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_streamBinary_ptr */\n",
              "__attribute__ ((const))\n",
              "uint8_t (*hs_bindgen_test_globals_235c0c54701b0df5 (void))[4096]\n",
              "{\n",
              "  return &streamBinary;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypeConstArray
          4096
          (TypeExtBinding
            ResolvedExtBinding {
              extCName = QualName {
                qualNameName = Name "uint8_t",
                qualNameKind =
                NameKindOrdinary},
              extHsRef = ExtRef {
                extRefModule = ModuleName
                  "HsBindgen.Runtime.Prelude",
                extRefIdentifier = Identifier
                  "Word8"},
              extCSpec = CTypeSpec
                (Just (Identifier "Word8")),
              extHsSpec = Just
                (HsTypeSpec
                  (Map.fromList
                    [
                      _×_
                        Bits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bounded
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Enum
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Eq
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        FiniteBits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Integral
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ix
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Num
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ord
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Read
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        ReadRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Real
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        StaticSize
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]))})),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_b1ee511251d335dd",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsExtBinding
              ExtRef {
                extRefModule = ModuleName
                  "HsBindgen.Runtime.Prelude",
                extRefIdentifier = Identifier
                  "Word32"}
              (CTypeSpec
                (Just (Identifier "Word32")))
              (Just
                (HsTypeSpec
                  (Map.fromList
                    [
                      _×_
                        Bits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bounded
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Enum
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Eq
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        FiniteBits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Integral
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ix
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Num
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ord
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Read
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        ReadRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Real
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        StaticSize
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})])))))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_b1ee511251d335dd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_streamBinary_len_ptr */\n",
              "__attribute__ ((const))\n",
              "uint32_t *hs_bindgen_test_globals_b1ee511251d335dd (void)\n",
              "{\n",
              "  return &streamBinary_len;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypeExtBinding
          ResolvedExtBinding {
            extCName = QualName {
              qualNameName = Name "uint32_t",
              qualNameKind =
              NameKindOrdinary},
            extHsRef = ExtRef {
              extRefModule = ModuleName
                "HsBindgen.Runtime.Prelude",
              extRefIdentifier = Identifier
                "Word32"},
            extCSpec = CTypeSpec
              (Just (Identifier "Word32")),
            extHsSpec = Just
              (HsTypeSpec
                (Map.fromList
                  [
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]))}),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_6a47eaca372272ae",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Struct2_t")))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_6a47eaca372272ae",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_some_global_struct_ptr */\n",
              "__attribute__ ((const))\n",
              "struct2_t *hs_bindgen_test_globals_6a47eaca372272ae (void)\n",
              "{\n",
              "  return &some_global_struct;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypeTypedef
          (TypedefSquashed
            (Name "struct2_t")
            (TypeStruct
              NamePair {
                nameC = Name "struct2_t",
                nameHsIdent = Identifier
                  "Struct2_t"}
              (NameOriginGenerated
                (AnonId "globals.h:420:9"))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_b8fe2f6ed308e786",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_b8fe2f6ed308e786",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_globalConstant_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int const *hs_bindgen_test_globals_b8fe2f6ed308e786 (void)\n",
              "{\n",
              "  return &globalConstant;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypeQualified
          TypeQualifierConst
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_3f4a524bb162b165",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "ConstInt")))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_3f4a524bb162b165",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_anotherGlobalConstant_ptr */\n",
              "__attribute__ ((const))\n",
              "ConstInt *hs_bindgen_test_globals_3f4a524bb162b165 (void)\n",
              "{\n",
              "  return &anotherGlobalConstant;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypeTypedef
          (TypedefRegular
            NamePair {
              nameC = Name "ConstInt",
              nameHsIdent = Identifier
                "ConstInt"}
            (TypeQualified
              TypeQualifierConst
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_834271d76b418960",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_834271d76b418960",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_staticConst_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int const *hs_bindgen_test_globals_834271d76b418960 (void)\n",
              "{\n",
              "  return &staticConst;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypeQualified
          TypeQualifierConst
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_c98b4d346a0bb607",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_c98b4d346a0bb607",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_classless_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int const *hs_bindgen_test_globals_c98b4d346a0bb607 (void)\n",
              "{\n",
              "  return &classless;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypeQualified
          TypeQualifierConst
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_e7a61097e2415261",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              4
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_e7a61097e2415261",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_constArray1_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int const (*hs_bindgen_test_globals_e7a61097e2415261 (void))[4]\n",
              "{\n",
              "  return &constArray1;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypeConstArray
          4
          (TypeQualified
            TypeQualifierConst
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_5e76637230135838",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "ConstIntArray")))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_5e76637230135838",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_constArray2_ptr */\n",
              "__attribute__ ((const))\n",
              "ConstIntArray *hs_bindgen_test_globals_5e76637230135838 (void)\n",
              "{\n",
              "  return &constArray2;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypeTypedef
          (TypedefRegular
            NamePair {
              nameC = Name "ConstIntArray",
              nameHsIdent = Identifier
                "ConstIntArray"}
            (TypeIncompleteArray
              (TypeQualified
                TypeQualifierConst
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_c7d3a2347092dfe0",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Tuple")))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_c7d3a2347092dfe0",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_constTuple_ptr */\n",
              "__attribute__ ((const))\n",
              "struct tuple const *hs_bindgen_test_globals_c7d3a2347092dfe0 (void)\n",
              "{\n",
              "  return &constTuple;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypeQualified
          TypeQualifierConst
          (TypeStruct
            NamePair {
              nameC = Name "tuple",
              nameHsIdent = Identifier
                "Tuple"}
            NameOriginInSource)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_3639a5c5b63aad84",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Tuple")))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_3639a5c5b63aad84",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_nonConstTuple_ptr */\n",
              "__attribute__ ((const))\n",
              "struct tuple *hs_bindgen_test_globals_3639a5c5b63aad84 (void)\n",
              "{\n",
              "  return &nonConstTuple;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypeStruct
          NamePair {
            nameC = Name "tuple",
            nameHsIdent = Identifier
              "Tuple"}
          NameOriginInSource),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_9fd2e0d3b265dee4",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPtr
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_9fd2e0d3b265dee4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_ptrToConstInt_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int const **hs_bindgen_test_globals_9fd2e0d3b265dee4 (void)\n",
              "{\n",
              "  return &ptrToConstInt;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypePointer
          (TypeQualified
            TypeQualifierConst
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_10cf1344894b0264",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPtr
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_10cf1344894b0264",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_constPtrToInt_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int *const *hs_bindgen_test_globals_10cf1344894b0264 (void)\n",
              "{\n",
              "  return &constPtrToInt;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypeQualified
          TypeQualifierConst
          (TypePointer
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_globals_1b236183822e0d24",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPtr
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_1b236183822e0d24",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_constPtrToConstInt_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int const *const *hs_bindgen_test_globals_1b236183822e0d24 (void)\n",
              "{\n",
              "  return &constPtrToConstInt;\n",
              "}"],
          capiWrapperImport =
          "globals.h"},
      foreignImportOrigin = Global
        (TypeQualified
          TypeQualifierConst
          (TypePointer
            (TypeQualified
              TypeQualifierConst
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple]
