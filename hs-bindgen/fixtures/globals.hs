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
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecModule = Nothing,
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
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
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecModule = Nothing,
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
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
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
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
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 4]),
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
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
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
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      4
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
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecModule = Nothing,
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
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
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecModule = Nothing,
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
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
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
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
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 4]),
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
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
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
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      4
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
            CTypeSpec {
              cTypeSpecModule = Just
                (ModuleName
                  "HsBindgen.Runtime.Prelude"),
              cTypeSpecIdentifier = Just
                (Identifier "Word8"),
              cTypeSpecInstances =
              Map.fromList
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
                          ]})]},
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
                  extHsSpec = CTypeSpec {
                    cTypeSpecModule = Just
                      (ModuleName
                        "HsBindgen.Runtime.Prelude"),
                    cTypeSpecIdentifier = Just
                      (Identifier "Word8"),
                    cTypeSpecInstances =
                    Map.fromList
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
                                ]})]}},
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
            CTypeSpec {
              cTypeSpecModule = Just
                (ModuleName
                  "HsBindgen.Runtime.Prelude"),
              cTypeSpecIdentifier = Just
                (Identifier "Word16"),
              cTypeSpecInstances =
              Map.fromList
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
                          ]})]},
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
                  extHsSpec = CTypeSpec {
                    cTypeSpecModule = Just
                      (ModuleName
                        "HsBindgen.Runtime.Prelude"),
                    cTypeSpecIdentifier = Just
                      (Identifier "Word16"),
                    cTypeSpecInstances =
                    Map.fromList
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
                                ]})]}},
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
            CTypeSpec {
              cTypeSpecModule = Just
                (ModuleName
                  "HsBindgen.Runtime.Prelude"),
              cTypeSpecIdentifier = Just
                (Identifier "Word8"),
              cTypeSpecInstances =
              Map.fromList
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
                          ]})]},
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
                  extHsSpec = CTypeSpec {
                    cTypeSpecModule = Just
                      (ModuleName
                        "HsBindgen.Runtime.Prelude"),
                    cTypeSpecIdentifier = Just
                      (Identifier "Word8"),
                    cTypeSpecInstances =
                    Map.fromList
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
                                ]})]}},
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
                      extHsSpec = CTypeSpec {
                        cTypeSpecModule = Just
                          (ModuleName
                            "HsBindgen.Runtime.Prelude"),
                        cTypeSpecIdentifier = Just
                          (Identifier "Word8"),
                        cTypeSpecInstances =
                        Map.fromList
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
                                    ]})]}},
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
                      extHsSpec = CTypeSpec {
                        cTypeSpecModule = Just
                          (ModuleName
                            "HsBindgen.Runtime.Prelude"),
                        cTypeSpecIdentifier = Just
                          (Identifier "Word16"),
                        cTypeSpecInstances =
                        Map.fromList
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
                                    ]})]}},
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
                      extHsSpec = CTypeSpec {
                        cTypeSpecModule = Just
                          (ModuleName
                            "HsBindgen.Runtime.Prelude"),
                        cTypeSpecIdentifier = Just
                          (Identifier "Word8"),
                        cTypeSpecInstances =
                        Map.fromList
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
                                    ]})]}},
                  structFieldOffset = 32,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecModule = Nothing,
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
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
                CTypeSpec {
                  cTypeSpecModule = Just
                    (ModuleName
                      "HsBindgen.Runtime.Prelude"),
                  cTypeSpecIdentifier = Just
                    (Identifier "Word8"),
                  cTypeSpecInstances =
                  Map.fromList
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
                              ]})]},
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
                      extHsSpec = CTypeSpec {
                        cTypeSpecModule = Just
                          (ModuleName
                            "HsBindgen.Runtime.Prelude"),
                        cTypeSpecIdentifier = Just
                          (Identifier "Word8"),
                        cTypeSpecInstances =
                        Map.fromList
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
                                    ]})]}},
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
                CTypeSpec {
                  cTypeSpecModule = Just
                    (ModuleName
                      "HsBindgen.Runtime.Prelude"),
                  cTypeSpecIdentifier = Just
                    (Identifier "Word16"),
                  cTypeSpecInstances =
                  Map.fromList
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
                              ]})]},
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
                      extHsSpec = CTypeSpec {
                        cTypeSpecModule = Just
                          (ModuleName
                            "HsBindgen.Runtime.Prelude"),
                        cTypeSpecIdentifier = Just
                          (Identifier "Word16"),
                        cTypeSpecInstances =
                        Map.fromList
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
                                    ]})]}},
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
                CTypeSpec {
                  cTypeSpecModule = Just
                    (ModuleName
                      "HsBindgen.Runtime.Prelude"),
                  cTypeSpecIdentifier = Just
                    (Identifier "Word8"),
                  cTypeSpecInstances =
                  Map.fromList
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
                              ]})]},
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
                      extHsSpec = CTypeSpec {
                        cTypeSpecModule = Just
                          (ModuleName
                            "HsBindgen.Runtime.Prelude"),
                        cTypeSpecIdentifier = Just
                          (Identifier "Word8"),
                        cTypeSpecInstances =
                        Map.fromList
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
                                    ]})]}},
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
                          extHsSpec = CTypeSpec {
                            cTypeSpecModule = Just
                              (ModuleName
                                "HsBindgen.Runtime.Prelude"),
                            cTypeSpecIdentifier = Just
                              (Identifier "Word8"),
                            cTypeSpecInstances =
                            Map.fromList
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
                                        ]})]}},
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
                          extHsSpec = CTypeSpec {
                            cTypeSpecModule = Just
                              (ModuleName
                                "HsBindgen.Runtime.Prelude"),
                            cTypeSpecIdentifier = Just
                              (Identifier "Word16"),
                            cTypeSpecInstances =
                            Map.fromList
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
                                        ]})]}},
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
                          extHsSpec = CTypeSpec {
                            cTypeSpecModule = Just
                              (ModuleName
                                "HsBindgen.Runtime.Prelude"),
                            cTypeSpecIdentifier = Just
                              (Identifier "Word8"),
                            cTypeSpecInstances =
                            Map.fromList
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
                                        ]})]}},
                      structFieldOffset = 32,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecModule = Nothing,
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
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
                        CTypeSpec {
                          cTypeSpecModule = Just
                            (ModuleName
                              "HsBindgen.Runtime.Prelude"),
                          cTypeSpecIdentifier = Just
                            (Identifier "Word8"),
                          cTypeSpecInstances =
                          Map.fromList
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
                                      ]})]},
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
                              extHsSpec = CTypeSpec {
                                cTypeSpecModule = Just
                                  (ModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                cTypeSpecIdentifier = Just
                                  (Identifier "Word8"),
                                cTypeSpecInstances =
                                Map.fromList
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
                                            ]})]}},
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
                        CTypeSpec {
                          cTypeSpecModule = Just
                            (ModuleName
                              "HsBindgen.Runtime.Prelude"),
                          cTypeSpecIdentifier = Just
                            (Identifier "Word16"),
                          cTypeSpecInstances =
                          Map.fromList
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
                                      ]})]},
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
                              extHsSpec = CTypeSpec {
                                cTypeSpecModule = Just
                                  (ModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                cTypeSpecIdentifier = Just
                                  (Identifier "Word16"),
                                cTypeSpecInstances =
                                Map.fromList
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
                                            ]})]}},
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
                        CTypeSpec {
                          cTypeSpecModule = Just
                            (ModuleName
                              "HsBindgen.Runtime.Prelude"),
                          cTypeSpecIdentifier = Just
                            (Identifier "Word8"),
                          cTypeSpecInstances =
                          Map.fromList
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
                                      ]})]},
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
                              extHsSpec = CTypeSpec {
                                cTypeSpecModule = Just
                                  (ModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                cTypeSpecIdentifier = Just
                                  (Identifier "Word8"),
                                cTypeSpecInstances =
                                Map.fromList
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
                                            ]})]}},
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
                                  extHsSpec = CTypeSpec {
                                    cTypeSpecModule = Just
                                      (ModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    cTypeSpecIdentifier = Just
                                      (Identifier "Word8"),
                                    cTypeSpecInstances =
                                    Map.fromList
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
                                                ]})]}},
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
                                  extHsSpec = CTypeSpec {
                                    cTypeSpecModule = Just
                                      (ModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    cTypeSpecIdentifier = Just
                                      (Identifier "Word16"),
                                    cTypeSpecInstances =
                                    Map.fromList
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
                                                ]})]}},
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
                                  extHsSpec = CTypeSpec {
                                    cTypeSpecModule = Just
                                      (ModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    cTypeSpecIdentifier = Just
                                      (Identifier "Word8"),
                                    cTypeSpecInstances =
                                    Map.fromList
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
                                                ]})]}},
                              structFieldOffset = 32,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
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
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 2,
                PeekByteOff (Idx 0) 4]),
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
                        CTypeSpec {
                          cTypeSpecModule = Just
                            (ModuleName
                              "HsBindgen.Runtime.Prelude"),
                          cTypeSpecIdentifier = Just
                            (Identifier "Word8"),
                          cTypeSpecInstances =
                          Map.fromList
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
                                      ]})]},
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
                              extHsSpec = CTypeSpec {
                                cTypeSpecModule = Just
                                  (ModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                cTypeSpecIdentifier = Just
                                  (Identifier "Word8"),
                                cTypeSpecInstances =
                                Map.fromList
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
                                            ]})]}},
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
                        CTypeSpec {
                          cTypeSpecModule = Just
                            (ModuleName
                              "HsBindgen.Runtime.Prelude"),
                          cTypeSpecIdentifier = Just
                            (Identifier "Word16"),
                          cTypeSpecInstances =
                          Map.fromList
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
                                      ]})]},
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
                              extHsSpec = CTypeSpec {
                                cTypeSpecModule = Just
                                  (ModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                cTypeSpecIdentifier = Just
                                  (Identifier "Word16"),
                                cTypeSpecInstances =
                                Map.fromList
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
                                            ]})]}},
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
                        CTypeSpec {
                          cTypeSpecModule = Just
                            (ModuleName
                              "HsBindgen.Runtime.Prelude"),
                          cTypeSpecIdentifier = Just
                            (Identifier "Word8"),
                          cTypeSpecInstances =
                          Map.fromList
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
                                      ]})]},
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
                              extHsSpec = CTypeSpec {
                                cTypeSpecModule = Just
                                  (ModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                cTypeSpecIdentifier = Just
                                  (Identifier "Word8"),
                                cTypeSpecInstances =
                                Map.fromList
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
                                            ]})]}},
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
                                  extHsSpec = CTypeSpec {
                                    cTypeSpecModule = Just
                                      (ModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    cTypeSpecIdentifier = Just
                                      (Identifier "Word8"),
                                    cTypeSpecInstances =
                                    Map.fromList
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
                                                ]})]}},
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
                                  extHsSpec = CTypeSpec {
                                    cTypeSpecModule = Just
                                      (ModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    cTypeSpecIdentifier = Just
                                      (Identifier "Word16"),
                                    cTypeSpecInstances =
                                    Map.fromList
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
                                                ]})]}},
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
                                  extHsSpec = CTypeSpec {
                                    cTypeSpecModule = Just
                                      (ModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    cTypeSpecIdentifier = Just
                                      (Identifier "Word8"),
                                    cTypeSpecInstances =
                                    Map.fromList
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
                                                ]})]}},
                              structFieldOffset = 32,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
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
                    PokeByteOff (Idx 4) 0 (Idx 0),
                    PokeByteOff (Idx 4) 2 (Idx 1),
                    PokeByteOff
                      (Idx 4)
                      4
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
            CTypeSpec {
              cTypeSpecModule = Just
                (ModuleName
                  "HsBindgen.Runtime.Prelude"),
              cTypeSpecIdentifier = Just
                (Identifier "Word16"),
              cTypeSpecInstances =
              Map.fromList
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
                          ]})]},
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
                  extHsSpec = CTypeSpec {
                    cTypeSpecModule = Just
                      (ModuleName
                        "HsBindgen.Runtime.Prelude"),
                    cTypeSpecIdentifier = Just
                      (Identifier "Word16"),
                    cTypeSpecInstances =
                    Map.fromList
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
                                ]})]}},
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
                      extHsSpec = CTypeSpec {
                        cTypeSpecModule = Just
                          (ModuleName
                            "HsBindgen.Runtime.Prelude"),
                        cTypeSpecIdentifier = Just
                          (Identifier "Word16"),
                        cTypeSpecInstances =
                        Map.fromList
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
                                    ]})]}},
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
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecModule = Nothing,
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
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
                CTypeSpec {
                  cTypeSpecModule = Just
                    (ModuleName
                      "HsBindgen.Runtime.Prelude"),
                  cTypeSpecIdentifier = Just
                    (Identifier "Word16"),
                  cTypeSpecInstances =
                  Map.fromList
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
                              ]})]},
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
                      extHsSpec = CTypeSpec {
                        cTypeSpecModule = Just
                          (ModuleName
                            "HsBindgen.Runtime.Prelude"),
                        cTypeSpecIdentifier = Just
                          (Identifier "Word16"),
                        cTypeSpecInstances =
                        Map.fromList
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
                                    ]})]}},
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
                          extHsSpec = CTypeSpec {
                            cTypeSpecModule = Just
                              (ModuleName
                                "HsBindgen.Runtime.Prelude"),
                            cTypeSpecIdentifier = Just
                              (Identifier "Word16"),
                            cTypeSpecInstances =
                            Map.fromList
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
                                        ]})]}},
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
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecModule = Nothing,
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
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
                        CTypeSpec {
                          cTypeSpecModule = Just
                            (ModuleName
                              "HsBindgen.Runtime.Prelude"),
                          cTypeSpecIdentifier = Just
                            (Identifier "Word16"),
                          cTypeSpecInstances =
                          Map.fromList
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
                                      ]})]},
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
                              extHsSpec = CTypeSpec {
                                cTypeSpecModule = Just
                                  (ModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                cTypeSpecIdentifier = Just
                                  (Identifier "Word16"),
                                cTypeSpecInstances =
                                Map.fromList
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
                                            ]})]}},
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
                                  extHsSpec = CTypeSpec {
                                    cTypeSpecModule = Just
                                      (ModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    cTypeSpecIdentifier = Just
                                      (Identifier "Word16"),
                                    cTypeSpecInstances =
                                    Map.fromList
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
                                                ]})]}},
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
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
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
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 2,
                PeekByteOff (Idx 0) 4]),
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
                        CTypeSpec {
                          cTypeSpecModule = Just
                            (ModuleName
                              "HsBindgen.Runtime.Prelude"),
                          cTypeSpecIdentifier = Just
                            (Identifier "Word16"),
                          cTypeSpecInstances =
                          Map.fromList
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
                                      ]})]},
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
                              extHsSpec = CTypeSpec {
                                cTypeSpecModule = Just
                                  (ModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                cTypeSpecIdentifier = Just
                                  (Identifier "Word16"),
                                cTypeSpecInstances =
                                Map.fromList
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
                                            ]})]}},
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
                                  extHsSpec = CTypeSpec {
                                    cTypeSpecModule = Just
                                      (ModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    cTypeSpecIdentifier = Just
                                      (Identifier "Word16"),
                                    cTypeSpecInstances =
                                    Map.fromList
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
                                                ]})]}},
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
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
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
                    PokeByteOff (Idx 4) 0 (Idx 0),
                    PokeByteOff (Idx 4) 2 (Idx 1),
                    PokeByteOff
                      (Idx 4)
                      4
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
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecModule = Nothing,
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
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
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecModule = Nothing,
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
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
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
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
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
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
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
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
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
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
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecModule = Nothing,
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
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
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecModule = Nothing,
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
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
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
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
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 4]),
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
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
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
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      4
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
          capiWrapperDefinition =
          "/* get_simpleGlobal_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_454b1c1a1b7d5b60 (void) { return &simpleGlobal; } ",
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
          "/* get_compoundGlobal1_ptr */ __attribute__ ((const)) struct config *hs_bindgen_test_globals_89495849c6dfe834 (void) { return &compoundGlobal1; } ",
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
          "/* get_compoundGlobal2_ptr */ __attribute__ ((const)) struct inline_struct *hs_bindgen_test_globals_4b79fc96cf429b3a (void) { return &compoundGlobal2; } ",
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
          capiWrapperDefinition =
          "/* get_nesInteger_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_e408a99291c13956 (void) { return &nesInteger; } ",
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
          capiWrapperDefinition =
          "/* get_nesFloating_ptr */ __attribute__ ((const)) float *hs_bindgen_test_globals_572819a2ae538f9e (void) { return &nesFloating; } ",
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
          capiWrapperDefinition =
          "/* get_nesString1_ptr */ __attribute__ ((const)) char **hs_bindgen_test_globals_b9c5b2253881699a (void) { return &nesString1; } ",
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
          capiWrapperDefinition =
          "/* get_nesString2_ptr */ __attribute__ ((const)) char (*hs_bindgen_test_globals_cea7b6d95dd81460 (void))[3] { return &nesString2; } ",
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
          capiWrapperDefinition =
          "/* get_nesCharacter_ptr */ __attribute__ ((const)) char *hs_bindgen_test_globals_1e474dd7b5a92e6a (void) { return &nesCharacter; } ",
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
          capiWrapperDefinition =
          "/* get_nesParen_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_5e7165450285240d (void) { return &nesParen; } ",
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
          capiWrapperDefinition =
          "/* get_nesUnary_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_f00692845b6c8efd (void) { return &nesUnary; } ",
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
          capiWrapperDefinition =
          "/* get_nesBinary_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_d737cdc355737c2b (void) { return &nesBinary; } ",
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
          capiWrapperDefinition =
          "/* get_nesConditional_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_a8a7382ba67a6c7d (void) { return &nesConditional; } ",
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
          capiWrapperDefinition =
          "/* get_nesCast_ptr */ __attribute__ ((const)) float *hs_bindgen_test_globals_ea6b1f97253ae23c (void) { return &nesCast; } ",
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
          capiWrapperDefinition =
          "/* get_nesCompound_ptr */ __attribute__ ((const)) signed int **hs_bindgen_test_globals_34ed05675f69bccd (void) { return &nesCompound; } ",
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
                CTypeSpec {
                  cTypeSpecModule = Just
                    (ModuleName
                      "HsBindgen.Runtime.Prelude"),
                  cTypeSpecIdentifier = Just
                    (Identifier "Word8"),
                  cTypeSpecInstances =
                  Map.fromList
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
                              ]})]})))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_9e7c70b133f8e175",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_nesInitList_ptr */ __attribute__ ((const)) uint8_t (*hs_bindgen_test_globals_9e7c70b133f8e175 (void))[4] { return &nesInitList; } ",
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
              extHsSpec = CTypeSpec {
                cTypeSpecModule = Just
                  (ModuleName
                    "HsBindgen.Runtime.Prelude"),
                cTypeSpecIdentifier = Just
                  (Identifier "Word8"),
                cTypeSpecInstances =
                Map.fromList
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
                            ]})]}})),
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
          capiWrapperDefinition =
          "/* get_nesBool_ptr */ __attribute__ ((const)) _Bool *hs_bindgen_test_globals_bd8b34eff62c9b12 (void) { return &nesBool; } ",
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
                CTypeSpec {
                  cTypeSpecModule = Just
                    (ModuleName
                      "HsBindgen.Runtime.Prelude"),
                  cTypeSpecIdentifier = Just
                    (Identifier "Word8"),
                  cTypeSpecInstances =
                  Map.fromList
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
                              ]})]})))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_235c0c54701b0df5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_streamBinary_ptr */ __attribute__ ((const)) uint8_t (*hs_bindgen_test_globals_235c0c54701b0df5 (void))[4096] { return &streamBinary; } ",
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
              extHsSpec = CTypeSpec {
                cTypeSpecModule = Just
                  (ModuleName
                    "HsBindgen.Runtime.Prelude"),
                cTypeSpecIdentifier = Just
                  (Identifier "Word8"),
                cTypeSpecInstances =
                Map.fromList
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
                            ]})]}})),
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
              CTypeSpec {
                cTypeSpecModule = Just
                  (ModuleName
                    "HsBindgen.Runtime.Prelude"),
                cTypeSpecIdentifier = Just
                  (Identifier "Word32"),
                cTypeSpecInstances =
                Map.fromList
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
                            ]})]}))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_b1ee511251d335dd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_streamBinary_len_ptr */ __attribute__ ((const)) uint32_t *hs_bindgen_test_globals_b1ee511251d335dd (void) { return &streamBinary_len; } ",
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
            extHsSpec = CTypeSpec {
              cTypeSpecModule = Just
                (ModuleName
                  "HsBindgen.Runtime.Prelude"),
              cTypeSpecIdentifier = Just
                (Identifier "Word32"),
              cTypeSpecInstances =
              Map.fromList
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
                          ]})]}}),
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
          capiWrapperDefinition =
          "/* get_some_global_struct_ptr */ __attribute__ ((const)) struct2_t *hs_bindgen_test_globals_6a47eaca372272ae (void) { return &some_global_struct; } ",
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
          "/* get_globalConstant_ptr */ __attribute__ ((const)) signed int const *hs_bindgen_test_globals_b8fe2f6ed308e786 (void) { return &globalConstant; } ",
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
          capiWrapperDefinition =
          "/* get_anotherGlobalConstant_ptr */ __attribute__ ((const)) ConstInt *hs_bindgen_test_globals_3f4a524bb162b165 (void) { return &anotherGlobalConstant; } ",
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
          "/* get_staticConst_ptr */ __attribute__ ((const)) signed int const *hs_bindgen_test_globals_834271d76b418960 (void) { return &staticConst; } ",
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
          "/* get_classless_ptr */ __attribute__ ((const)) signed int const *hs_bindgen_test_globals_c98b4d346a0bb607 (void) { return &classless; } ",
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
          "/* get_constArray1_ptr */ __attribute__ ((const)) signed int const (*hs_bindgen_test_globals_e7a61097e2415261 (void))[4] { return &constArray1; } ",
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
          "/* get_constArray2_ptr */ __attribute__ ((const)) ConstIntArray *hs_bindgen_test_globals_5e76637230135838 (void) { return &constArray2; } ",
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
          "/* get_constTuple_ptr */ __attribute__ ((const)) struct tuple const *hs_bindgen_test_globals_c7d3a2347092dfe0 (void) { return &constTuple; } ",
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
          capiWrapperDefinition =
          "/* get_nonConstTuple_ptr */ __attribute__ ((const)) struct tuple *hs_bindgen_test_globals_3639a5c5b63aad84 (void) { return &nonConstTuple; } ",
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
          "/* get_ptrToConstInt_ptr */ __attribute__ ((const)) signed int const **hs_bindgen_test_globals_9fd2e0d3b265dee4 (void) { return &ptrToConstInt; } ",
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
          "/* get_constPtrToInt_ptr */ __attribute__ ((const)) signed int *const *hs_bindgen_test_globals_10cf1344894b0264 (void) { return &constPtrToInt; } ",
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
          "/* get_constPtrToConstInt_ptr */ __attribute__ ((const)) signed int const *const *hs_bindgen_test_globals_1b236183822e0d24 (void) { return &constPtrToConstInt; } ",
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
