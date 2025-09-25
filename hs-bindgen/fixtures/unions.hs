[
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Dim2",
      structConstr = Name
        "@NsConstr"
        "Dim2",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "dim2_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:2:9",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "dim2_x"},
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
                "unions.h:2:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["unions.h"],
                  headerInclude = "unions.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "dim2_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:3:9",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier
                    "dim2_y"},
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
                "unions.h:3:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["unions.h"],
                  headerInclude = "unions.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "unions.h:1:8",
            declId = NamePair {
              nameC = Name "Dim2",
              nameHsIdent = Identifier
                "Dim2"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["unions.h"],
                headerInclude = "unions.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Dim2"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:2:9",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "dim2_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:3:9",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "dim2_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
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
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "Dim2",
          commentLocation = Just
            "unions.h:1:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["unions.h"],
              headerInclude = "unions.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Dim2",
          structConstr = Name
            "@NsConstr"
            "Dim2",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "dim2_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:2:9",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "dim2_x"},
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
                    "unions.h:2:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["unions.h"],
                      headerInclude = "unions.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "dim2_y",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:3:9",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "dim2_y"},
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
                    "unions.h:3:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["unions.h"],
                      headerInclude = "unions.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "unions.h:1:8",
                declId = NamePair {
                  nameC = Name "Dim2",
                  nameHsIdent = Identifier
                    "Dim2"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["unions.h"],
                    headerInclude = "unions.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Dim2"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:2:9",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "dim2_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:3:9",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = Identifier
                            "dim2_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
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
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "Dim2",
              commentLocation = Just
                "unions.h:1:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["unions.h"],
                  headerInclude = "unions.h"},
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
                    "Dim2",
                  structConstr = Name
                    "@NsConstr"
                    "Dim2",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dim2_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:2:9",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "dim2_x"},
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
                            "unions.h:2:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dim2_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:3:9",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "dim2_y"},
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
                            "unions.h:3:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:1:8",
                        declId = NamePair {
                          nameC = Name "Dim2",
                          nameHsIdent = Identifier
                            "Dim2"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["unions.h"],
                            headerInclude = "unions.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Dim2"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:2:9",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "dim2_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:3:9",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "dim2_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
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
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "Dim2",
                      commentLocation = Just
                        "unions.h:1:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["unions.h"],
                          headerInclude = "unions.h"},
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
                    "Dim2",
                  structConstr = Name
                    "@NsConstr"
                    "Dim2",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dim2_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:2:9",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "dim2_x"},
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
                            "unions.h:2:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dim2_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:3:9",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "dim2_y"},
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
                            "unions.h:3:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:1:8",
                        declId = NamePair {
                          nameC = Name "Dim2",
                          nameHsIdent = Identifier
                            "Dim2"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["unions.h"],
                            headerInclude = "unions.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Dim2"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:2:9",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "dim2_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:3:9",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "dim2_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
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
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "Dim2",
                      commentLocation = Just
                        "unions.h:1:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["unions.h"],
                          headerInclude = "unions.h"},
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
        "Dim2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Dim2",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Dim3",
      structConstr = Name
        "@NsConstr"
        "Dim3",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "dim3_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:7:9",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "dim3_x"},
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
                "unions.h:7:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["unions.h"],
                  headerInclude = "unions.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "dim3_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:8:9",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier
                    "dim3_y"},
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
                "unions.h:8:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["unions.h"],
                  headerInclude = "unions.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "dim3_z",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:9:9",
                fieldName = NamePair {
                  nameC = Name "z",
                  nameHsIdent = Identifier
                    "dim3_z"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "z",
              commentLocation = Just
                "unions.h:9:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["unions.h"],
                  headerInclude = "unions.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "unions.h:6:8",
            declId = NamePair {
              nameC = Name "Dim3",
              nameHsIdent = Identifier
                "Dim3"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["unions.h"],
                headerInclude = "unions.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Dim3"),
              structSizeof = 12,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:7:9",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "dim3_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:8:9",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "dim3_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:9:9",
                    fieldName = NamePair {
                      nameC = Name "z",
                      nameHsIdent = Identifier
                        "dim3_z"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
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
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "Dim3",
          commentLocation = Just
            "unions.h:6:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["unions.h"],
              headerInclude = "unions.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Dim3",
          structConstr = Name
            "@NsConstr"
            "Dim3",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "dim3_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:7:9",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "dim3_x"},
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
                    "unions.h:7:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["unions.h"],
                      headerInclude = "unions.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "dim3_y",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:8:9",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "dim3_y"},
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
                    "unions.h:8:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["unions.h"],
                      headerInclude = "unions.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "dim3_z",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:9:9",
                    fieldName = NamePair {
                      nameC = Name "z",
                      nameHsIdent = Identifier
                        "dim3_z"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "z",
                  commentLocation = Just
                    "unions.h:9:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["unions.h"],
                      headerInclude = "unions.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "unions.h:6:8",
                declId = NamePair {
                  nameC = Name "Dim3",
                  nameHsIdent = Identifier
                    "Dim3"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["unions.h"],
                    headerInclude = "unions.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Dim3"),
                  structSizeof = 12,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:7:9",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "dim3_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:8:9",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = Identifier
                            "dim3_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:9:9",
                        fieldName = NamePair {
                          nameC = Name "z",
                          nameHsIdent = Identifier
                            "dim3_z"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
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
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "Dim3",
              commentLocation = Just
                "unions.h:6:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["unions.h"],
                  headerInclude = "unions.h"},
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
                    "Dim3",
                  structConstr = Name
                    "@NsConstr"
                    "Dim3",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dim3_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:7:9",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "dim3_x"},
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
                            "unions.h:7:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dim3_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:8:9",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "dim3_y"},
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
                            "unions.h:8:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dim3_z",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:9:9",
                            fieldName = NamePair {
                              nameC = Name "z",
                              nameHsIdent = Identifier
                                "dim3_z"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "z",
                          commentLocation = Just
                            "unions.h:9:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:6:8",
                        declId = NamePair {
                          nameC = Name "Dim3",
                          nameHsIdent = Identifier
                            "Dim3"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["unions.h"],
                            headerInclude = "unions.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Dim3"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:7:9",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "dim3_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:8:9",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "dim3_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:9:9",
                                fieldName = NamePair {
                                  nameC = Name "z",
                                  nameHsIdent = Identifier
                                    "dim3_z"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
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
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "Dim3",
                      commentLocation = Just
                        "unions.h:6:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["unions.h"],
                          headerInclude = "unions.h"},
                      commentChildren = []}})
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
                  structName = Name
                    "@NsTypeConstr"
                    "Dim3",
                  structConstr = Name
                    "@NsConstr"
                    "Dim3",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dim3_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:7:9",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "dim3_x"},
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
                            "unions.h:7:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dim3_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:8:9",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "dim3_y"},
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
                            "unions.h:8:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dim3_z",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:9:9",
                            fieldName = NamePair {
                              nameC = Name "z",
                              nameHsIdent = Identifier
                                "dim3_z"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "z",
                          commentLocation = Just
                            "unions.h:9:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:6:8",
                        declId = NamePair {
                          nameC = Name "Dim3",
                          nameHsIdent = Identifier
                            "Dim3"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["unions.h"],
                            headerInclude = "unions.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Dim3"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:7:9",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "dim3_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:8:9",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "dim3_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:9:9",
                                fieldName = NamePair {
                                  nameC = Name "z",
                                  nameHsIdent = Identifier
                                    "dim3_z"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
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
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "Dim3",
                      commentLocation = Just
                        "unions.h:6:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["unions.h"],
                          headerInclude = "unions.h"},
                      commentChildren = []}}
                (Add 3)
                (Seq
                  [
                    PokeByteOff (Idx 4) 0 (Idx 0),
                    PokeByteOff (Idx 4) 4 (Idx 1),
                    PokeByteOff
                      (Idx 4)
                      8
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
        "Dim3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Dim3",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "DimPayload",
      newtypeConstr = Name
        "@NsConstr"
        "DimPayload",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_DimPayload",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "unions.h:12:7",
          declId = NamePair {
            nameC = Name "DimPayload",
            nameHsIdent = Identifier
              "DimPayload"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["unions.h"],
              headerInclude = "unions.h"},
          declComment = Nothing},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "DimPayload",
              newtypeField = Name
                "@NsVar"
                "un_DimPayload"},
            unionSizeof = 8,
            unionAlignment = 4,
            unionFields = [
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc = "unions.h:13:17",
                  fieldName = NamePair {
                    nameC = Name "dim2",
                    nameHsIdent = Identifier
                      "dimPayload_dim2"},
                  fieldComment = Nothing},
                unionFieldType = TypeStruct
                  NamePair {
                    nameC = Name "Dim2",
                    nameHsIdent = Identifier "Dim2"}
                  NameOriginInSource},
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc = "unions.h:14:17",
                  fieldName = NamePair {
                    nameC = Name "dim3",
                    nameHsIdent = Identifier
                      "dimPayload_dim3"},
                  fieldComment = Nothing},
                unionFieldType = TypeStruct
                  NamePair {
                    nameC = Name "Dim2",
                    nameHsIdent = Identifier "Dim2"}
                  NameOriginInSource}]},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "DimPayload",
          commentLocation = Just
            "unions.h:12:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["unions.h"],
              headerInclude = "unions.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveVia
        (HsSizedByteArray 8 4),
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "DimPayload",
      deriveInstanceComment =
      Nothing},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = Name
        "@NsVar"
        "get_dimPayload_dim2",
      unionGetterType = HsTypRef
        (Name "@NsTypeConstr" "Dim2"),
      unionGetterConstr = Name
        "@NsTypeConstr"
        "DimPayload",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "dim2",
          commentLocation = Just
            "unions.h:13:17",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["unions.h"],
              headerInclude = "unions.h"},
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_dimPayload_dim2"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = Name
        "@NsVar"
        "set_dimPayload_dim2",
      unionSetterType = HsTypRef
        (Name "@NsTypeConstr" "Dim2"),
      unionSetterConstr = Name
        "@NsTypeConstr"
        "DimPayload",
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
                  "get_dimPayload_dim2"]]}},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = Name
        "@NsVar"
        "get_dimPayload_dim3",
      unionGetterType = HsTypRef
        (Name "@NsTypeConstr" "Dim2"),
      unionGetterConstr = Name
        "@NsTypeConstr"
        "DimPayload",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "dim3",
          commentLocation = Just
            "unions.h:14:17",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["unions.h"],
              headerInclude = "unions.h"},
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_dimPayload_dim3"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = Name
        "@NsVar"
        "set_dimPayload_dim3",
      unionSetterType = HsTypRef
        (Name "@NsTypeConstr" "Dim2"),
      unionSetterConstr = Name
        "@NsTypeConstr"
        "DimPayload",
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
                  "get_dimPayload_dim3"]]}},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Dim",
      structConstr = Name
        "@NsConstr"
        "Dim",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "dim_tag",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:18:9",
                fieldName = NamePair {
                  nameC = Name "tag",
                  nameHsIdent = Identifier
                    "dim_tag"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "tag",
              commentLocation = Just
                "unions.h:18:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["unions.h"],
                  headerInclude = "unions.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "dim_payload",
          fieldType = HsTypRef
            (Name
              "@NsTypeConstr"
              "DimPayload"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:19:22",
                fieldName = NamePair {
                  nameC = Name "payload",
                  nameHsIdent = Identifier
                    "dim_payload"},
                fieldComment = Nothing},
              structFieldType = TypeUnion
                NamePair {
                  nameC = Name "DimPayload",
                  nameHsIdent = Identifier
                    "DimPayload"}
                NameOriginInSource,
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "payload",
              commentLocation = Just
                "unions.h:19:22",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["unions.h"],
                  headerInclude = "unions.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "unions.h:17:8",
            declId = NamePair {
              nameC = Name "Dim",
              nameHsIdent = Identifier "Dim"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["unions.h"],
                headerInclude = "unions.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Dim"),
              structSizeof = 12,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:18:9",
                    fieldName = NamePair {
                      nameC = Name "tag",
                      nameHsIdent = Identifier
                        "dim_tag"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:19:22",
                    fieldName = NamePair {
                      nameC = Name "payload",
                      nameHsIdent = Identifier
                        "dim_payload"},
                    fieldComment = Nothing},
                  structFieldType = TypeUnion
                    NamePair {
                      nameC = Name "DimPayload",
                      nameHsIdent = Identifier
                        "DimPayload"}
                    NameOriginInSource,
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
        [Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "Dim",
          commentLocation = Just
            "unions.h:17:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["unions.h"],
              headerInclude = "unions.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Dim",
          structConstr = Name
            "@NsConstr"
            "Dim",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "dim_tag",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:18:9",
                    fieldName = NamePair {
                      nameC = Name "tag",
                      nameHsIdent = Identifier
                        "dim_tag"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "tag",
                  commentLocation = Just
                    "unions.h:18:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["unions.h"],
                      headerInclude = "unions.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "dim_payload",
              fieldType = HsTypRef
                (Name
                  "@NsTypeConstr"
                  "DimPayload"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:19:22",
                    fieldName = NamePair {
                      nameC = Name "payload",
                      nameHsIdent = Identifier
                        "dim_payload"},
                    fieldComment = Nothing},
                  structFieldType = TypeUnion
                    NamePair {
                      nameC = Name "DimPayload",
                      nameHsIdent = Identifier
                        "DimPayload"}
                    NameOriginInSource,
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "payload",
                  commentLocation = Just
                    "unions.h:19:22",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["unions.h"],
                      headerInclude = "unions.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "unions.h:17:8",
                declId = NamePair {
                  nameC = Name "Dim",
                  nameHsIdent = Identifier "Dim"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["unions.h"],
                    headerInclude = "unions.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Dim"),
                  structSizeof = 12,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:18:9",
                        fieldName = NamePair {
                          nameC = Name "tag",
                          nameHsIdent = Identifier
                            "dim_tag"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:19:22",
                        fieldName = NamePair {
                          nameC = Name "payload",
                          nameHsIdent = Identifier
                            "dim_payload"},
                        fieldComment = Nothing},
                      structFieldType = TypeUnion
                        NamePair {
                          nameC = Name "DimPayload",
                          nameHsIdent = Identifier
                            "DimPayload"}
                        NameOriginInSource,
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
            [Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "Dim",
              commentLocation = Just
                "unions.h:17:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["unions.h"],
                  headerInclude = "unions.h"},
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
                    "Dim",
                  structConstr = Name
                    "@NsConstr"
                    "Dim",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dim_tag",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:18:9",
                            fieldName = NamePair {
                              nameC = Name "tag",
                              nameHsIdent = Identifier
                                "dim_tag"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "tag",
                          commentLocation = Just
                            "unions.h:18:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dim_payload",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "DimPayload"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:19:22",
                            fieldName = NamePair {
                              nameC = Name "payload",
                              nameHsIdent = Identifier
                                "dim_payload"},
                            fieldComment = Nothing},
                          structFieldType = TypeUnion
                            NamePair {
                              nameC = Name "DimPayload",
                              nameHsIdent = Identifier
                                "DimPayload"}
                            NameOriginInSource,
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "payload",
                          commentLocation = Just
                            "unions.h:19:22",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:17:8",
                        declId = NamePair {
                          nameC = Name "Dim",
                          nameHsIdent = Identifier "Dim"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["unions.h"],
                            headerInclude = "unions.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Dim"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:18:9",
                                fieldName = NamePair {
                                  nameC = Name "tag",
                                  nameHsIdent = Identifier
                                    "dim_tag"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:19:22",
                                fieldName = NamePair {
                                  nameC = Name "payload",
                                  nameHsIdent = Identifier
                                    "dim_payload"},
                                fieldComment = Nothing},
                              structFieldType = TypeUnion
                                NamePair {
                                  nameC = Name "DimPayload",
                                  nameHsIdent = Identifier
                                    "DimPayload"}
                                NameOriginInSource,
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
                    [Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "Dim",
                      commentLocation = Just
                        "unions.h:17:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["unions.h"],
                          headerInclude = "unions.h"},
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
                    "Dim",
                  structConstr = Name
                    "@NsConstr"
                    "Dim",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dim_tag",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:18:9",
                            fieldName = NamePair {
                              nameC = Name "tag",
                              nameHsIdent = Identifier
                                "dim_tag"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "tag",
                          commentLocation = Just
                            "unions.h:18:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dim_payload",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "DimPayload"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:19:22",
                            fieldName = NamePair {
                              nameC = Name "payload",
                              nameHsIdent = Identifier
                                "dim_payload"},
                            fieldComment = Nothing},
                          structFieldType = TypeUnion
                            NamePair {
                              nameC = Name "DimPayload",
                              nameHsIdent = Identifier
                                "DimPayload"}
                            NameOriginInSource,
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "payload",
                          commentLocation = Just
                            "unions.h:19:22",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:17:8",
                        declId = NamePair {
                          nameC = Name "Dim",
                          nameHsIdent = Identifier "Dim"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["unions.h"],
                            headerInclude = "unions.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Dim"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:18:9",
                                fieldName = NamePair {
                                  nameC = Name "tag",
                                  nameHsIdent = Identifier
                                    "dim_tag"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:19:22",
                                fieldName = NamePair {
                                  nameC = Name "payload",
                                  nameHsIdent = Identifier
                                    "dim_payload"},
                                fieldComment = Nothing},
                              structFieldType = TypeUnion
                                NamePair {
                                  nameC = Name "DimPayload",
                                  nameHsIdent = Identifier
                                    "DimPayload"}
                                NameOriginInSource,
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
                    [Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "Dim",
                      commentLocation = Just
                        "unions.h:17:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["unions.h"],
                          headerInclude = "unions.h"},
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
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "DimPayloadB",
      newtypeConstr = Name
        "@NsConstr"
        "DimPayloadB",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_DimPayloadB",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "unions.h:23:15",
          declId = NamePair {
            nameC = Name "DimPayloadB",
            nameHsIdent = Identifier
              "DimPayloadB"},
          declOrigin = NameOriginInSource,
          declAliases = [
            Name "DimPayloadB"],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["unions.h"],
              headerInclude = "unions.h"},
          declComment = Nothing},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "DimPayloadB",
              newtypeField = Name
                "@NsVar"
                "un_DimPayloadB"},
            unionSizeof = 8,
            unionAlignment = 4,
            unionFields = [
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc = "unions.h:24:17",
                  fieldName = NamePair {
                    nameC = Name "dim2",
                    nameHsIdent = Identifier
                      "dimPayloadB_dim2"},
                  fieldComment = Nothing},
                unionFieldType = TypeStruct
                  NamePair {
                    nameC = Name "Dim2",
                    nameHsIdent = Identifier "Dim2"}
                  NameOriginInSource},
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc = "unions.h:25:17",
                  fieldName = NamePair {
                    nameC = Name "dim3",
                    nameHsIdent = Identifier
                      "dimPayloadB_dim3"},
                  fieldComment = Nothing},
                unionFieldType = TypeStruct
                  NamePair {
                    nameC = Name "Dim2",
                    nameHsIdent = Identifier "Dim2"}
                  NameOriginInSource}]},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "DimPayloadB",
          commentLocation = Just
            "unions.h:23:15",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["unions.h"],
              headerInclude = "unions.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveVia
        (HsSizedByteArray 8 4),
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "DimPayloadB",
      deriveInstanceComment =
      Nothing},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = Name
        "@NsVar"
        "get_dimPayloadB_dim2",
      unionGetterType = HsTypRef
        (Name "@NsTypeConstr" "Dim2"),
      unionGetterConstr = Name
        "@NsTypeConstr"
        "DimPayloadB",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "dim2",
          commentLocation = Just
            "unions.h:24:17",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["unions.h"],
              headerInclude = "unions.h"},
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_dimPayloadB_dim2"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = Name
        "@NsVar"
        "set_dimPayloadB_dim2",
      unionSetterType = HsTypRef
        (Name "@NsTypeConstr" "Dim2"),
      unionSetterConstr = Name
        "@NsTypeConstr"
        "DimPayloadB",
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
                  "get_dimPayloadB_dim2"]]}},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = Name
        "@NsVar"
        "get_dimPayloadB_dim3",
      unionGetterType = HsTypRef
        (Name "@NsTypeConstr" "Dim2"),
      unionGetterConstr = Name
        "@NsTypeConstr"
        "DimPayloadB",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "dim3",
          commentLocation = Just
            "unions.h:25:17",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["unions.h"],
              headerInclude = "unions.h"},
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_dimPayloadB_dim3"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = Name
        "@NsVar"
        "set_dimPayloadB_dim3",
      unionSetterType = HsTypRef
        (Name "@NsTypeConstr" "Dim2"),
      unionSetterConstr = Name
        "@NsTypeConstr"
        "DimPayloadB",
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
                  "get_dimPayloadB_dim3"]]}},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "DimB",
      structConstr = Name
        "@NsConstr"
        "DimB",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "dimB_tag",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:29:9",
                fieldName = NamePair {
                  nameC = Name "tag",
                  nameHsIdent = Identifier
                    "dimB_tag"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "tag",
              commentLocation = Just
                "unions.h:29:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["unions.h"],
                  headerInclude = "unions.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "dimB_payload",
          fieldType = HsTypRef
            (Name
              "@NsTypeConstr"
              "DimPayloadB"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:30:17",
                fieldName = NamePair {
                  nameC = Name "payload",
                  nameHsIdent = Identifier
                    "dimB_payload"},
                fieldComment = Nothing},
              structFieldType = TypeTypedef
                (TypedefSquashed
                  (Name "DimPayloadB")
                  (TypeUnion
                    NamePair {
                      nameC = Name "DimPayloadB",
                      nameHsIdent = Identifier
                        "DimPayloadB"}
                    NameOriginInSource)),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "payload",
              commentLocation = Just
                "unions.h:30:17",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["unions.h"],
                  headerInclude = "unions.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "unions.h:28:8",
            declId = NamePair {
              nameC = Name "DimB",
              nameHsIdent = Identifier
                "DimB"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["unions.h"],
                headerInclude = "unions.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "DimB"),
              structSizeof = 12,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:29:9",
                    fieldName = NamePair {
                      nameC = Name "tag",
                      nameHsIdent = Identifier
                        "dimB_tag"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:30:17",
                    fieldName = NamePair {
                      nameC = Name "payload",
                      nameHsIdent = Identifier
                        "dimB_payload"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name "DimPayloadB")
                      (TypeUnion
                        NamePair {
                          nameC = Name "DimPayloadB",
                          nameHsIdent = Identifier
                            "DimPayloadB"}
                        NameOriginInSource)),
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
        [Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "DimB",
          commentLocation = Just
            "unions.h:28:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["unions.h"],
              headerInclude = "unions.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "DimB",
          structConstr = Name
            "@NsConstr"
            "DimB",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "dimB_tag",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:29:9",
                    fieldName = NamePair {
                      nameC = Name "tag",
                      nameHsIdent = Identifier
                        "dimB_tag"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "tag",
                  commentLocation = Just
                    "unions.h:29:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["unions.h"],
                      headerInclude = "unions.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "dimB_payload",
              fieldType = HsTypRef
                (Name
                  "@NsTypeConstr"
                  "DimPayloadB"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:30:17",
                    fieldName = NamePair {
                      nameC = Name "payload",
                      nameHsIdent = Identifier
                        "dimB_payload"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name "DimPayloadB")
                      (TypeUnion
                        NamePair {
                          nameC = Name "DimPayloadB",
                          nameHsIdent = Identifier
                            "DimPayloadB"}
                        NameOriginInSource)),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "payload",
                  commentLocation = Just
                    "unions.h:30:17",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["unions.h"],
                      headerInclude = "unions.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "unions.h:28:8",
                declId = NamePair {
                  nameC = Name "DimB",
                  nameHsIdent = Identifier
                    "DimB"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["unions.h"],
                    headerInclude = "unions.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "DimB"),
                  structSizeof = 12,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:29:9",
                        fieldName = NamePair {
                          nameC = Name "tag",
                          nameHsIdent = Identifier
                            "dimB_tag"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:30:17",
                        fieldName = NamePair {
                          nameC = Name "payload",
                          nameHsIdent = Identifier
                            "dimB_payload"},
                        fieldComment = Nothing},
                      structFieldType = TypeTypedef
                        (TypedefSquashed
                          (Name "DimPayloadB")
                          (TypeUnion
                            NamePair {
                              nameC = Name "DimPayloadB",
                              nameHsIdent = Identifier
                                "DimPayloadB"}
                            NameOriginInSource)),
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
            [Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "DimB",
              commentLocation = Just
                "unions.h:28:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["unions.h"],
                  headerInclude = "unions.h"},
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
                    "DimB",
                  structConstr = Name
                    "@NsConstr"
                    "DimB",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dimB_tag",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:29:9",
                            fieldName = NamePair {
                              nameC = Name "tag",
                              nameHsIdent = Identifier
                                "dimB_tag"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "tag",
                          commentLocation = Just
                            "unions.h:29:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dimB_payload",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "DimPayloadB"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:30:17",
                            fieldName = NamePair {
                              nameC = Name "payload",
                              nameHsIdent = Identifier
                                "dimB_payload"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefSquashed
                              (Name "DimPayloadB")
                              (TypeUnion
                                NamePair {
                                  nameC = Name "DimPayloadB",
                                  nameHsIdent = Identifier
                                    "DimPayloadB"}
                                NameOriginInSource)),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "payload",
                          commentLocation = Just
                            "unions.h:30:17",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:28:8",
                        declId = NamePair {
                          nameC = Name "DimB",
                          nameHsIdent = Identifier
                            "DimB"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["unions.h"],
                            headerInclude = "unions.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "DimB"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:29:9",
                                fieldName = NamePair {
                                  nameC = Name "tag",
                                  nameHsIdent = Identifier
                                    "dimB_tag"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:30:17",
                                fieldName = NamePair {
                                  nameC = Name "payload",
                                  nameHsIdent = Identifier
                                    "dimB_payload"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefSquashed
                                  (Name "DimPayloadB")
                                  (TypeUnion
                                    NamePair {
                                      nameC = Name "DimPayloadB",
                                      nameHsIdent = Identifier
                                        "DimPayloadB"}
                                    NameOriginInSource)),
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
                    [Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "DimB",
                      commentLocation = Just
                        "unions.h:28:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["unions.h"],
                          headerInclude = "unions.h"},
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
                    "DimB",
                  structConstr = Name
                    "@NsConstr"
                    "DimB",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dimB_tag",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:29:9",
                            fieldName = NamePair {
                              nameC = Name "tag",
                              nameHsIdent = Identifier
                                "dimB_tag"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "tag",
                          commentLocation = Just
                            "unions.h:29:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dimB_payload",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "DimPayloadB"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:30:17",
                            fieldName = NamePair {
                              nameC = Name "payload",
                              nameHsIdent = Identifier
                                "dimB_payload"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefSquashed
                              (Name "DimPayloadB")
                              (TypeUnion
                                NamePair {
                                  nameC = Name "DimPayloadB",
                                  nameHsIdent = Identifier
                                    "DimPayloadB"}
                                NameOriginInSource)),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "payload",
                          commentLocation = Just
                            "unions.h:30:17",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:28:8",
                        declId = NamePair {
                          nameC = Name "DimB",
                          nameHsIdent = Identifier
                            "DimB"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["unions.h"],
                            headerInclude = "unions.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "DimB"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:29:9",
                                fieldName = NamePair {
                                  nameC = Name "tag",
                                  nameHsIdent = Identifier
                                    "dimB_tag"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:30:17",
                                fieldName = NamePair {
                                  nameC = Name "payload",
                                  nameHsIdent = Identifier
                                    "dimB_payload"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefSquashed
                                  (Name "DimPayloadB")
                                  (TypeUnion
                                    NamePair {
                                      nameC = Name "DimPayloadB",
                                      nameHsIdent = Identifier
                                        "DimPayloadB"}
                                    NameOriginInSource)),
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
                    [Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "DimB",
                      commentLocation = Just
                        "unions.h:28:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["unions.h"],
                          headerInclude = "unions.h"},
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
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "AnonA_xy",
      structConstr = Name
        "@NsConstr"
        "AnonA_xy",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "anonA_xy_x",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:35:21",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "anonA_xy_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Just
                "unions.h:35:21",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["unions.h"],
                  headerInclude = "unions.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "anonA_xy_y",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:35:31",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier
                    "anonA_xy_y"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Just
                "unions.h:35:31",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["unions.h"],
                  headerInclude = "unions.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "unions.h:35:5",
            declId = NamePair {
              nameC = Name "AnonA_xy",
              nameHsIdent = Identifier
                "AnonA_xy"},
            declOrigin = NameOriginGenerated
              (AnonId "unions.h:35:5"),
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["unions.h"],
                headerInclude = "unions.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "AnonA_xy"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:35:21",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "anonA_xy_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:35:31",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "anonA_xy_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
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
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Just
            "unions.h:35:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["unions.h"],
              headerInclude = "unions.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "AnonA_xy",
          structConstr = Name
            "@NsConstr"
            "AnonA_xy",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "anonA_xy_x",
              fieldType = HsPrimType
                HsPrimCDouble,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:35:21",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "anonA_xy_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "x",
                  commentLocation = Just
                    "unions.h:35:21",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["unions.h"],
                      headerInclude = "unions.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "anonA_xy_y",
              fieldType = HsPrimType
                HsPrimCDouble,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:35:31",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "anonA_xy_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "y",
                  commentLocation = Just
                    "unions.h:35:31",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["unions.h"],
                      headerInclude = "unions.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "unions.h:35:5",
                declId = NamePair {
                  nameC = Name "AnonA_xy",
                  nameHsIdent = Identifier
                    "AnonA_xy"},
                declOrigin = NameOriginGenerated
                  (AnonId "unions.h:35:5"),
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["unions.h"],
                    headerInclude = "unions.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "AnonA_xy"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:35:21",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "anonA_xy_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimFloating PrimDouble),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:35:31",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = Identifier
                            "anonA_xy_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimFloating PrimDouble),
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
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Nothing,
              commentLocation = Just
                "unions.h:35:5",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["unions.h"],
                  headerInclude = "unions.h"},
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
                    "AnonA_xy",
                  structConstr = Name
                    "@NsConstr"
                    "AnonA_xy",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "anonA_xy_x",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:35:21",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "anonA_xy_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "unions.h:35:21",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "anonA_xy_y",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:35:31",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "anonA_xy_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "unions.h:35:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:35:5",
                        declId = NamePair {
                          nameC = Name "AnonA_xy",
                          nameHsIdent = Identifier
                            "AnonA_xy"},
                        declOrigin = NameOriginGenerated
                          (AnonId "unions.h:35:5"),
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["unions.h"],
                            headerInclude = "unions.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "AnonA_xy"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:35:21",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "anonA_xy_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:35:31",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "anonA_xy_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
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
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "unions.h:35:5",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["unions.h"],
                          headerInclude = "unions.h"},
                      commentChildren = []}})
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
                  structName = Name
                    "@NsTypeConstr"
                    "AnonA_xy",
                  structConstr = Name
                    "@NsConstr"
                    "AnonA_xy",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "anonA_xy_x",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:35:21",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "anonA_xy_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "unions.h:35:21",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "anonA_xy_y",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:35:31",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "anonA_xy_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "unions.h:35:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:35:5",
                        declId = NamePair {
                          nameC = Name "AnonA_xy",
                          nameHsIdent = Identifier
                            "AnonA_xy"},
                        declOrigin = NameOriginGenerated
                          (AnonId "unions.h:35:5"),
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["unions.h"],
                            headerInclude = "unions.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "AnonA_xy"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:35:21",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "anonA_xy_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:35:31",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "anonA_xy_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
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
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "unions.h:35:5",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["unions.h"],
                          headerInclude = "unions.h"},
                      commentChildren = []}}
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
      deriveInstanceName = Name
        "@NsTypeConstr"
        "AnonA_xy",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "AnonA_xy",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "AnonA_polar",
      structConstr = Name
        "@NsConstr"
        "AnonA_polar",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "anonA_polar_r",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:36:21",
                fieldName = NamePair {
                  nameC = Name "r",
                  nameHsIdent = Identifier
                    "anonA_polar_r"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "r",
              commentLocation = Just
                "unions.h:36:21",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["unions.h"],
                  headerInclude = "unions.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "anonA_polar_p",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:36:31",
                fieldName = NamePair {
                  nameC = Name "p",
                  nameHsIdent = Identifier
                    "anonA_polar_p"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "p",
              commentLocation = Just
                "unions.h:36:31",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["unions.h"],
                  headerInclude = "unions.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "unions.h:36:5",
            declId = NamePair {
              nameC = Name "AnonA_polar",
              nameHsIdent = Identifier
                "AnonA_polar"},
            declOrigin = NameOriginGenerated
              (AnonId "unions.h:36:5"),
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["unions.h"],
                headerInclude = "unions.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name
                  "@NsConstr"
                  "AnonA_polar"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:36:21",
                    fieldName = NamePair {
                      nameC = Name "r",
                      nameHsIdent = Identifier
                        "anonA_polar_r"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:36:31",
                    fieldName = NamePair {
                      nameC = Name "p",
                      nameHsIdent = Identifier
                        "anonA_polar_p"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
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
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Just
            "unions.h:36:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["unions.h"],
              headerInclude = "unions.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "AnonA_polar",
          structConstr = Name
            "@NsConstr"
            "AnonA_polar",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "anonA_polar_r",
              fieldType = HsPrimType
                HsPrimCDouble,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:36:21",
                    fieldName = NamePair {
                      nameC = Name "r",
                      nameHsIdent = Identifier
                        "anonA_polar_r"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "r",
                  commentLocation = Just
                    "unions.h:36:21",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["unions.h"],
                      headerInclude = "unions.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "anonA_polar_p",
              fieldType = HsPrimType
                HsPrimCDouble,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:36:31",
                    fieldName = NamePair {
                      nameC = Name "p",
                      nameHsIdent = Identifier
                        "anonA_polar_p"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "p",
                  commentLocation = Just
                    "unions.h:36:31",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["unions.h"],
                      headerInclude = "unions.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "unions.h:36:5",
                declId = NamePair {
                  nameC = Name "AnonA_polar",
                  nameHsIdent = Identifier
                    "AnonA_polar"},
                declOrigin = NameOriginGenerated
                  (AnonId "unions.h:36:5"),
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["unions.h"],
                    headerInclude = "unions.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name
                      "@NsConstr"
                      "AnonA_polar"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:36:21",
                        fieldName = NamePair {
                          nameC = Name "r",
                          nameHsIdent = Identifier
                            "anonA_polar_r"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimFloating PrimDouble),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:36:31",
                        fieldName = NamePair {
                          nameC = Name "p",
                          nameHsIdent = Identifier
                            "anonA_polar_p"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimFloating PrimDouble),
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
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Nothing,
              commentLocation = Just
                "unions.h:36:5",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["unions.h"],
                  headerInclude = "unions.h"},
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
                    "AnonA_polar",
                  structConstr = Name
                    "@NsConstr"
                    "AnonA_polar",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "anonA_polar_r",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:36:21",
                            fieldName = NamePair {
                              nameC = Name "r",
                              nameHsIdent = Identifier
                                "anonA_polar_r"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "r",
                          commentLocation = Just
                            "unions.h:36:21",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "anonA_polar_p",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:36:31",
                            fieldName = NamePair {
                              nameC = Name "p",
                              nameHsIdent = Identifier
                                "anonA_polar_p"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "p",
                          commentLocation = Just
                            "unions.h:36:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:36:5",
                        declId = NamePair {
                          nameC = Name "AnonA_polar",
                          nameHsIdent = Identifier
                            "AnonA_polar"},
                        declOrigin = NameOriginGenerated
                          (AnonId "unions.h:36:5"),
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["unions.h"],
                            headerInclude = "unions.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "AnonA_polar"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:36:21",
                                fieldName = NamePair {
                                  nameC = Name "r",
                                  nameHsIdent = Identifier
                                    "anonA_polar_r"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:36:31",
                                fieldName = NamePair {
                                  nameC = Name "p",
                                  nameHsIdent = Identifier
                                    "anonA_polar_p"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
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
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "unions.h:36:5",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["unions.h"],
                          headerInclude = "unions.h"},
                      commentChildren = []}})
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
                  structName = Name
                    "@NsTypeConstr"
                    "AnonA_polar",
                  structConstr = Name
                    "@NsConstr"
                    "AnonA_polar",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "anonA_polar_r",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:36:21",
                            fieldName = NamePair {
                              nameC = Name "r",
                              nameHsIdent = Identifier
                                "anonA_polar_r"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "r",
                          commentLocation = Just
                            "unions.h:36:21",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "anonA_polar_p",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:36:31",
                            fieldName = NamePair {
                              nameC = Name "p",
                              nameHsIdent = Identifier
                                "anonA_polar_p"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "p",
                          commentLocation = Just
                            "unions.h:36:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["unions.h"],
                              headerInclude = "unions.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:36:5",
                        declId = NamePair {
                          nameC = Name "AnonA_polar",
                          nameHsIdent = Identifier
                            "AnonA_polar"},
                        declOrigin = NameOriginGenerated
                          (AnonId "unions.h:36:5"),
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["unions.h"],
                            headerInclude = "unions.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "AnonA_polar"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:36:21",
                                fieldName = NamePair {
                                  nameC = Name "r",
                                  nameHsIdent = Identifier
                                    "anonA_polar_r"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:36:31",
                                fieldName = NamePair {
                                  nameC = Name "p",
                                  nameHsIdent = Identifier
                                    "anonA_polar_p"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
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
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "unions.h:36:5",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["unions.h"],
                          headerInclude = "unions.h"},
                      commentChildren = []}}
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
      deriveInstanceName = Name
        "@NsTypeConstr"
        "AnonA_polar",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "AnonA_polar",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "AnonA",
      newtypeConstr = Name
        "@NsConstr"
        "AnonA",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_AnonA",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "unions.h:34:7",
          declId = NamePair {
            nameC = Name "AnonA",
            nameHsIdent = Identifier
              "AnonA"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["unions.h"],
              headerInclude = "unions.h"},
          declComment = Nothing},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "AnonA",
              newtypeField = Name
                "@NsVar"
                "un_AnonA"},
            unionSizeof = 16,
            unionAlignment = 8,
            unionFields = [
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc = "unions.h:35:36",
                  fieldName = NamePair {
                    nameC = Name "xy",
                    nameHsIdent = Identifier
                      "anonA_xy"},
                  fieldComment = Nothing},
                unionFieldType = TypeStruct
                  NamePair {
                    nameC = Name "AnonA_xy",
                    nameHsIdent = Identifier
                      "AnonA_xy"}
                  (NameOriginGenerated
                    (AnonId "unions.h:35:5"))},
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc = "unions.h:36:36",
                  fieldName = NamePair {
                    nameC = Name "polar",
                    nameHsIdent = Identifier
                      "anonA_polar"},
                  fieldComment = Nothing},
                unionFieldType = TypeStruct
                  NamePair {
                    nameC = Name "AnonA_polar",
                    nameHsIdent = Identifier
                      "AnonA_polar"}
                  (NameOriginGenerated
                    (AnonId "unions.h:36:5"))}]},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "AnonA",
          commentLocation = Just
            "unions.h:34:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["unions.h"],
              headerInclude = "unions.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveVia
        (HsSizedByteArray 16 8),
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "AnonA",
      deriveInstanceComment =
      Nothing},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = Name
        "@NsVar"
        "get_anonA_xy",
      unionGetterType = HsTypRef
        (Name
          "@NsTypeConstr"
          "AnonA_xy"),
      unionGetterConstr = Name
        "@NsTypeConstr"
        "AnonA",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "xy",
          commentLocation = Just
            "unions.h:35:36",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["unions.h"],
              headerInclude = "unions.h"},
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier "set_anonA_xy"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = Name
        "@NsVar"
        "set_anonA_xy",
      unionSetterType = HsTypRef
        (Name
          "@NsTypeConstr"
          "AnonA_xy"),
      unionSetterConstr = Name
        "@NsTypeConstr"
        "AnonA",
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
                Identifier "get_anonA_xy"]]}},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = Name
        "@NsVar"
        "get_anonA_polar",
      unionGetterType = HsTypRef
        (Name
          "@NsTypeConstr"
          "AnonA_polar"),
      unionGetterConstr = Name
        "@NsTypeConstr"
        "AnonA",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "polar",
          commentLocation = Just
            "unions.h:36:36",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["unions.h"],
              headerInclude = "unions.h"},
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_anonA_polar"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = Name
        "@NsVar"
        "set_anonA_polar",
      unionSetterType = HsTypRef
        (Name
          "@NsTypeConstr"
          "AnonA_polar"),
      unionSetterConstr = Name
        "@NsTypeConstr"
        "AnonA",
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
                  "get_anonA_polar"]]}}]
