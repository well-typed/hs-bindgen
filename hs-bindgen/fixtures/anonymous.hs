[
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "S1_c",
      structConstr = Name
        "@NsConstr"
        "S1_c",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "s1_c_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "anonymous.h:4:9",
                fieldName = NamePair {
                  nameC = Name "a",
                  nameHsIdent = Identifier
                    "s1_c_a"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "a",
              commentLocation = Just
                "anonymous.h:4:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["anonymous.h"],
                  headerInclude = "anonymous.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "s1_c_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "anonymous.h:5:9",
                fieldName = NamePair {
                  nameC = Name "b",
                  nameHsIdent = Identifier
                    "s1_c_b"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "b",
              commentLocation = Just
                "anonymous.h:5:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["anonymous.h"],
                  headerInclude = "anonymous.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "anonymous.h:3:3",
            declId = NamePair {
              nameC = Name "S1_c",
              nameHsIdent = Identifier
                "S1_c"},
            declOrigin = NameOriginGenerated
              (AnonId "anonymous.h:3:3"),
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["anonymous.h"],
                headerInclude = "anonymous.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "S1_c"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:4:9",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s1_c_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:5:9",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier
                        "s1_c_b"},
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
          commentOrigin = Nothing,
          commentLocation = Just
            "anonymous.h:3:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["anonymous.h"],
              headerInclude = "anonymous.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "S1_c",
          structConstr = Name
            "@NsConstr"
            "S1_c",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "s1_c_a",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:4:9",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s1_c_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "a",
                  commentLocation = Just
                    "anonymous.h:4:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["anonymous.h"],
                      headerInclude = "anonymous.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "s1_c_b",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:5:9",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier
                        "s1_c_b"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "b",
                  commentLocation = Just
                    "anonymous.h:5:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["anonymous.h"],
                      headerInclude = "anonymous.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "anonymous.h:3:3",
                declId = NamePair {
                  nameC = Name "S1_c",
                  nameHsIdent = Identifier
                    "S1_c"},
                declOrigin = NameOriginGenerated
                  (AnonId "anonymous.h:3:3"),
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["anonymous.h"],
                    headerInclude = "anonymous.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "S1_c"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "anonymous.h:4:9",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = Identifier
                            "s1_c_a"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "anonymous.h:5:9",
                        fieldName = NamePair {
                          nameC = Name "b",
                          nameHsIdent = Identifier
                            "s1_c_b"},
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
              commentOrigin = Nothing,
              commentLocation = Just
                "anonymous.h:3:3",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["anonymous.h"],
                  headerInclude = "anonymous.h"},
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
                    "S1_c",
                  structConstr = Name
                    "@NsConstr"
                    "S1_c",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s1_c_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:4:9",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s1_c_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "anonymous.h:4:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s1_c_b",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:5:9",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier
                                "s1_c_b"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "b",
                          commentLocation = Just
                            "anonymous.h:5:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "anonymous.h:3:3",
                        declId = NamePair {
                          nameC = Name "S1_c",
                          nameHsIdent = Identifier
                            "S1_c"},
                        declOrigin = NameOriginGenerated
                          (AnonId "anonymous.h:3:3"),
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["anonymous.h"],
                            headerInclude = "anonymous.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S1_c"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:4:9",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s1_c_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:5:9",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier
                                    "s1_c_b"},
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
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "anonymous.h:3:3",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["anonymous.h"],
                          headerInclude = "anonymous.h"},
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
                    "S1_c",
                  structConstr = Name
                    "@NsConstr"
                    "S1_c",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s1_c_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:4:9",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s1_c_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "anonymous.h:4:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s1_c_b",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:5:9",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier
                                "s1_c_b"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "b",
                          commentLocation = Just
                            "anonymous.h:5:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "anonymous.h:3:3",
                        declId = NamePair {
                          nameC = Name "S1_c",
                          nameHsIdent = Identifier
                            "S1_c"},
                        declOrigin = NameOriginGenerated
                          (AnonId "anonymous.h:3:3"),
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["anonymous.h"],
                            headerInclude = "anonymous.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S1_c"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:4:9",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s1_c_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:5:9",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier
                                    "s1_c_b"},
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
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "anonymous.h:3:3",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["anonymous.h"],
                          headerInclude = "anonymous.h"},
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
        "S1_c",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S1_c",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "S1",
      structConstr = Name
        "@NsConstr"
        "S1",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "s1_c",
          fieldType = HsTypRef
            (Name "@NsTypeConstr" "S1_c"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "anonymous.h:6:5",
                fieldName = NamePair {
                  nameC = Name "c",
                  nameHsIdent = Identifier
                    "s1_c"},
                fieldComment = Nothing},
              structFieldType = TypeStruct
                NamePair {
                  nameC = Name "S1_c",
                  nameHsIdent = Identifier "S1_c"}
                (NameOriginGenerated
                  (AnonId "anonymous.h:3:3")),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "c",
              commentLocation = Just
                "anonymous.h:6:5",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["anonymous.h"],
                  headerInclude = "anonymous.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "s1_d",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "anonymous.h:8:7",
                fieldName = NamePair {
                  nameC = Name "d",
                  nameHsIdent = Identifier
                    "s1_d"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "d",
              commentLocation = Just
                "anonymous.h:8:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["anonymous.h"],
                  headerInclude = "anonymous.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "anonymous.h:2:8",
            declId = NamePair {
              nameC = Name "S1",
              nameHsIdent = Identifier "S1"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["anonymous.h"],
                headerInclude = "anonymous.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "S1"),
              structSizeof = 12,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:6:5",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = Identifier
                        "s1_c"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "S1_c",
                      nameHsIdent = Identifier "S1_c"}
                    (NameOriginGenerated
                      (AnonId "anonymous.h:3:3")),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:8:7",
                    fieldName = NamePair {
                      nameC = Name "d",
                      nameHsIdent = Identifier
                        "s1_d"},
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
          commentOrigin = Just "S1",
          commentLocation = Just
            "anonymous.h:2:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["anonymous.h"],
              headerInclude = "anonymous.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "S1",
          structConstr = Name
            "@NsConstr"
            "S1",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "s1_c",
              fieldType = HsTypRef
                (Name "@NsTypeConstr" "S1_c"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:6:5",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = Identifier
                        "s1_c"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "S1_c",
                      nameHsIdent = Identifier "S1_c"}
                    (NameOriginGenerated
                      (AnonId "anonymous.h:3:3")),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "c",
                  commentLocation = Just
                    "anonymous.h:6:5",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["anonymous.h"],
                      headerInclude = "anonymous.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "s1_d",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:8:7",
                    fieldName = NamePair {
                      nameC = Name "d",
                      nameHsIdent = Identifier
                        "s1_d"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "d",
                  commentLocation = Just
                    "anonymous.h:8:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["anonymous.h"],
                      headerInclude = "anonymous.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "anonymous.h:2:8",
                declId = NamePair {
                  nameC = Name "S1",
                  nameHsIdent = Identifier "S1"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["anonymous.h"],
                    headerInclude = "anonymous.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "S1"),
                  structSizeof = 12,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "anonymous.h:6:5",
                        fieldName = NamePair {
                          nameC = Name "c",
                          nameHsIdent = Identifier
                            "s1_c"},
                        fieldComment = Nothing},
                      structFieldType = TypeStruct
                        NamePair {
                          nameC = Name "S1_c",
                          nameHsIdent = Identifier "S1_c"}
                        (NameOriginGenerated
                          (AnonId "anonymous.h:3:3")),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "anonymous.h:8:7",
                        fieldName = NamePair {
                          nameC = Name "d",
                          nameHsIdent = Identifier
                            "s1_d"},
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
              commentOrigin = Just "S1",
              commentLocation = Just
                "anonymous.h:2:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["anonymous.h"],
                  headerInclude = "anonymous.h"},
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
                    "S1",
                  structConstr = Name
                    "@NsConstr"
                    "S1",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s1_c",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "S1_c"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:6:5",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = Identifier
                                "s1_c"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "S1_c",
                              nameHsIdent = Identifier "S1_c"}
                            (NameOriginGenerated
                              (AnonId "anonymous.h:3:3")),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "anonymous.h:6:5",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s1_d",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:8:7",
                            fieldName = NamePair {
                              nameC = Name "d",
                              nameHsIdent = Identifier
                                "s1_d"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "d",
                          commentLocation = Just
                            "anonymous.h:8:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "anonymous.h:2:8",
                        declId = NamePair {
                          nameC = Name "S1",
                          nameHsIdent = Identifier "S1"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["anonymous.h"],
                            headerInclude = "anonymous.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S1"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:6:5",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = Identifier
                                    "s1_c"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "S1_c",
                                  nameHsIdent = Identifier "S1_c"}
                                (NameOriginGenerated
                                  (AnonId "anonymous.h:3:3")),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:8:7",
                                fieldName = NamePair {
                                  nameC = Name "d",
                                  nameHsIdent = Identifier
                                    "s1_d"},
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
                      commentOrigin = Just "S1",
                      commentLocation = Just
                        "anonymous.h:2:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["anonymous.h"],
                          headerInclude = "anonymous.h"},
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
                    "S1",
                  structConstr = Name
                    "@NsConstr"
                    "S1",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s1_c",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "S1_c"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:6:5",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = Identifier
                                "s1_c"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "S1_c",
                              nameHsIdent = Identifier "S1_c"}
                            (NameOriginGenerated
                              (AnonId "anonymous.h:3:3")),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "anonymous.h:6:5",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s1_d",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:8:7",
                            fieldName = NamePair {
                              nameC = Name "d",
                              nameHsIdent = Identifier
                                "s1_d"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "d",
                          commentLocation = Just
                            "anonymous.h:8:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "anonymous.h:2:8",
                        declId = NamePair {
                          nameC = Name "S1",
                          nameHsIdent = Identifier "S1"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["anonymous.h"],
                            headerInclude = "anonymous.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S1"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:6:5",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = Identifier
                                    "s1_c"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "S1_c",
                                  nameHsIdent = Identifier "S1_c"}
                                (NameOriginGenerated
                                  (AnonId "anonymous.h:3:3")),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:8:7",
                                fieldName = NamePair {
                                  nameC = Name "d",
                                  nameHsIdent = Identifier
                                    "s1_d"},
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
                      commentOrigin = Just "S1",
                      commentLocation = Just
                        "anonymous.h:2:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["anonymous.h"],
                          headerInclude = "anonymous.h"},
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
        "S1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S1",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "S2_inner_deep",
      structConstr = Name
        "@NsConstr"
        "S2_inner_deep",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "s2_inner_deep_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "anonymous.h:16:11",
                fieldName = NamePair {
                  nameC = Name "b",
                  nameHsIdent = Identifier
                    "s2_inner_deep_b"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "b",
              commentLocation = Just
                "anonymous.h:16:11",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["anonymous.h"],
                  headerInclude = "anonymous.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "anonymous.h:15:5",
            declId = NamePair {
              nameC = Name "S2_inner_deep",
              nameHsIdent = Identifier
                "S2_inner_deep"},
            declOrigin = NameOriginGenerated
              (AnonId "anonymous.h:15:5"),
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["anonymous.h"],
                headerInclude = "anonymous.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name
                  "@NsConstr"
                  "S2_inner_deep"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:16:11",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier
                        "s2_inner_deep_b"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
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
            "anonymous.h:15:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["anonymous.h"],
              headerInclude = "anonymous.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "S2_inner_deep",
          structConstr = Name
            "@NsConstr"
            "S2_inner_deep",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "s2_inner_deep_b",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:16:11",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier
                        "s2_inner_deep_b"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "b",
                  commentLocation = Just
                    "anonymous.h:16:11",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["anonymous.h"],
                      headerInclude = "anonymous.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "anonymous.h:15:5",
                declId = NamePair {
                  nameC = Name "S2_inner_deep",
                  nameHsIdent = Identifier
                    "S2_inner_deep"},
                declOrigin = NameOriginGenerated
                  (AnonId "anonymous.h:15:5"),
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["anonymous.h"],
                    headerInclude = "anonymous.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name
                      "@NsConstr"
                      "S2_inner_deep"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "anonymous.h:16:11",
                        fieldName = NamePair {
                          nameC = Name "b",
                          nameHsIdent = Identifier
                            "s2_inner_deep_b"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
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
                "anonymous.h:15:5",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["anonymous.h"],
                  headerInclude = "anonymous.h"},
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
                    "S2_inner_deep",
                  structConstr = Name
                    "@NsConstr"
                    "S2_inner_deep",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s2_inner_deep_b",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:16:11",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier
                                "s2_inner_deep_b"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "b",
                          commentLocation = Just
                            "anonymous.h:16:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "anonymous.h:15:5",
                        declId = NamePair {
                          nameC = Name "S2_inner_deep",
                          nameHsIdent = Identifier
                            "S2_inner_deep"},
                        declOrigin = NameOriginGenerated
                          (AnonId "anonymous.h:15:5"),
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["anonymous.h"],
                            headerInclude = "anonymous.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "S2_inner_deep"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:16:11",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier
                                    "s2_inner_deep_b"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
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
                        "anonymous.h:15:5",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["anonymous.h"],
                          headerInclude = "anonymous.h"},
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
                    "S2_inner_deep",
                  structConstr = Name
                    "@NsConstr"
                    "S2_inner_deep",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s2_inner_deep_b",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:16:11",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier
                                "s2_inner_deep_b"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "b",
                          commentLocation = Just
                            "anonymous.h:16:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "anonymous.h:15:5",
                        declId = NamePair {
                          nameC = Name "S2_inner_deep",
                          nameHsIdent = Identifier
                            "S2_inner_deep"},
                        declOrigin = NameOriginGenerated
                          (AnonId "anonymous.h:15:5"),
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["anonymous.h"],
                            headerInclude = "anonymous.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "S2_inner_deep"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:16:11",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier
                                    "s2_inner_deep_b"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
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
                        "anonymous.h:15:5",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["anonymous.h"],
                          headerInclude = "anonymous.h"},
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
        "S2_inner_deep",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S2_inner_deep",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "S2_inner",
      structConstr = Name
        "@NsConstr"
        "S2_inner",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "s2_inner_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "anonymous.h:14:9",
                fieldName = NamePair {
                  nameC = Name "a",
                  nameHsIdent = Identifier
                    "s2_inner_a"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "a",
              commentLocation = Just
                "anonymous.h:14:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["anonymous.h"],
                  headerInclude = "anonymous.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "s2_inner_deep",
          fieldType = HsTypRef
            (Name
              "@NsTypeConstr"
              "S2_inner_deep"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "anonymous.h:17:7",
                fieldName = NamePair {
                  nameC = Name "deep",
                  nameHsIdent = Identifier
                    "s2_inner_deep"},
                fieldComment = Nothing},
              structFieldType = TypeStruct
                NamePair {
                  nameC = Name "S2_inner_deep",
                  nameHsIdent = Identifier
                    "S2_inner_deep"}
                (NameOriginGenerated
                  (AnonId "anonymous.h:15:5")),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "deep",
              commentLocation = Just
                "anonymous.h:17:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["anonymous.h"],
                  headerInclude = "anonymous.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "anonymous.h:13:3",
            declId = NamePair {
              nameC = Name "S2_inner",
              nameHsIdent = Identifier
                "S2_inner"},
            declOrigin = NameOriginGenerated
              (AnonId "anonymous.h:13:3"),
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["anonymous.h"],
                headerInclude = "anonymous.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "S2_inner"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:14:9",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s2_inner_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:17:7",
                    fieldName = NamePair {
                      nameC = Name "deep",
                      nameHsIdent = Identifier
                        "s2_inner_deep"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "S2_inner_deep",
                      nameHsIdent = Identifier
                        "S2_inner_deep"}
                    (NameOriginGenerated
                      (AnonId "anonymous.h:15:5")),
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
          commentOrigin = Nothing,
          commentLocation = Just
            "anonymous.h:13:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["anonymous.h"],
              headerInclude = "anonymous.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "S2_inner",
          structConstr = Name
            "@NsConstr"
            "S2_inner",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "s2_inner_a",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:14:9",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s2_inner_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "a",
                  commentLocation = Just
                    "anonymous.h:14:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["anonymous.h"],
                      headerInclude = "anonymous.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "s2_inner_deep",
              fieldType = HsTypRef
                (Name
                  "@NsTypeConstr"
                  "S2_inner_deep"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:17:7",
                    fieldName = NamePair {
                      nameC = Name "deep",
                      nameHsIdent = Identifier
                        "s2_inner_deep"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "S2_inner_deep",
                      nameHsIdent = Identifier
                        "S2_inner_deep"}
                    (NameOriginGenerated
                      (AnonId "anonymous.h:15:5")),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "deep",
                  commentLocation = Just
                    "anonymous.h:17:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["anonymous.h"],
                      headerInclude = "anonymous.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "anonymous.h:13:3",
                declId = NamePair {
                  nameC = Name "S2_inner",
                  nameHsIdent = Identifier
                    "S2_inner"},
                declOrigin = NameOriginGenerated
                  (AnonId "anonymous.h:13:3"),
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["anonymous.h"],
                    headerInclude = "anonymous.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "S2_inner"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "anonymous.h:14:9",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = Identifier
                            "s2_inner_a"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "anonymous.h:17:7",
                        fieldName = NamePair {
                          nameC = Name "deep",
                          nameHsIdent = Identifier
                            "s2_inner_deep"},
                        fieldComment = Nothing},
                      structFieldType = TypeStruct
                        NamePair {
                          nameC = Name "S2_inner_deep",
                          nameHsIdent = Identifier
                            "S2_inner_deep"}
                        (NameOriginGenerated
                          (AnonId "anonymous.h:15:5")),
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
              commentOrigin = Nothing,
              commentLocation = Just
                "anonymous.h:13:3",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["anonymous.h"],
                  headerInclude = "anonymous.h"},
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
                    "S2_inner",
                  structConstr = Name
                    "@NsConstr"
                    "S2_inner",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s2_inner_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:14:9",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s2_inner_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "anonymous.h:14:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s2_inner_deep",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "S2_inner_deep"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:17:7",
                            fieldName = NamePair {
                              nameC = Name "deep",
                              nameHsIdent = Identifier
                                "s2_inner_deep"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "S2_inner_deep",
                              nameHsIdent = Identifier
                                "S2_inner_deep"}
                            (NameOriginGenerated
                              (AnonId "anonymous.h:15:5")),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "deep",
                          commentLocation = Just
                            "anonymous.h:17:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "anonymous.h:13:3",
                        declId = NamePair {
                          nameC = Name "S2_inner",
                          nameHsIdent = Identifier
                            "S2_inner"},
                        declOrigin = NameOriginGenerated
                          (AnonId "anonymous.h:13:3"),
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["anonymous.h"],
                            headerInclude = "anonymous.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S2_inner"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:14:9",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s2_inner_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:17:7",
                                fieldName = NamePair {
                                  nameC = Name "deep",
                                  nameHsIdent = Identifier
                                    "s2_inner_deep"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "S2_inner_deep",
                                  nameHsIdent = Identifier
                                    "S2_inner_deep"}
                                (NameOriginGenerated
                                  (AnonId "anonymous.h:15:5")),
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
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "anonymous.h:13:3",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["anonymous.h"],
                          headerInclude = "anonymous.h"},
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
                    "S2_inner",
                  structConstr = Name
                    "@NsConstr"
                    "S2_inner",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s2_inner_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:14:9",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s2_inner_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "anonymous.h:14:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s2_inner_deep",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "S2_inner_deep"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:17:7",
                            fieldName = NamePair {
                              nameC = Name "deep",
                              nameHsIdent = Identifier
                                "s2_inner_deep"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "S2_inner_deep",
                              nameHsIdent = Identifier
                                "S2_inner_deep"}
                            (NameOriginGenerated
                              (AnonId "anonymous.h:15:5")),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "deep",
                          commentLocation = Just
                            "anonymous.h:17:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "anonymous.h:13:3",
                        declId = NamePair {
                          nameC = Name "S2_inner",
                          nameHsIdent = Identifier
                            "S2_inner"},
                        declOrigin = NameOriginGenerated
                          (AnonId "anonymous.h:13:3"),
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["anonymous.h"],
                            headerInclude = "anonymous.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S2_inner"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:14:9",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s2_inner_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:17:7",
                                fieldName = NamePair {
                                  nameC = Name "deep",
                                  nameHsIdent = Identifier
                                    "s2_inner_deep"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "S2_inner_deep",
                                  nameHsIdent = Identifier
                                    "S2_inner_deep"}
                                (NameOriginGenerated
                                  (AnonId "anonymous.h:15:5")),
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
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "anonymous.h:13:3",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["anonymous.h"],
                          headerInclude = "anonymous.h"},
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
        "S2_inner",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S2_inner",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "S2",
      structConstr = Name
        "@NsConstr"
        "S2",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "s2_inner",
          fieldType = HsTypRef
            (Name
              "@NsTypeConstr"
              "S2_inner"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "anonymous.h:18:5",
                fieldName = NamePair {
                  nameC = Name "inner",
                  nameHsIdent = Identifier
                    "s2_inner"},
                fieldComment = Nothing},
              structFieldType = TypeStruct
                NamePair {
                  nameC = Name "S2_inner",
                  nameHsIdent = Identifier
                    "S2_inner"}
                (NameOriginGenerated
                  (AnonId "anonymous.h:13:3")),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "inner",
              commentLocation = Just
                "anonymous.h:18:5",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["anonymous.h"],
                  headerInclude = "anonymous.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "s2_d",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "anonymous.h:20:7",
                fieldName = NamePair {
                  nameC = Name "d",
                  nameHsIdent = Identifier
                    "s2_d"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "d",
              commentLocation = Just
                "anonymous.h:20:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["anonymous.h"],
                  headerInclude = "anonymous.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "anonymous.h:12:8",
            declId = NamePair {
              nameC = Name "S2",
              nameHsIdent = Identifier "S2"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["anonymous.h"],
                headerInclude = "anonymous.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "S2"),
              structSizeof = 12,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:18:5",
                    fieldName = NamePair {
                      nameC = Name "inner",
                      nameHsIdent = Identifier
                        "s2_inner"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "S2_inner",
                      nameHsIdent = Identifier
                        "S2_inner"}
                    (NameOriginGenerated
                      (AnonId "anonymous.h:13:3")),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:20:7",
                    fieldName = NamePair {
                      nameC = Name "d",
                      nameHsIdent = Identifier
                        "s2_d"},
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
          commentOrigin = Just "S2",
          commentLocation = Just
            "anonymous.h:12:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["anonymous.h"],
              headerInclude = "anonymous.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "S2",
          structConstr = Name
            "@NsConstr"
            "S2",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "s2_inner",
              fieldType = HsTypRef
                (Name
                  "@NsTypeConstr"
                  "S2_inner"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:18:5",
                    fieldName = NamePair {
                      nameC = Name "inner",
                      nameHsIdent = Identifier
                        "s2_inner"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "S2_inner",
                      nameHsIdent = Identifier
                        "S2_inner"}
                    (NameOriginGenerated
                      (AnonId "anonymous.h:13:3")),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "inner",
                  commentLocation = Just
                    "anonymous.h:18:5",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["anonymous.h"],
                      headerInclude = "anonymous.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "s2_d",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:20:7",
                    fieldName = NamePair {
                      nameC = Name "d",
                      nameHsIdent = Identifier
                        "s2_d"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "d",
                  commentLocation = Just
                    "anonymous.h:20:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["anonymous.h"],
                      headerInclude = "anonymous.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "anonymous.h:12:8",
                declId = NamePair {
                  nameC = Name "S2",
                  nameHsIdent = Identifier "S2"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["anonymous.h"],
                    headerInclude = "anonymous.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "S2"),
                  structSizeof = 12,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "anonymous.h:18:5",
                        fieldName = NamePair {
                          nameC = Name "inner",
                          nameHsIdent = Identifier
                            "s2_inner"},
                        fieldComment = Nothing},
                      structFieldType = TypeStruct
                        NamePair {
                          nameC = Name "S2_inner",
                          nameHsIdent = Identifier
                            "S2_inner"}
                        (NameOriginGenerated
                          (AnonId "anonymous.h:13:3")),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "anonymous.h:20:7",
                        fieldName = NamePair {
                          nameC = Name "d",
                          nameHsIdent = Identifier
                            "s2_d"},
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
              commentOrigin = Just "S2",
              commentLocation = Just
                "anonymous.h:12:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["anonymous.h"],
                  headerInclude = "anonymous.h"},
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
                    "S2",
                  structConstr = Name
                    "@NsConstr"
                    "S2",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s2_inner",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "S2_inner"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:18:5",
                            fieldName = NamePair {
                              nameC = Name "inner",
                              nameHsIdent = Identifier
                                "s2_inner"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "S2_inner",
                              nameHsIdent = Identifier
                                "S2_inner"}
                            (NameOriginGenerated
                              (AnonId "anonymous.h:13:3")),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "inner",
                          commentLocation = Just
                            "anonymous.h:18:5",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s2_d",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:20:7",
                            fieldName = NamePair {
                              nameC = Name "d",
                              nameHsIdent = Identifier
                                "s2_d"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "d",
                          commentLocation = Just
                            "anonymous.h:20:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "anonymous.h:12:8",
                        declId = NamePair {
                          nameC = Name "S2",
                          nameHsIdent = Identifier "S2"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["anonymous.h"],
                            headerInclude = "anonymous.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S2"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:18:5",
                                fieldName = NamePair {
                                  nameC = Name "inner",
                                  nameHsIdent = Identifier
                                    "s2_inner"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "S2_inner",
                                  nameHsIdent = Identifier
                                    "S2_inner"}
                                (NameOriginGenerated
                                  (AnonId "anonymous.h:13:3")),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:20:7",
                                fieldName = NamePair {
                                  nameC = Name "d",
                                  nameHsIdent = Identifier
                                    "s2_d"},
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
                      commentOrigin = Just "S2",
                      commentLocation = Just
                        "anonymous.h:12:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["anonymous.h"],
                          headerInclude = "anonymous.h"},
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
                    "S2",
                  structConstr = Name
                    "@NsConstr"
                    "S2",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s2_inner",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "S2_inner"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:18:5",
                            fieldName = NamePair {
                              nameC = Name "inner",
                              nameHsIdent = Identifier
                                "s2_inner"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "S2_inner",
                              nameHsIdent = Identifier
                                "S2_inner"}
                            (NameOriginGenerated
                              (AnonId "anonymous.h:13:3")),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "inner",
                          commentLocation = Just
                            "anonymous.h:18:5",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s2_d",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:20:7",
                            fieldName = NamePair {
                              nameC = Name "d",
                              nameHsIdent = Identifier
                                "s2_d"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "d",
                          commentLocation = Just
                            "anonymous.h:20:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "anonymous.h:12:8",
                        declId = NamePair {
                          nameC = Name "S2",
                          nameHsIdent = Identifier "S2"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["anonymous.h"],
                            headerInclude = "anonymous.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S2"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:18:5",
                                fieldName = NamePair {
                                  nameC = Name "inner",
                                  nameHsIdent = Identifier
                                    "s2_inner"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "S2_inner",
                                  nameHsIdent = Identifier
                                    "S2_inner"}
                                (NameOriginGenerated
                                  (AnonId "anonymous.h:13:3")),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:20:7",
                                fieldName = NamePair {
                                  nameC = Name "d",
                                  nameHsIdent = Identifier
                                    "s2_d"},
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
                      commentOrigin = Just "S2",
                      commentLocation = Just
                        "anonymous.h:12:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["anonymous.h"],
                          headerInclude = "anonymous.h"},
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
        "S2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S2",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "S3",
      structConstr = Name
        "@NsConstr"
        "S3",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "s3_c",
          fieldType = HsPtr
            (HsPtr
              (HsTypRef
                (Name "@NsTypeConstr" "S3_c"))),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "anonymous.h:28:7",
                fieldName = NamePair {
                  nameC = Name "c",
                  nameHsIdent = Identifier
                    "s3_c"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypePointer
                  (TypeStruct
                    NamePair {
                      nameC = Name "S3_c",
                      nameHsIdent = Identifier "S3_c"}
                    (NameOriginGenerated
                      (AnonId "anonymous.h:25:3")))),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "c",
              commentLocation = Just
                "anonymous.h:28:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["anonymous.h"],
                  headerInclude = "anonymous.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "s3_d",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "anonymous.h:30:7",
                fieldName = NamePair {
                  nameC = Name "d",
                  nameHsIdent = Identifier
                    "s3_d"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "d",
              commentLocation = Just
                "anonymous.h:30:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["anonymous.h"],
                  headerInclude = "anonymous.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "anonymous.h:24:8",
            declId = NamePair {
              nameC = Name "S3",
              nameHsIdent = Identifier "S3"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["anonymous.h"],
                headerInclude = "anonymous.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "S3"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:28:7",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = Identifier
                        "s3_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypePointer
                      (TypeStruct
                        NamePair {
                          nameC = Name "S3_c",
                          nameHsIdent = Identifier "S3_c"}
                        (NameOriginGenerated
                          (AnonId "anonymous.h:25:3")))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:30:7",
                    fieldName = NamePair {
                      nameC = Name "d",
                      nameHsIdent = Identifier
                        "s3_d"},
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
          commentOrigin = Just "S3",
          commentLocation = Just
            "anonymous.h:24:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["anonymous.h"],
              headerInclude = "anonymous.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "S3",
          structConstr = Name
            "@NsConstr"
            "S3",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "s3_c",
              fieldType = HsPtr
                (HsPtr
                  (HsTypRef
                    (Name "@NsTypeConstr" "S3_c"))),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:28:7",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = Identifier
                        "s3_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypePointer
                      (TypeStruct
                        NamePair {
                          nameC = Name "S3_c",
                          nameHsIdent = Identifier "S3_c"}
                        (NameOriginGenerated
                          (AnonId "anonymous.h:25:3")))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "c",
                  commentLocation = Just
                    "anonymous.h:28:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["anonymous.h"],
                      headerInclude = "anonymous.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "s3_d",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:30:7",
                    fieldName = NamePair {
                      nameC = Name "d",
                      nameHsIdent = Identifier
                        "s3_d"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "d",
                  commentLocation = Just
                    "anonymous.h:30:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["anonymous.h"],
                      headerInclude = "anonymous.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "anonymous.h:24:8",
                declId = NamePair {
                  nameC = Name "S3",
                  nameHsIdent = Identifier "S3"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["anonymous.h"],
                    headerInclude = "anonymous.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "S3"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "anonymous.h:28:7",
                        fieldName = NamePair {
                          nameC = Name "c",
                          nameHsIdent = Identifier
                            "s3_c"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = Name "S3_c",
                              nameHsIdent = Identifier "S3_c"}
                            (NameOriginGenerated
                              (AnonId "anonymous.h:25:3")))),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "anonymous.h:30:7",
                        fieldName = NamePair {
                          nameC = Name "d",
                          nameHsIdent = Identifier
                            "s3_d"},
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
              commentOrigin = Just "S3",
              commentLocation = Just
                "anonymous.h:24:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["anonymous.h"],
                  headerInclude = "anonymous.h"},
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
                    "S3",
                  structConstr = Name
                    "@NsConstr"
                    "S3",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s3_c",
                      fieldType = HsPtr
                        (HsPtr
                          (HsTypRef
                            (Name "@NsTypeConstr" "S3_c"))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:28:7",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = Identifier
                                "s3_c"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypePointer
                              (TypeStruct
                                NamePair {
                                  nameC = Name "S3_c",
                                  nameHsIdent = Identifier "S3_c"}
                                (NameOriginGenerated
                                  (AnonId "anonymous.h:25:3")))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "anonymous.h:28:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s3_d",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:30:7",
                            fieldName = NamePair {
                              nameC = Name "d",
                              nameHsIdent = Identifier
                                "s3_d"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "d",
                          commentLocation = Just
                            "anonymous.h:30:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "anonymous.h:24:8",
                        declId = NamePair {
                          nameC = Name "S3",
                          nameHsIdent = Identifier "S3"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["anonymous.h"],
                            headerInclude = "anonymous.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S3"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:28:7",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = Identifier
                                    "s3_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypePointer
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "S3_c",
                                      nameHsIdent = Identifier "S3_c"}
                                    (NameOriginGenerated
                                      (AnonId "anonymous.h:25:3")))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:30:7",
                                fieldName = NamePair {
                                  nameC = Name "d",
                                  nameHsIdent = Identifier
                                    "s3_d"},
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
                      commentOrigin = Just "S3",
                      commentLocation = Just
                        "anonymous.h:24:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["anonymous.h"],
                          headerInclude = "anonymous.h"},
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
                    "S3",
                  structConstr = Name
                    "@NsConstr"
                    "S3",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s3_c",
                      fieldType = HsPtr
                        (HsPtr
                          (HsTypRef
                            (Name "@NsTypeConstr" "S3_c"))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:28:7",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = Identifier
                                "s3_c"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypePointer
                              (TypeStruct
                                NamePair {
                                  nameC = Name "S3_c",
                                  nameHsIdent = Identifier "S3_c"}
                                (NameOriginGenerated
                                  (AnonId "anonymous.h:25:3")))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "anonymous.h:28:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s3_d",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:30:7",
                            fieldName = NamePair {
                              nameC = Name "d",
                              nameHsIdent = Identifier
                                "s3_d"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "d",
                          commentLocation = Just
                            "anonymous.h:30:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "anonymous.h:24:8",
                        declId = NamePair {
                          nameC = Name "S3",
                          nameHsIdent = Identifier "S3"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["anonymous.h"],
                            headerInclude = "anonymous.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S3"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:28:7",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = Identifier
                                    "s3_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypePointer
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "S3_c",
                                      nameHsIdent = Identifier "S3_c"}
                                    (NameOriginGenerated
                                      (AnonId "anonymous.h:25:3")))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:30:7",
                                fieldName = NamePair {
                                  nameC = Name "d",
                                  nameHsIdent = Identifier
                                    "s3_d"},
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
                      commentOrigin = Just "S3",
                      commentLocation = Just
                        "anonymous.h:24:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["anonymous.h"],
                          headerInclude = "anonymous.h"},
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
        "S3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S3",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "S3_c",
      structConstr = Name
        "@NsConstr"
        "S3_c",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "s3_c_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "anonymous.h:26:9",
                fieldName = NamePair {
                  nameC = Name "a",
                  nameHsIdent = Identifier
                    "s3_c_a"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "a",
              commentLocation = Just
                "anonymous.h:26:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["anonymous.h"],
                  headerInclude = "anonymous.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "s3_c_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "anonymous.h:27:9",
                fieldName = NamePair {
                  nameC = Name "b",
                  nameHsIdent = Identifier
                    "s3_c_b"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "b",
              commentLocation = Just
                "anonymous.h:27:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["anonymous.h"],
                  headerInclude = "anonymous.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "anonymous.h:25:3",
            declId = NamePair {
              nameC = Name "S3_c",
              nameHsIdent = Identifier
                "S3_c"},
            declOrigin = NameOriginGenerated
              (AnonId "anonymous.h:25:3"),
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["anonymous.h"],
                headerInclude = "anonymous.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "S3_c"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:26:9",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s3_c_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:27:9",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier
                        "s3_c_b"},
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
          commentOrigin = Nothing,
          commentLocation = Just
            "anonymous.h:25:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["anonymous.h"],
              headerInclude = "anonymous.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "S3_c",
          structConstr = Name
            "@NsConstr"
            "S3_c",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "s3_c_a",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:26:9",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s3_c_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "a",
                  commentLocation = Just
                    "anonymous.h:26:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["anonymous.h"],
                      headerInclude = "anonymous.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "s3_c_b",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "anonymous.h:27:9",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier
                        "s3_c_b"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "b",
                  commentLocation = Just
                    "anonymous.h:27:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["anonymous.h"],
                      headerInclude = "anonymous.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "anonymous.h:25:3",
                declId = NamePair {
                  nameC = Name "S3_c",
                  nameHsIdent = Identifier
                    "S3_c"},
                declOrigin = NameOriginGenerated
                  (AnonId "anonymous.h:25:3"),
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["anonymous.h"],
                    headerInclude = "anonymous.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "S3_c"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "anonymous.h:26:9",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = Identifier
                            "s3_c_a"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "anonymous.h:27:9",
                        fieldName = NamePair {
                          nameC = Name "b",
                          nameHsIdent = Identifier
                            "s3_c_b"},
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
              commentOrigin = Nothing,
              commentLocation = Just
                "anonymous.h:25:3",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["anonymous.h"],
                  headerInclude = "anonymous.h"},
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
                    "S3_c",
                  structConstr = Name
                    "@NsConstr"
                    "S3_c",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s3_c_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:26:9",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s3_c_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "anonymous.h:26:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s3_c_b",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:27:9",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier
                                "s3_c_b"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "b",
                          commentLocation = Just
                            "anonymous.h:27:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "anonymous.h:25:3",
                        declId = NamePair {
                          nameC = Name "S3_c",
                          nameHsIdent = Identifier
                            "S3_c"},
                        declOrigin = NameOriginGenerated
                          (AnonId "anonymous.h:25:3"),
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["anonymous.h"],
                            headerInclude = "anonymous.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S3_c"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:26:9",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s3_c_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:27:9",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier
                                    "s3_c_b"},
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
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "anonymous.h:25:3",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["anonymous.h"],
                          headerInclude = "anonymous.h"},
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
                    "S3_c",
                  structConstr = Name
                    "@NsConstr"
                    "S3_c",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s3_c_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:26:9",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s3_c_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "anonymous.h:26:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s3_c_b",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "anonymous.h:27:9",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier
                                "s3_c_b"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "b",
                          commentLocation = Just
                            "anonymous.h:27:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["anonymous.h"],
                              headerInclude = "anonymous.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "anonymous.h:25:3",
                        declId = NamePair {
                          nameC = Name "S3_c",
                          nameHsIdent = Identifier
                            "S3_c"},
                        declOrigin = NameOriginGenerated
                          (AnonId "anonymous.h:25:3"),
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["anonymous.h"],
                            headerInclude = "anonymous.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S3_c"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:26:9",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s3_c_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "anonymous.h:27:9",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier
                                    "s3_c_b"},
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
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "anonymous.h:25:3",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["anonymous.h"],
                          headerInclude = "anonymous.h"},
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
        "S3_c",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S3_c",
      deriveInstanceComment =
      Nothing}]
