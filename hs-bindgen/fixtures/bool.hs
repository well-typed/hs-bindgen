[
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Bools1",
      structConstr = Name
        "@NsConstr"
        "Bools1",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "bools1_x",
          fieldType = HsPrimType
            HsPrimCBool,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bool.h:2:11",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "bools1_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                PrimBool,
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Just
                "bool.h:2:11",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bool.h"],
                  headerInclude = "bool.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "bools1_y",
          fieldType = HsPrimType
            HsPrimCBool,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bool.h:3:11",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier
                    "bools1_y"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                PrimBool,
              structFieldOffset = 8,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Just
                "bool.h:3:11",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bool.h"],
                  headerInclude = "bool.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "bool.h:1:8",
            declId = NamePair {
              nameC = Name "bools1",
              nameHsIdent = Identifier
                "Bools1"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["bool.h"],
                headerInclude = "bool.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Bools1"),
              structSizeof = 2,
              structAlignment = 1,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bool.h:2:11",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "bools1_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bool.h:3:11",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "bools1_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 8,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecModule = ModuleName
                "Example",
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "bools1",
          commentLocation = Just
            "bool.h:1:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["bool.h"],
              headerInclude = "bool.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Bools1",
          structConstr = Name
            "@NsConstr"
            "Bools1",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "bools1_x",
              fieldType = HsPrimType
                HsPrimCBool,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bool.h:2:11",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "bools1_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "x",
                  commentLocation = Just
                    "bool.h:2:11",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bool.h"],
                      headerInclude = "bool.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "bools1_y",
              fieldType = HsPrimType
                HsPrimCBool,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bool.h:3:11",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "bools1_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 8,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "y",
                  commentLocation = Just
                    "bool.h:3:11",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bool.h"],
                      headerInclude = "bool.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "bool.h:1:8",
                declId = NamePair {
                  nameC = Name "bools1",
                  nameHsIdent = Identifier
                    "Bools1"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["bool.h"],
                    headerInclude = "bool.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Bools1"),
                  structSizeof = 2,
                  structAlignment = 1,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bool.h:2:11",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "bools1_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        PrimBool,
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bool.h:3:11",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = Identifier
                            "bools1_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        PrimBool,
                      structFieldOffset = 8,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecModule = ModuleName
                    "Example",
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "bools1",
              commentLocation = Just
                "bool.h:1:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bool.h"],
                  headerInclude = "bool.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 2,
          storableAlignment = 1,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Bools1",
                  structConstr = Name
                    "@NsConstr"
                    "Bools1",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "bools1_x",
                      fieldType = HsPrimType
                        HsPrimCBool,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bool.h:2:11",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "bools1_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            PrimBool,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "bool.h:2:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bool.h"],
                              headerInclude = "bool.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "bools1_y",
                      fieldType = HsPrimType
                        HsPrimCBool,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bool.h:3:11",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "bools1_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            PrimBool,
                          structFieldOffset = 8,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "bool.h:3:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bool.h"],
                              headerInclude = "bool.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bool.h:1:8",
                        declId = NamePair {
                          nameC = Name "bools1",
                          nameHsIdent = Identifier
                            "Bools1"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["bool.h"],
                            headerInclude = "bool.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Bools1"),
                          structSizeof = 2,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bool.h:2:11",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "bools1_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                PrimBool,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bool.h:3:11",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "bools1_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                PrimBool,
                              structFieldOffset = 8,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = ModuleName
                            "Example",
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "bools1",
                      commentLocation = Just
                        "bool.h:1:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["bool.h"],
                          headerInclude = "bool.h"},
                      commentChildren = []}})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 1]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Bools1",
                  structConstr = Name
                    "@NsConstr"
                    "Bools1",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "bools1_x",
                      fieldType = HsPrimType
                        HsPrimCBool,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bool.h:2:11",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "bools1_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            PrimBool,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "bool.h:2:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bool.h"],
                              headerInclude = "bool.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "bools1_y",
                      fieldType = HsPrimType
                        HsPrimCBool,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bool.h:3:11",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "bools1_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            PrimBool,
                          structFieldOffset = 8,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "bool.h:3:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bool.h"],
                              headerInclude = "bool.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bool.h:1:8",
                        declId = NamePair {
                          nameC = Name "bools1",
                          nameHsIdent = Identifier
                            "Bools1"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["bool.h"],
                            headerInclude = "bool.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Bools1"),
                          structSizeof = 2,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bool.h:2:11",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "bools1_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                PrimBool,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bool.h:3:11",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "bools1_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                PrimBool,
                              structFieldOffset = 8,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = ModuleName
                            "Example",
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "bools1",
                      commentLocation = Just
                        "bool.h:1:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["bool.h"],
                          headerInclude = "bool.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      1
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
        "Bools1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Bools1",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Bools2",
      structConstr = Name
        "@NsConstr"
        "Bools2",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "bools2_x",
          fieldType = HsPrimType
            HsPrimCBool,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bool.h:9:10",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "bools2_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                PrimBool,
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Just
                "bool.h:9:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bool.h"],
                  headerInclude = "bool.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "bools2_y",
          fieldType = HsPrimType
            HsPrimCBool,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bool.h:10:10",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier
                    "bools2_y"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                PrimBool,
              structFieldOffset = 8,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Just
                "bool.h:10:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bool.h"],
                  headerInclude = "bool.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "bool.h:8:8",
            declId = NamePair {
              nameC = Name "bools2",
              nameHsIdent = Identifier
                "Bools2"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["bool.h"],
                headerInclude = "bool.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Bools2"),
              structSizeof = 2,
              structAlignment = 1,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bool.h:9:10",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "bools2_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bool.h:10:10",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "bools2_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 8,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecModule = ModuleName
                "Example",
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "bools2",
          commentLocation = Just
            "bool.h:8:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["bool.h"],
              headerInclude = "bool.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Bools2",
          structConstr = Name
            "@NsConstr"
            "Bools2",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "bools2_x",
              fieldType = HsPrimType
                HsPrimCBool,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bool.h:9:10",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "bools2_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "x",
                  commentLocation = Just
                    "bool.h:9:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bool.h"],
                      headerInclude = "bool.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "bools2_y",
              fieldType = HsPrimType
                HsPrimCBool,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bool.h:10:10",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "bools2_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 8,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "y",
                  commentLocation = Just
                    "bool.h:10:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bool.h"],
                      headerInclude = "bool.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "bool.h:8:8",
                declId = NamePair {
                  nameC = Name "bools2",
                  nameHsIdent = Identifier
                    "Bools2"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["bool.h"],
                    headerInclude = "bool.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Bools2"),
                  structSizeof = 2,
                  structAlignment = 1,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bool.h:9:10",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "bools2_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        PrimBool,
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bool.h:10:10",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = Identifier
                            "bools2_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        PrimBool,
                      structFieldOffset = 8,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecModule = ModuleName
                    "Example",
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "bools2",
              commentLocation = Just
                "bool.h:8:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bool.h"],
                  headerInclude = "bool.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 2,
          storableAlignment = 1,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Bools2",
                  structConstr = Name
                    "@NsConstr"
                    "Bools2",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "bools2_x",
                      fieldType = HsPrimType
                        HsPrimCBool,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bool.h:9:10",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "bools2_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            PrimBool,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "bool.h:9:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bool.h"],
                              headerInclude = "bool.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "bools2_y",
                      fieldType = HsPrimType
                        HsPrimCBool,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bool.h:10:10",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "bools2_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            PrimBool,
                          structFieldOffset = 8,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "bool.h:10:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bool.h"],
                              headerInclude = "bool.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bool.h:8:8",
                        declId = NamePair {
                          nameC = Name "bools2",
                          nameHsIdent = Identifier
                            "Bools2"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["bool.h"],
                            headerInclude = "bool.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Bools2"),
                          structSizeof = 2,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bool.h:9:10",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "bools2_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                PrimBool,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bool.h:10:10",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "bools2_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                PrimBool,
                              structFieldOffset = 8,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = ModuleName
                            "Example",
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "bools2",
                      commentLocation = Just
                        "bool.h:8:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["bool.h"],
                          headerInclude = "bool.h"},
                      commentChildren = []}})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 1]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Bools2",
                  structConstr = Name
                    "@NsConstr"
                    "Bools2",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "bools2_x",
                      fieldType = HsPrimType
                        HsPrimCBool,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bool.h:9:10",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "bools2_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            PrimBool,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "bool.h:9:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bool.h"],
                              headerInclude = "bool.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "bools2_y",
                      fieldType = HsPrimType
                        HsPrimCBool,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bool.h:10:10",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "bools2_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            PrimBool,
                          structFieldOffset = 8,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "bool.h:10:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bool.h"],
                              headerInclude = "bool.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bool.h:8:8",
                        declId = NamePair {
                          nameC = Name "bools2",
                          nameHsIdent = Identifier
                            "Bools2"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["bool.h"],
                            headerInclude = "bool.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Bools2"),
                          structSizeof = 2,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bool.h:9:10",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "bools2_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                PrimBool,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bool.h:10:10",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "bools2_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                PrimBool,
                              structFieldOffset = 8,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = ModuleName
                            "Example",
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "bools2",
                      commentLocation = Just
                        "bool.h:8:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["bool.h"],
                          headerInclude = "bool.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      1
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
        "Bools2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Bools2",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "BOOL",
      newtypeConstr = Name
        "@NsConstr"
        "BOOL",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_BOOL",
        fieldType = HsPrimType
          HsPrimCBool,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "bool.h:13:9",
          declId = NamePair {
            nameC = Name "BOOL",
            nameHsIdent = Identifier
              "BOOL"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["bool.h"],
              headerInclude = "bool.h"},
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "BOOL",
              newtypeField = Name
                "@NsVar"
                "un_BOOL"},
            macroType = TypePrim PrimBool},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = ModuleName
              "Example",
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
          commentTitle = Nothing,
          commentOrigin = Just "BOOL",
          commentLocation = Just
            "bool.h:13:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["bool.h"],
              headerInclude = "bool.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOL",
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
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Bools3",
      structConstr = Name
        "@NsConstr"
        "Bools3",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "bools3_x",
          fieldType = HsTypRef
            (Name "@NsTypeConstr" "BOOL"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bool.h:16:10",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "bools3_x"},
                fieldComment = Nothing},
              structFieldType =
              TypeMacroTypedef
                NamePair {
                  nameC = Name "BOOL",
                  nameHsIdent = Identifier "BOOL"}
                NameOriginInSource,
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Just
                "bool.h:16:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bool.h"],
                  headerInclude = "bool.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "bools3_y",
          fieldType = HsTypRef
            (Name "@NsTypeConstr" "BOOL"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bool.h:17:10",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier
                    "bools3_y"},
                fieldComment = Nothing},
              structFieldType =
              TypeMacroTypedef
                NamePair {
                  nameC = Name "BOOL",
                  nameHsIdent = Identifier "BOOL"}
                NameOriginInSource,
              structFieldOffset = 8,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Just
                "bool.h:17:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bool.h"],
                  headerInclude = "bool.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "bool.h:15:8",
            declId = NamePair {
              nameC = Name "bools3",
              nameHsIdent = Identifier
                "Bools3"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["bool.h"],
                headerInclude = "bool.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Bools3"),
              structSizeof = 2,
              structAlignment = 1,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bool.h:16:10",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "bools3_x"},
                    fieldComment = Nothing},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = Name "BOOL",
                      nameHsIdent = Identifier "BOOL"}
                    NameOriginInSource,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bool.h:17:10",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "bools3_y"},
                    fieldComment = Nothing},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = Name "BOOL",
                      nameHsIdent = Identifier "BOOL"}
                    NameOriginInSource,
                  structFieldOffset = 8,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecModule = ModuleName
                "Example",
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "bools3",
          commentLocation = Just
            "bool.h:15:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["bool.h"],
              headerInclude = "bool.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Bools3",
          structConstr = Name
            "@NsConstr"
            "Bools3",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "bools3_x",
              fieldType = HsTypRef
                (Name "@NsTypeConstr" "BOOL"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bool.h:16:10",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "bools3_x"},
                    fieldComment = Nothing},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = Name "BOOL",
                      nameHsIdent = Identifier "BOOL"}
                    NameOriginInSource,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "x",
                  commentLocation = Just
                    "bool.h:16:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bool.h"],
                      headerInclude = "bool.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "bools3_y",
              fieldType = HsTypRef
                (Name "@NsTypeConstr" "BOOL"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bool.h:17:10",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "bools3_y"},
                    fieldComment = Nothing},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = Name "BOOL",
                      nameHsIdent = Identifier "BOOL"}
                    NameOriginInSource,
                  structFieldOffset = 8,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "y",
                  commentLocation = Just
                    "bool.h:17:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bool.h"],
                      headerInclude = "bool.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "bool.h:15:8",
                declId = NamePair {
                  nameC = Name "bools3",
                  nameHsIdent = Identifier
                    "Bools3"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["bool.h"],
                    headerInclude = "bool.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Bools3"),
                  structSizeof = 2,
                  structAlignment = 1,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bool.h:16:10",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "bools3_x"},
                        fieldComment = Nothing},
                      structFieldType =
                      TypeMacroTypedef
                        NamePair {
                          nameC = Name "BOOL",
                          nameHsIdent = Identifier "BOOL"}
                        NameOriginInSource,
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bool.h:17:10",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = Identifier
                            "bools3_y"},
                        fieldComment = Nothing},
                      structFieldType =
                      TypeMacroTypedef
                        NamePair {
                          nameC = Name "BOOL",
                          nameHsIdent = Identifier "BOOL"}
                        NameOriginInSource,
                      structFieldOffset = 8,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecModule = ModuleName
                    "Example",
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "bools3",
              commentLocation = Just
                "bool.h:15:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bool.h"],
                  headerInclude = "bool.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 2,
          storableAlignment = 1,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Bools3",
                  structConstr = Name
                    "@NsConstr"
                    "Bools3",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "bools3_x",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "BOOL"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bool.h:16:10",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "bools3_x"},
                            fieldComment = Nothing},
                          structFieldType =
                          TypeMacroTypedef
                            NamePair {
                              nameC = Name "BOOL",
                              nameHsIdent = Identifier "BOOL"}
                            NameOriginInSource,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "bool.h:16:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bool.h"],
                              headerInclude = "bool.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "bools3_y",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "BOOL"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bool.h:17:10",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "bools3_y"},
                            fieldComment = Nothing},
                          structFieldType =
                          TypeMacroTypedef
                            NamePair {
                              nameC = Name "BOOL",
                              nameHsIdent = Identifier "BOOL"}
                            NameOriginInSource,
                          structFieldOffset = 8,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "bool.h:17:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bool.h"],
                              headerInclude = "bool.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bool.h:15:8",
                        declId = NamePair {
                          nameC = Name "bools3",
                          nameHsIdent = Identifier
                            "Bools3"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["bool.h"],
                            headerInclude = "bool.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Bools3"),
                          structSizeof = 2,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bool.h:16:10",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "bools3_x"},
                                fieldComment = Nothing},
                              structFieldType =
                              TypeMacroTypedef
                                NamePair {
                                  nameC = Name "BOOL",
                                  nameHsIdent = Identifier "BOOL"}
                                NameOriginInSource,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bool.h:17:10",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "bools3_y"},
                                fieldComment = Nothing},
                              structFieldType =
                              TypeMacroTypedef
                                NamePair {
                                  nameC = Name "BOOL",
                                  nameHsIdent = Identifier "BOOL"}
                                NameOriginInSource,
                              structFieldOffset = 8,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = ModuleName
                            "Example",
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "bools3",
                      commentLocation = Just
                        "bool.h:15:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["bool.h"],
                          headerInclude = "bool.h"},
                      commentChildren = []}})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 1]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Bools3",
                  structConstr = Name
                    "@NsConstr"
                    "Bools3",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "bools3_x",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "BOOL"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bool.h:16:10",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "bools3_x"},
                            fieldComment = Nothing},
                          structFieldType =
                          TypeMacroTypedef
                            NamePair {
                              nameC = Name "BOOL",
                              nameHsIdent = Identifier "BOOL"}
                            NameOriginInSource,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "bool.h:16:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bool.h"],
                              headerInclude = "bool.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "bools3_y",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "BOOL"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bool.h:17:10",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "bools3_y"},
                            fieldComment = Nothing},
                          structFieldType =
                          TypeMacroTypedef
                            NamePair {
                              nameC = Name "BOOL",
                              nameHsIdent = Identifier "BOOL"}
                            NameOriginInSource,
                          structFieldOffset = 8,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "bool.h:17:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bool.h"],
                              headerInclude = "bool.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bool.h:15:8",
                        declId = NamePair {
                          nameC = Name "bools3",
                          nameHsIdent = Identifier
                            "Bools3"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["bool.h"],
                            headerInclude = "bool.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Bools3"),
                          structSizeof = 2,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bool.h:16:10",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "bools3_x"},
                                fieldComment = Nothing},
                              structFieldType =
                              TypeMacroTypedef
                                NamePair {
                                  nameC = Name "BOOL",
                                  nameHsIdent = Identifier "BOOL"}
                                NameOriginInSource,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bool.h:17:10",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "bools3_y"},
                                fieldComment = Nothing},
                              structFieldType =
                              TypeMacroTypedef
                                NamePair {
                                  nameC = Name "BOOL",
                                  nameHsIdent = Identifier "BOOL"}
                                NameOriginInSource,
                              structFieldOffset = 8,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = ModuleName
                            "Example",
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "bools3",
                      commentLocation = Just
                        "bool.h:15:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["bool.h"],
                          headerInclude = "bool.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      1
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
        "Bools3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Bools3",
      deriveInstanceComment =
      Nothing}]
