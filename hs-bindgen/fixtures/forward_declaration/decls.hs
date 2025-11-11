[
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "S1_t",
      structConstr = Name
        "@NsConstr"
        "S1_t",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "s1_t_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "forward_declaration.h:4:7",
                fieldName = NamePair {
                  nameC = Name "a",
                  nameHsIdent = Identifier
                    "s1_t_a"},
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
                "forward_declaration.h:4:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["forward_declaration.h"],
                  headerInclude =
                  "forward_declaration.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "forward_declaration.h:3:8",
            declId = NamePair {
              nameC = Name "S1_t",
              nameHsIdent = Identifier
                "S1_t"},
            declOrigin =
            NameOriginRenamedFrom
              (Name "S1"),
            declAliases = [Name "S1_t"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["forward_declaration.h"],
                headerInclude =
                "forward_declaration.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "S1_t"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "forward_declaration.h:4:7",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s1_t_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "S1_t",
          commentLocation = Just
            "forward_declaration.h:3:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["forward_declaration.h"],
              headerInclude =
              "forward_declaration.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "S1_t",
          structConstr = Name
            "@NsConstr"
            "S1_t",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "s1_t_a",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "forward_declaration.h:4:7",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s1_t_a"},
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
                    "forward_declaration.h:4:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["forward_declaration.h"],
                      headerInclude =
                      "forward_declaration.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "forward_declaration.h:3:8",
                declId = NamePair {
                  nameC = Name "S1_t",
                  nameHsIdent = Identifier
                    "S1_t"},
                declOrigin =
                NameOriginRenamedFrom
                  (Name "S1"),
                declAliases = [Name "S1_t"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["forward_declaration.h"],
                    headerInclude =
                    "forward_declaration.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "S1_t"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "forward_declaration.h:4:7",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = Identifier
                            "s1_t_a"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "S1_t",
              commentLocation = Just
                "forward_declaration.h:3:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["forward_declaration.h"],
                  headerInclude =
                  "forward_declaration.h"},
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
                    "S1_t",
                  structConstr = Name
                    "@NsConstr"
                    "S1_t",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s1_t_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "forward_declaration.h:4:7",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s1_t_a"},
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
                            "forward_declaration.h:4:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["forward_declaration.h"],
                              headerInclude =
                              "forward_declaration.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "forward_declaration.h:3:8",
                        declId = NamePair {
                          nameC = Name "S1_t",
                          nameHsIdent = Identifier
                            "S1_t"},
                        declOrigin =
                        NameOriginRenamedFrom
                          (Name "S1"),
                        declAliases = [Name "S1_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["forward_declaration.h"],
                            headerInclude =
                            "forward_declaration.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S1_t"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "forward_declaration.h:4:7",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s1_t_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "S1_t",
                      commentLocation = Just
                        "forward_declaration.h:3:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["forward_declaration.h"],
                          headerInclude =
                          "forward_declaration.h"},
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
                    "S1_t",
                  structConstr = Name
                    "@NsConstr"
                    "S1_t",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s1_t_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "forward_declaration.h:4:7",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s1_t_a"},
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
                            "forward_declaration.h:4:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["forward_declaration.h"],
                              headerInclude =
                              "forward_declaration.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "forward_declaration.h:3:8",
                        declId = NamePair {
                          nameC = Name "S1_t",
                          nameHsIdent = Identifier
                            "S1_t"},
                        declOrigin =
                        NameOriginRenamedFrom
                          (Name "S1"),
                        declAliases = [Name "S1_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["forward_declaration.h"],
                            headerInclude =
                            "forward_declaration.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S1_t"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "forward_declaration.h:4:7",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s1_t_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "S1_t",
                      commentLocation = Just
                        "forward_declaration.h:3:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["forward_declaration.h"],
                          headerInclude =
                          "forward_declaration.h"},
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
        "S1_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S1_t",
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
            "s2_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "forward_declaration.h:10:7",
                fieldName = NamePair {
                  nameC = Name "a",
                  nameHsIdent = Identifier
                    "s2_a"},
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
                "forward_declaration.h:10:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["forward_declaration.h"],
                  headerInclude =
                  "forward_declaration.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "forward_declaration.h:9:8",
            declId = NamePair {
              nameC = Name "S2",
              nameHsIdent = Identifier "S2"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["forward_declaration.h"],
                headerInclude =
                "forward_declaration.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "S2"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "forward_declaration.h:10:7",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s2_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "S2",
          commentLocation = Just
            "forward_declaration.h:9:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["forward_declaration.h"],
              headerInclude =
              "forward_declaration.h"},
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
                "s2_a",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "forward_declaration.h:10:7",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s2_a"},
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
                    "forward_declaration.h:10:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["forward_declaration.h"],
                      headerInclude =
                      "forward_declaration.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "forward_declaration.h:9:8",
                declId = NamePair {
                  nameC = Name "S2",
                  nameHsIdent = Identifier "S2"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["forward_declaration.h"],
                    headerInclude =
                    "forward_declaration.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "S2"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "forward_declaration.h:10:7",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = Identifier
                            "s2_a"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "S2",
              commentLocation = Just
                "forward_declaration.h:9:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["forward_declaration.h"],
                  headerInclude =
                  "forward_declaration.h"},
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
                    "S2",
                  structConstr = Name
                    "@NsConstr"
                    "S2",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s2_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "forward_declaration.h:10:7",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s2_a"},
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
                            "forward_declaration.h:10:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["forward_declaration.h"],
                              headerInclude =
                              "forward_declaration.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "forward_declaration.h:9:8",
                        declId = NamePair {
                          nameC = Name "S2",
                          nameHsIdent = Identifier "S2"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["forward_declaration.h"],
                            headerInclude =
                            "forward_declaration.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S2"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "forward_declaration.h:10:7",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s2_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "S2",
                      commentLocation = Just
                        "forward_declaration.h:9:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["forward_declaration.h"],
                          headerInclude =
                          "forward_declaration.h"},
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
                    "S2",
                  structConstr = Name
                    "@NsConstr"
                    "S2",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s2_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "forward_declaration.h:10:7",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s2_a"},
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
                            "forward_declaration.h:10:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["forward_declaration.h"],
                              headerInclude =
                              "forward_declaration.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "forward_declaration.h:9:8",
                        declId = NamePair {
                          nameC = Name "S2",
                          nameHsIdent = Identifier "S2"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["forward_declaration.h"],
                            headerInclude =
                            "forward_declaration.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S2"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "forward_declaration.h:10:7",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s2_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "S2",
                      commentLocation = Just
                        "forward_declaration.h:9:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["forward_declaration.h"],
                          headerInclude =
                          "forward_declaration.h"},
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
      Nothing}]
