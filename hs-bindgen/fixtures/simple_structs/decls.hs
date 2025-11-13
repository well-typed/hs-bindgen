[
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
            "s1_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "simple_structs.h:3:9",
                fieldName = NamePair {
                  nameC = Name "a",
                  nameHsIdent = Identifier
                    "s1_a"},
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
                "simple_structs.h:3:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "s1_b",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "simple_structs.h:4:10",
                fieldName = NamePair {
                  nameC = Name "b",
                  nameHsIdent = Identifier
                    "s1_b"},
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
              commentOrigin = Just "b",
              commentLocation = Just
                "simple_structs.h:4:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "simple_structs.h:2:8",
            declId = NamePair {
              nameC = Name "S1",
              nameHsIdent = Identifier "S1"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["simple_structs.h"],
                headerInclude =
                "simple_structs.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "S1"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:3:9",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s1_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:4:10",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier
                        "s1_b"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 32,
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
          commentOrigin = Just "S1",
          commentLocation = Just
            "simple_structs.h:2:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["simple_structs.h"],
              headerInclude =
              "simple_structs.h"},
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
                "s1_a",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:3:9",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s1_a"},
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
                    "simple_structs.h:3:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["simple_structs.h"],
                      headerInclude =
                      "simple_structs.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "s1_b",
              fieldType = HsPrimType
                HsPrimCChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:4:10",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier
                        "s1_b"},
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
                  commentOrigin = Just "b",
                  commentLocation = Just
                    "simple_structs.h:4:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["simple_structs.h"],
                      headerInclude =
                      "simple_structs.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "simple_structs.h:2:8",
                declId = NamePair {
                  nameC = Name "S1",
                  nameHsIdent = Identifier "S1"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["simple_structs.h"],
                    headerInclude =
                    "simple_structs.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "S1"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:3:9",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = Identifier
                            "s1_a"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:4:10",
                        fieldName = NamePair {
                          nameC = Name "b",
                          nameHsIdent = Identifier
                            "s1_b"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 32,
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
              commentOrigin = Just "S1",
              commentLocation = Just
                "simple_structs.h:2:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
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
                    "S1",
                  structConstr = Name
                    "@NsConstr"
                    "S1",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s1_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:3:9",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s1_a"},
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
                            "simple_structs.h:3:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s1_b",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:4:10",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier
                                "s1_b"},
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
                          commentOrigin = Just "b",
                          commentLocation = Just
                            "simple_structs.h:4:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "simple_structs.h:2:8",
                        declId = NamePair {
                          nameC = Name "S1",
                          nameHsIdent = Identifier "S1"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["simple_structs.h"],
                            headerInclude =
                            "simple_structs.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S1"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:3:9",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s1_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:4:10",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier
                                    "s1_b"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 32,
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
                      commentOrigin = Just "S1",
                      commentLocation = Just
                        "simple_structs.h:2:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["simple_structs.h"],
                          headerInclude =
                          "simple_structs.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "s1_a")
                  (Idx 0),
                PeekCField
                  (HsStrLit "s1_b")
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
                    "S1",
                  structConstr = Name
                    "@NsConstr"
                    "S1",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s1_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:3:9",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s1_a"},
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
                            "simple_structs.h:3:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s1_b",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:4:10",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier
                                "s1_b"},
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
                          commentOrigin = Just "b",
                          commentLocation = Just
                            "simple_structs.h:4:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "simple_structs.h:2:8",
                        declId = NamePair {
                          nameC = Name "S1",
                          nameHsIdent = Identifier "S1"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["simple_structs.h"],
                            headerInclude =
                            "simple_structs.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S1"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:3:9",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s1_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:4:10",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier
                                    "s1_b"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 32,
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
                      commentOrigin = Just "S1",
                      commentLocation = Just
                        "simple_structs.h:2:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["simple_structs.h"],
                          headerInclude =
                          "simple_structs.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "s1_a")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "s1_b")
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
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "S1"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "s1_a",
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
            (Name "@NsTypeConstr" "S1"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "s1_a",
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
            (Name "@NsTypeConstr" "S1"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "s1_b",
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
            (Name "@NsTypeConstr" "S1"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "s1_b",
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
        "S2_t",
      structConstr = Name
        "@NsConstr"
        "S2_t",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "s2_t_a",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "simple_structs.h:9:10",
                fieldName = NamePair {
                  nameC = Name "a",
                  nameHsIdent = Identifier
                    "s2_t_a"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "a",
              commentLocation = Just
                "simple_structs.h:9:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "s2_t_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "simple_structs.h:10:9",
                fieldName = NamePair {
                  nameC = Name "b",
                  nameHsIdent = Identifier
                    "s2_t_b"},
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
                "simple_structs.h:10:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "s2_t_c",
          fieldType = HsPrimType
            HsPrimCFloat,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "simple_structs.h:11:11",
                fieldName = NamePair {
                  nameC = Name "c",
                  nameHsIdent = Identifier
                    "s2_t_c"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimFloating PrimFloat),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "c",
              commentLocation = Just
                "simple_structs.h:11:11",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "simple_structs.h:8:16",
            declId = NamePair {
              nameC = Name "S2_t",
              nameHsIdent = Identifier
                "S2_t"},
            declOrigin =
            NameOriginRenamedFrom
              (Name "S2"),
            declAliases = [Name "S2_t"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["simple_structs.h"],
                headerInclude =
                "simple_structs.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "S2_t"),
              structSizeof = 12,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:9:10",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s2_t_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:10:9",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier
                        "s2_t_b"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:11:11",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = Identifier
                        "s2_t_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimFloat),
                  structFieldOffset = 64,
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
          commentOrigin = Just "S2_t",
          commentLocation = Just
            "simple_structs.h:8:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["simple_structs.h"],
              headerInclude =
              "simple_structs.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "S2_t",
          structConstr = Name
            "@NsConstr"
            "S2_t",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "s2_t_a",
              fieldType = HsPrimType
                HsPrimCChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:9:10",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s2_t_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "a",
                  commentLocation = Just
                    "simple_structs.h:9:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["simple_structs.h"],
                      headerInclude =
                      "simple_structs.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "s2_t_b",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:10:9",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier
                        "s2_t_b"},
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
                    "simple_structs.h:10:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["simple_structs.h"],
                      headerInclude =
                      "simple_structs.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "s2_t_c",
              fieldType = HsPrimType
                HsPrimCFloat,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:11:11",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = Identifier
                        "s2_t_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimFloat),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "c",
                  commentLocation = Just
                    "simple_structs.h:11:11",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["simple_structs.h"],
                      headerInclude =
                      "simple_structs.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "simple_structs.h:8:16",
                declId = NamePair {
                  nameC = Name "S2_t",
                  nameHsIdent = Identifier
                    "S2_t"},
                declOrigin =
                NameOriginRenamedFrom
                  (Name "S2"),
                declAliases = [Name "S2_t"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["simple_structs.h"],
                    headerInclude =
                    "simple_structs.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "S2_t"),
                  structSizeof = 12,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:9:10",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = Identifier
                            "s2_t_a"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:10:9",
                        fieldName = NamePair {
                          nameC = Name "b",
                          nameHsIdent = Identifier
                            "s2_t_b"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:11:11",
                        fieldName = NamePair {
                          nameC = Name "c",
                          nameHsIdent = Identifier
                            "s2_t_c"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimFloating PrimFloat),
                      structFieldOffset = 64,
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
              commentOrigin = Just "S2_t",
              commentLocation = Just
                "simple_structs.h:8:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
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
                    "S2_t",
                  structConstr = Name
                    "@NsConstr"
                    "S2_t",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s2_t_a",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:9:10",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s2_t_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "simple_structs.h:9:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s2_t_b",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:10:9",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier
                                "s2_t_b"},
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
                            "simple_structs.h:10:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s2_t_c",
                      fieldType = HsPrimType
                        HsPrimCFloat,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:11:11",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = Identifier
                                "s2_t_c"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimFloat),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "simple_structs.h:11:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "simple_structs.h:8:16",
                        declId = NamePair {
                          nameC = Name "S2_t",
                          nameHsIdent = Identifier
                            "S2_t"},
                        declOrigin =
                        NameOriginRenamedFrom
                          (Name "S2"),
                        declAliases = [Name "S2_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["simple_structs.h"],
                            headerInclude =
                            "simple_structs.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S2_t"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:9:10",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s2_t_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:10:9",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier
                                    "s2_t_b"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:11:11",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = Identifier
                                    "s2_t_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimFloat),
                              structFieldOffset = 64,
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
                      commentOrigin = Just "S2_t",
                      commentLocation = Just
                        "simple_structs.h:8:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["simple_structs.h"],
                          headerInclude =
                          "simple_structs.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "s2_t_a")
                  (Idx 0),
                PeekCField
                  (HsStrLit "s2_t_b")
                  (Idx 0),
                PeekCField
                  (HsStrLit "s2_t_c")
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
                    "S2_t",
                  structConstr = Name
                    "@NsConstr"
                    "S2_t",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s2_t_a",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:9:10",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s2_t_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "simple_structs.h:9:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s2_t_b",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:10:9",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier
                                "s2_t_b"},
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
                            "simple_structs.h:10:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s2_t_c",
                      fieldType = HsPrimType
                        HsPrimCFloat,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:11:11",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = Identifier
                                "s2_t_c"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimFloat),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "simple_structs.h:11:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "simple_structs.h:8:16",
                        declId = NamePair {
                          nameC = Name "S2_t",
                          nameHsIdent = Identifier
                            "S2_t"},
                        declOrigin =
                        NameOriginRenamedFrom
                          (Name "S2"),
                        declAliases = [Name "S2_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["simple_structs.h"],
                            headerInclude =
                            "simple_structs.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S2_t"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:9:10",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s2_t_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:10:9",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier
                                    "s2_t_b"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:11:11",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = Identifier
                                    "s2_t_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimFloat),
                              structFieldOffset = 64,
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
                      commentOrigin = Just "S2_t",
                      commentLocation = Just
                        "simple_structs.h:8:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["simple_structs.h"],
                          headerInclude =
                          "simple_structs.h"},
                      commentChildren = []}}
                (Add 3)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "s2_t_a")
                      (Idx 4)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "s2_t_b")
                      (Idx 4)
                      (Idx 1),
                    PokeCField
                      (HsStrLit "s2_t_c")
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
        "S2_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S2_t",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "S2_t"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "s2_t_a",
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
            (Name "@NsTypeConstr" "S2_t"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "s2_t_a",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCChar,
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
            (Name "@NsTypeConstr" "S2_t"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "s2_t_b",
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
            (Name "@NsTypeConstr" "S2_t"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "s2_t_b",
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
            (Name "@NsTypeConstr" "S2_t"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "s2_t_c",
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
            (Name "@NsTypeConstr" "S2_t"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "s2_t_c",
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
        "S3_t",
      structConstr = Name
        "@NsConstr"
        "S3_t",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "s3_t_a",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "simple_structs.h:16:10",
                fieldName = NamePair {
                  nameC = Name "a",
                  nameHsIdent = Identifier
                    "s3_t_a"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "a",
              commentLocation = Just
                "simple_structs.h:16:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "simple_structs.h:15:9",
            declId = NamePair {
              nameC = Name "S3_t",
              nameHsIdent = Identifier
                "S3_t"},
            declOrigin = NameOriginGenerated
              (AnonId
                "simple_structs.h:15:9"),
            declAliases = [Name "S3_t"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["simple_structs.h"],
                headerInclude =
                "simple_structs.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "S3_t"),
              structSizeof = 1,
              structAlignment = 1,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:16:10",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s3_t_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
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
          commentOrigin = Nothing,
          commentLocation = Just
            "simple_structs.h:15:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["simple_structs.h"],
              headerInclude =
              "simple_structs.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "S3_t",
          structConstr = Name
            "@NsConstr"
            "S3_t",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "s3_t_a",
              fieldType = HsPrimType
                HsPrimCChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:16:10",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s3_t_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "a",
                  commentLocation = Just
                    "simple_structs.h:16:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["simple_structs.h"],
                      headerInclude =
                      "simple_structs.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "simple_structs.h:15:9",
                declId = NamePair {
                  nameC = Name "S3_t",
                  nameHsIdent = Identifier
                    "S3_t"},
                declOrigin = NameOriginGenerated
                  (AnonId
                    "simple_structs.h:15:9"),
                declAliases = [Name "S3_t"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["simple_structs.h"],
                    headerInclude =
                    "simple_structs.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "S3_t"),
                  structSizeof = 1,
                  structAlignment = 1,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:16:10",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = Identifier
                            "s3_t_a"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
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
              commentOrigin = Nothing,
              commentLocation = Just
                "simple_structs.h:15:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 1,
          storableAlignment = 1,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "S3_t",
                  structConstr = Name
                    "@NsConstr"
                    "S3_t",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s3_t_a",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:16:10",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s3_t_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "simple_structs.h:16:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "simple_structs.h:15:9",
                        declId = NamePair {
                          nameC = Name "S3_t",
                          nameHsIdent = Identifier
                            "S3_t"},
                        declOrigin = NameOriginGenerated
                          (AnonId
                            "simple_structs.h:15:9"),
                        declAliases = [Name "S3_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["simple_structs.h"],
                            headerInclude =
                            "simple_structs.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S3_t"),
                          structSizeof = 1,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:16:10",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s3_t_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
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
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "simple_structs.h:15:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["simple_structs.h"],
                          headerInclude =
                          "simple_structs.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "s3_t_a")
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
                    "S3_t",
                  structConstr = Name
                    "@NsConstr"
                    "S3_t",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s3_t_a",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:16:10",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s3_t_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "simple_structs.h:16:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "simple_structs.h:15:9",
                        declId = NamePair {
                          nameC = Name "S3_t",
                          nameHsIdent = Identifier
                            "S3_t"},
                        declOrigin = NameOriginGenerated
                          (AnonId
                            "simple_structs.h:15:9"),
                        declAliases = [Name "S3_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["simple_structs.h"],
                            headerInclude =
                            "simple_structs.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S3_t"),
                          structSizeof = 1,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:16:10",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s3_t_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
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
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "simple_structs.h:15:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["simple_structs.h"],
                          headerInclude =
                          "simple_structs.h"},
                      commentChildren = []}}
                (Add 1)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "s3_t_a")
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
        "S3_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S3_t",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "S3_t"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "s3_t_a",
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
            (Name "@NsTypeConstr" "S3_t"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "s3_t_a",
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
        "S4",
      structConstr = Name
        "@NsConstr"
        "S4",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "s4_b",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "simple_structs.h:20:10",
                fieldName = NamePair {
                  nameC = Name "b",
                  nameHsIdent = Identifier
                    "s4_b"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "b",
              commentLocation = Just
                "simple_structs.h:20:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "s4_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "simple_structs.h:21:9",
                fieldName = NamePair {
                  nameC = Name "a",
                  nameHsIdent = Identifier
                    "s4_a"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "a",
              commentLocation = Just
                "simple_structs.h:21:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "s4_c",
          fieldType = HsPtr
            (HsPrimType HsPrimCInt),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "simple_structs.h:22:10",
                fieldName = NamePair {
                  nameC = Name "c",
                  nameHsIdent = Identifier
                    "s4_c"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypePrim
                  (PrimIntegral PrimInt Signed)),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "c",
              commentLocation = Just
                "simple_structs.h:22:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "simple_structs.h:19:8",
            declId = NamePair {
              nameC = Name "S4",
              nameHsIdent = Identifier "S4"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["simple_structs.h"],
                headerInclude =
                "simple_structs.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "S4"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:20:10",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier
                        "s4_b"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:21:9",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s4_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:22:10",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = Identifier
                        "s4_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypePrim
                      (PrimIntegral PrimInt Signed)),
                  structFieldOffset = 64,
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
          commentOrigin = Just "S4",
          commentLocation = Just
            "simple_structs.h:19:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["simple_structs.h"],
              headerInclude =
              "simple_structs.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "S4",
          structConstr = Name
            "@NsConstr"
            "S4",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "s4_b",
              fieldType = HsPrimType
                HsPrimCChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:20:10",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier
                        "s4_b"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "b",
                  commentLocation = Just
                    "simple_structs.h:20:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["simple_structs.h"],
                      headerInclude =
                      "simple_structs.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "s4_a",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:21:9",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s4_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "a",
                  commentLocation = Just
                    "simple_structs.h:21:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["simple_structs.h"],
                      headerInclude =
                      "simple_structs.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "s4_c",
              fieldType = HsPtr
                (HsPrimType HsPrimCInt),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:22:10",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = Identifier
                        "s4_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypePrim
                      (PrimIntegral PrimInt Signed)),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "c",
                  commentLocation = Just
                    "simple_structs.h:22:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["simple_structs.h"],
                      headerInclude =
                      "simple_structs.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "simple_structs.h:19:8",
                declId = NamePair {
                  nameC = Name "S4",
                  nameHsIdent = Identifier "S4"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["simple_structs.h"],
                    headerInclude =
                    "simple_structs.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "S4"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:20:10",
                        fieldName = NamePair {
                          nameC = Name "b",
                          nameHsIdent = Identifier
                            "s4_b"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:21:9",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = Identifier
                            "s4_a"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:22:10",
                        fieldName = NamePair {
                          nameC = Name "c",
                          nameHsIdent = Identifier
                            "s4_c"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypePrim
                          (PrimIntegral PrimInt Signed)),
                      structFieldOffset = 64,
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
              commentOrigin = Just "S4",
              commentLocation = Just
                "simple_structs.h:19:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
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
                    "S4",
                  structConstr = Name
                    "@NsConstr"
                    "S4",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s4_b",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:20:10",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier
                                "s4_b"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "b",
                          commentLocation = Just
                            "simple_structs.h:20:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s4_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:21:9",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s4_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "simple_structs.h:21:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s4_c",
                      fieldType = HsPtr
                        (HsPrimType HsPrimCInt),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:22:10",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = Identifier
                                "s4_c"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypePrim
                              (PrimIntegral PrimInt Signed)),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "simple_structs.h:22:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "simple_structs.h:19:8",
                        declId = NamePair {
                          nameC = Name "S4",
                          nameHsIdent = Identifier "S4"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["simple_structs.h"],
                            headerInclude =
                            "simple_structs.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S4"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:20:10",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier
                                    "s4_b"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:21:9",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s4_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:22:10",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = Identifier
                                    "s4_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
                              structFieldOffset = 64,
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
                      commentOrigin = Just "S4",
                      commentLocation = Just
                        "simple_structs.h:19:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["simple_structs.h"],
                          headerInclude =
                          "simple_structs.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "s4_b")
                  (Idx 0),
                PeekCField
                  (HsStrLit "s4_a")
                  (Idx 0),
                PeekCField
                  (HsStrLit "s4_c")
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
                    "S4",
                  structConstr = Name
                    "@NsConstr"
                    "S4",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s4_b",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:20:10",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier
                                "s4_b"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "b",
                          commentLocation = Just
                            "simple_structs.h:20:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s4_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:21:9",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s4_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "simple_structs.h:21:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s4_c",
                      fieldType = HsPtr
                        (HsPrimType HsPrimCInt),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:22:10",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = Identifier
                                "s4_c"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypePrim
                              (PrimIntegral PrimInt Signed)),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "simple_structs.h:22:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "simple_structs.h:19:8",
                        declId = NamePair {
                          nameC = Name "S4",
                          nameHsIdent = Identifier "S4"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["simple_structs.h"],
                            headerInclude =
                            "simple_structs.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S4"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:20:10",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier
                                    "s4_b"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:21:9",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s4_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:22:10",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = Identifier
                                    "s4_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
                              structFieldOffset = 64,
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
                      commentOrigin = Just "S4",
                      commentLocation = Just
                        "simple_structs.h:19:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["simple_structs.h"],
                          headerInclude =
                          "simple_structs.h"},
                      commentChildren = []}}
                (Add 3)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "s4_b")
                      (Idx 4)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "s4_a")
                      (Idx 4)
                      (Idx 1),
                    PokeCField
                      (HsStrLit "s4_c")
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
        "S4",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S4",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "S4"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "s4_b",
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
            (Name "@NsTypeConstr" "S4"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "s4_b",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCChar,
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
            (Name "@NsTypeConstr" "S4"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "s4_a",
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
            (Name "@NsTypeConstr" "S4"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "s4_a",
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
            (Name "@NsTypeConstr" "S4"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "s4_c",
          hasCFieldInstanceCFieldType =
          HsPtr (HsPrimType HsPrimCInt),
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
            (Name "@NsTypeConstr" "S4"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "s4_c",
          hasFieldInstanceFieldType =
          HsPtr (HsPrimType HsPrimCInt),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "S5",
      structConstr = Name
        "@NsConstr"
        "S5",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "s5_a",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "simple_structs.h:27:10",
                fieldName = NamePair {
                  nameC = Name "a",
                  nameHsIdent = Identifier
                    "s5_a"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "a",
              commentLocation = Just
                "simple_structs.h:27:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "s5_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "simple_structs.h:28:9",
                fieldName = NamePair {
                  nameC = Name "b",
                  nameHsIdent = Identifier
                    "s5_b"},
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
                "simple_structs.h:28:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "simple_structs.h:26:16",
            declId = NamePair {
              nameC = Name "S5",
              nameHsIdent = Identifier "S5"},
            declOrigin = NameOriginInSource,
            declAliases = [Name "S5"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["simple_structs.h"],
                headerInclude =
                "simple_structs.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "S5"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:27:10",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s5_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:28:9",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier
                        "s5_b"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
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
          commentOrigin = Just "S5",
          commentLocation = Just
            "simple_structs.h:26:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["simple_structs.h"],
              headerInclude =
              "simple_structs.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "S5",
          structConstr = Name
            "@NsConstr"
            "S5",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "s5_a",
              fieldType = HsPrimType
                HsPrimCChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:27:10",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s5_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "a",
                  commentLocation = Just
                    "simple_structs.h:27:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["simple_structs.h"],
                      headerInclude =
                      "simple_structs.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "s5_b",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:28:9",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier
                        "s5_b"},
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
                    "simple_structs.h:28:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["simple_structs.h"],
                      headerInclude =
                      "simple_structs.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "simple_structs.h:26:16",
                declId = NamePair {
                  nameC = Name "S5",
                  nameHsIdent = Identifier "S5"},
                declOrigin = NameOriginInSource,
                declAliases = [Name "S5"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["simple_structs.h"],
                    headerInclude =
                    "simple_structs.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "S5"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:27:10",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = Identifier
                            "s5_a"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:28:9",
                        fieldName = NamePair {
                          nameC = Name "b",
                          nameHsIdent = Identifier
                            "s5_b"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 32,
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
              commentOrigin = Just "S5",
              commentLocation = Just
                "simple_structs.h:26:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
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
                    "S5",
                  structConstr = Name
                    "@NsConstr"
                    "S5",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s5_a",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:27:10",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s5_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "simple_structs.h:27:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s5_b",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:28:9",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier
                                "s5_b"},
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
                            "simple_structs.h:28:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "simple_structs.h:26:16",
                        declId = NamePair {
                          nameC = Name "S5",
                          nameHsIdent = Identifier "S5"},
                        declOrigin = NameOriginInSource,
                        declAliases = [Name "S5"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["simple_structs.h"],
                            headerInclude =
                            "simple_structs.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S5"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:27:10",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s5_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:28:9",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier
                                    "s5_b"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
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
                      commentOrigin = Just "S5",
                      commentLocation = Just
                        "simple_structs.h:26:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["simple_structs.h"],
                          headerInclude =
                          "simple_structs.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "s5_a")
                  (Idx 0),
                PeekCField
                  (HsStrLit "s5_b")
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
                    "S5",
                  structConstr = Name
                    "@NsConstr"
                    "S5",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s5_a",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:27:10",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s5_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "simple_structs.h:27:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s5_b",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:28:9",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier
                                "s5_b"},
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
                            "simple_structs.h:28:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "simple_structs.h:26:16",
                        declId = NamePair {
                          nameC = Name "S5",
                          nameHsIdent = Identifier "S5"},
                        declOrigin = NameOriginInSource,
                        declAliases = [Name "S5"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["simple_structs.h"],
                            headerInclude =
                            "simple_structs.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S5"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:27:10",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s5_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:28:9",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier
                                    "s5_b"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
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
                      commentOrigin = Just "S5",
                      commentLocation = Just
                        "simple_structs.h:26:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["simple_structs.h"],
                          headerInclude =
                          "simple_structs.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "s5_a")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "s5_b")
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
        "S5",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S5",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "S5"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "s5_a",
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
            (Name "@NsTypeConstr" "S5"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "s5_a",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCChar,
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
            (Name "@NsTypeConstr" "S5"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "s5_b",
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
            (Name "@NsTypeConstr" "S5"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "s5_b",
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
        "S6",
      structConstr = Name
        "@NsConstr"
        "S6",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "s6_a",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "simple_structs.h:31:18",
                fieldName = NamePair {
                  nameC = Name "a",
                  nameHsIdent = Identifier
                    "s6_a"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "a",
              commentLocation = Just
                "simple_structs.h:31:18",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "s6_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "simple_structs.h:31:25",
                fieldName = NamePair {
                  nameC = Name "b",
                  nameHsIdent = Identifier
                    "s6_b"},
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
                "simple_structs.h:31:25",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "simple_structs.h:31:8",
            declId = NamePair {
              nameC = Name "S6",
              nameHsIdent = Identifier "S6"},
            declOrigin = NameOriginInSource,
            declAliases = [Name "S6"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["simple_structs.h"],
                headerInclude =
                "simple_structs.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "S6"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:31:18",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s6_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:31:25",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier
                        "s6_b"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
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
          commentOrigin = Just "S6",
          commentLocation = Just
            "simple_structs.h:31:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["simple_structs.h"],
              headerInclude =
              "simple_structs.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "S6",
          structConstr = Name
            "@NsConstr"
            "S6",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "s6_a",
              fieldType = HsPrimType
                HsPrimCChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:31:18",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s6_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "a",
                  commentLocation = Just
                    "simple_structs.h:31:18",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["simple_structs.h"],
                      headerInclude =
                      "simple_structs.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "s6_b",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:31:25",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier
                        "s6_b"},
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
                    "simple_structs.h:31:25",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["simple_structs.h"],
                      headerInclude =
                      "simple_structs.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "simple_structs.h:31:8",
                declId = NamePair {
                  nameC = Name "S6",
                  nameHsIdent = Identifier "S6"},
                declOrigin = NameOriginInSource,
                declAliases = [Name "S6"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["simple_structs.h"],
                    headerInclude =
                    "simple_structs.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "S6"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:31:18",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = Identifier
                            "s6_a"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:31:25",
                        fieldName = NamePair {
                          nameC = Name "b",
                          nameHsIdent = Identifier
                            "s6_b"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 32,
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
              commentOrigin = Just "S6",
              commentLocation = Just
                "simple_structs.h:31:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
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
                    "S6",
                  structConstr = Name
                    "@NsConstr"
                    "S6",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s6_a",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:31:18",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s6_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "simple_structs.h:31:18",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s6_b",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:31:25",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier
                                "s6_b"},
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
                            "simple_structs.h:31:25",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "simple_structs.h:31:8",
                        declId = NamePair {
                          nameC = Name "S6",
                          nameHsIdent = Identifier "S6"},
                        declOrigin = NameOriginInSource,
                        declAliases = [Name "S6"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["simple_structs.h"],
                            headerInclude =
                            "simple_structs.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S6"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:31:18",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s6_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:31:25",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier
                                    "s6_b"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
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
                      commentOrigin = Just "S6",
                      commentLocation = Just
                        "simple_structs.h:31:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["simple_structs.h"],
                          headerInclude =
                          "simple_structs.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "s6_a")
                  (Idx 0),
                PeekCField
                  (HsStrLit "s6_b")
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
                    "S6",
                  structConstr = Name
                    "@NsConstr"
                    "S6",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s6_a",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:31:18",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s6_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "simple_structs.h:31:18",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s6_b",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:31:25",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier
                                "s6_b"},
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
                            "simple_structs.h:31:25",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "simple_structs.h:31:8",
                        declId = NamePair {
                          nameC = Name "S6",
                          nameHsIdent = Identifier "S6"},
                        declOrigin = NameOriginInSource,
                        declAliases = [Name "S6"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["simple_structs.h"],
                            headerInclude =
                            "simple_structs.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S6"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:31:18",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s6_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:31:25",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier
                                    "s6_b"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
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
                      commentOrigin = Just "S6",
                      commentLocation = Just
                        "simple_structs.h:31:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["simple_structs.h"],
                          headerInclude =
                          "simple_structs.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "s6_a")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "s6_b")
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
        "S6",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S6",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "S6"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "s6_a",
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
            (Name "@NsTypeConstr" "S6"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "s6_a",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCChar,
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
            (Name "@NsTypeConstr" "S6"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "s6_b",
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
            (Name "@NsTypeConstr" "S6"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "s6_b",
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
        "S7a_Deref",
      structConstr = Name
        "@NsConstr"
        "S7a_Deref",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "s7a_Deref_a",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "simple_structs.h:34:23",
                fieldName = NamePair {
                  nameC = Name "a",
                  nameHsIdent = Identifier
                    "s7a_Deref_a"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "a",
              commentLocation = Just
                "simple_structs.h:34:23",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "s7a_Deref_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "simple_structs.h:34:30",
                fieldName = NamePair {
                  nameC = Name "b",
                  nameHsIdent = Identifier
                    "s7a_Deref_b"},
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
                "simple_structs.h:34:30",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "simple_structs.h:34:9",
            declId = NamePair {
              nameC = Name "S7a_Deref",
              nameHsIdent = Identifier
                "S7a_Deref"},
            declOrigin = NameOriginGenerated
              (AnonId
                "simple_structs.h:34:9"),
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["simple_structs.h"],
                headerInclude =
                "simple_structs.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "S7a_Deref"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:34:23",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s7a_Deref_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:34:30",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier
                        "s7a_Deref_b"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
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
          commentOrigin = Nothing,
          commentLocation = Just
            "simple_structs.h:34:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["simple_structs.h"],
              headerInclude =
              "simple_structs.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "S7a_Deref",
          structConstr = Name
            "@NsConstr"
            "S7a_Deref",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "s7a_Deref_a",
              fieldType = HsPrimType
                HsPrimCChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:34:23",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s7a_Deref_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "a",
                  commentLocation = Just
                    "simple_structs.h:34:23",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["simple_structs.h"],
                      headerInclude =
                      "simple_structs.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "s7a_Deref_b",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:34:30",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier
                        "s7a_Deref_b"},
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
                    "simple_structs.h:34:30",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["simple_structs.h"],
                      headerInclude =
                      "simple_structs.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "simple_structs.h:34:9",
                declId = NamePair {
                  nameC = Name "S7a_Deref",
                  nameHsIdent = Identifier
                    "S7a_Deref"},
                declOrigin = NameOriginGenerated
                  (AnonId
                    "simple_structs.h:34:9"),
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["simple_structs.h"],
                    headerInclude =
                    "simple_structs.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "S7a_Deref"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:34:23",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = Identifier
                            "s7a_Deref_a"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:34:30",
                        fieldName = NamePair {
                          nameC = Name "b",
                          nameHsIdent = Identifier
                            "s7a_Deref_b"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 32,
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
              commentOrigin = Nothing,
              commentLocation = Just
                "simple_structs.h:34:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
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
                    "S7a_Deref",
                  structConstr = Name
                    "@NsConstr"
                    "S7a_Deref",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s7a_Deref_a",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:34:23",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s7a_Deref_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "simple_structs.h:34:23",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s7a_Deref_b",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:34:30",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier
                                "s7a_Deref_b"},
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
                            "simple_structs.h:34:30",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "simple_structs.h:34:9",
                        declId = NamePair {
                          nameC = Name "S7a_Deref",
                          nameHsIdent = Identifier
                            "S7a_Deref"},
                        declOrigin = NameOriginGenerated
                          (AnonId
                            "simple_structs.h:34:9"),
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["simple_structs.h"],
                            headerInclude =
                            "simple_structs.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S7a_Deref"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:34:23",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s7a_Deref_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:34:30",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier
                                    "s7a_Deref_b"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
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
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "simple_structs.h:34:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["simple_structs.h"],
                          headerInclude =
                          "simple_structs.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "s7a_Deref_a")
                  (Idx 0),
                PeekCField
                  (HsStrLit "s7a_Deref_b")
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
                    "S7a_Deref",
                  structConstr = Name
                    "@NsConstr"
                    "S7a_Deref",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s7a_Deref_a",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:34:23",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s7a_Deref_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "simple_structs.h:34:23",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s7a_Deref_b",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:34:30",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier
                                "s7a_Deref_b"},
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
                            "simple_structs.h:34:30",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "simple_structs.h:34:9",
                        declId = NamePair {
                          nameC = Name "S7a_Deref",
                          nameHsIdent = Identifier
                            "S7a_Deref"},
                        declOrigin = NameOriginGenerated
                          (AnonId
                            "simple_structs.h:34:9"),
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["simple_structs.h"],
                            headerInclude =
                            "simple_structs.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S7a_Deref"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:34:23",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s7a_Deref_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:34:30",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier
                                    "s7a_Deref_b"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
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
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "simple_structs.h:34:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["simple_structs.h"],
                          headerInclude =
                          "simple_structs.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "s7a_Deref_a")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "s7a_Deref_b")
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
        "S7a_Deref",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S7a_Deref",
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
              "S7a_Deref"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "s7a_Deref_a",
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
              "S7a_Deref"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "s7a_Deref_a",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCChar,
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
              "S7a_Deref"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "s7a_Deref_b",
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
              "S7a_Deref"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "s7a_Deref_b",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "S7a",
      newtypeConstr = Name
        "@NsConstr"
        "S7a",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_S7a",
        fieldType = HsPtr
          (HsTypRef
            (Name
              "@NsTypeConstr"
              "S7a_Deref")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "simple_structs.h:34:36",
          declId = NamePair {
            nameC = Name "S7a",
            nameHsIdent = Identifier "S7a"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["simple_structs.h"],
              headerInclude =
              "simple_structs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "S7a",
              newtypeField = Name
                "@NsVar"
                "un_S7a"},
            typedefType = TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name "S7a_Deref",
                  nameHsIdent = Identifier
                    "S7a_Deref"}
                (NameOriginGenerated
                  (AnonId
                    "simple_structs.h:34:9")))},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "S7a",
          commentLocation = Just
            "simple_structs.h:34:36",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["simple_structs.h"],
              headerInclude =
              "simple_structs.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S7a",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S7a",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S7a",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S7a",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "S7a"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_S7a",
          hasFieldInstanceFieldType =
          HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "S7a_Deref")),
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
            (Name "@NsTypeConstr" "S7a"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_S7a",
          hasCFieldInstanceCFieldType =
          HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "S7a_Deref")),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "S7b_Deref",
      structConstr = Name
        "@NsConstr"
        "S7b_Deref",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "s7b_Deref_a",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "simple_structs.h:35:23",
                fieldName = NamePair {
                  nameC = Name "a",
                  nameHsIdent = Identifier
                    "s7b_Deref_a"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "a",
              commentLocation = Just
                "simple_structs.h:35:23",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "s7b_Deref_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "simple_structs.h:35:30",
                fieldName = NamePair {
                  nameC = Name "b",
                  nameHsIdent = Identifier
                    "s7b_Deref_b"},
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
                "simple_structs.h:35:30",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "simple_structs.h:35:9",
            declId = NamePair {
              nameC = Name "S7b_Deref",
              nameHsIdent = Identifier
                "S7b_Deref"},
            declOrigin = NameOriginGenerated
              (AnonId
                "simple_structs.h:35:9"),
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["simple_structs.h"],
                headerInclude =
                "simple_structs.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "S7b_Deref"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:35:23",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s7b_Deref_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:35:30",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier
                        "s7b_Deref_b"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
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
          commentOrigin = Nothing,
          commentLocation = Just
            "simple_structs.h:35:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["simple_structs.h"],
              headerInclude =
              "simple_structs.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "S7b_Deref",
          structConstr = Name
            "@NsConstr"
            "S7b_Deref",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "s7b_Deref_a",
              fieldType = HsPrimType
                HsPrimCChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:35:23",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "s7b_Deref_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "a",
                  commentLocation = Just
                    "simple_structs.h:35:23",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["simple_structs.h"],
                      headerInclude =
                      "simple_structs.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "s7b_Deref_b",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:35:30",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier
                        "s7b_Deref_b"},
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
                    "simple_structs.h:35:30",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["simple_structs.h"],
                      headerInclude =
                      "simple_structs.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "simple_structs.h:35:9",
                declId = NamePair {
                  nameC = Name "S7b_Deref",
                  nameHsIdent = Identifier
                    "S7b_Deref"},
                declOrigin = NameOriginGenerated
                  (AnonId
                    "simple_structs.h:35:9"),
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["simple_structs.h"],
                    headerInclude =
                    "simple_structs.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "S7b_Deref"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:35:23",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = Identifier
                            "s7b_Deref_a"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:35:30",
                        fieldName = NamePair {
                          nameC = Name "b",
                          nameHsIdent = Identifier
                            "s7b_Deref_b"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 32,
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
              commentOrigin = Nothing,
              commentLocation = Just
                "simple_structs.h:35:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["simple_structs.h"],
                  headerInclude =
                  "simple_structs.h"},
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
                    "S7b_Deref",
                  structConstr = Name
                    "@NsConstr"
                    "S7b_Deref",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s7b_Deref_a",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:35:23",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s7b_Deref_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "simple_structs.h:35:23",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s7b_Deref_b",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:35:30",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier
                                "s7b_Deref_b"},
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
                            "simple_structs.h:35:30",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "simple_structs.h:35:9",
                        declId = NamePair {
                          nameC = Name "S7b_Deref",
                          nameHsIdent = Identifier
                            "S7b_Deref"},
                        declOrigin = NameOriginGenerated
                          (AnonId
                            "simple_structs.h:35:9"),
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["simple_structs.h"],
                            headerInclude =
                            "simple_structs.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S7b_Deref"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:35:23",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s7b_Deref_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:35:30",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier
                                    "s7b_Deref_b"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
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
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "simple_structs.h:35:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["simple_structs.h"],
                          headerInclude =
                          "simple_structs.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "s7b_Deref_a")
                  (Idx 0),
                PeekCField
                  (HsStrLit "s7b_Deref_b")
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
                    "S7b_Deref",
                  structConstr = Name
                    "@NsConstr"
                    "S7b_Deref",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s7b_Deref_a",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:35:23",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "s7b_Deref_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "simple_structs.h:35:23",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "s7b_Deref_b",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "simple_structs.h:35:30",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier
                                "s7b_Deref_b"},
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
                            "simple_structs.h:35:30",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["simple_structs.h"],
                              headerInclude =
                              "simple_structs.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "simple_structs.h:35:9",
                        declId = NamePair {
                          nameC = Name "S7b_Deref",
                          nameHsIdent = Identifier
                            "S7b_Deref"},
                        declOrigin = NameOriginGenerated
                          (AnonId
                            "simple_structs.h:35:9"),
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["simple_structs.h"],
                            headerInclude =
                            "simple_structs.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "S7b_Deref"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:35:23",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "s7b_Deref_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:35:30",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier
                                    "s7b_Deref_b"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
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
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "simple_structs.h:35:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["simple_structs.h"],
                          headerInclude =
                          "simple_structs.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "s7b_Deref_a")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "s7b_Deref_b")
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
        "S7b_Deref",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S7b_Deref",
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
              "S7b_Deref"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "s7b_Deref_a",
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
              "S7b_Deref"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "s7b_Deref_a",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCChar,
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
              "S7b_Deref"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "s7b_Deref_b",
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
              "S7b_Deref"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "s7b_Deref_b",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "S7b",
      newtypeConstr = Name
        "@NsConstr"
        "S7b",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_S7b",
        fieldType = HsPtr
          (HsPtr
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "S7b_Deref")))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "simple_structs.h:35:38",
          declId = NamePair {
            nameC = Name "S7b",
            nameHsIdent = Identifier "S7b"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["simple_structs.h"],
              headerInclude =
              "simple_structs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "S7b",
              newtypeField = Name
                "@NsVar"
                "un_S7b"},
            typedefType = TypePointer
              (TypePointer
                (TypePointer
                  (TypeStruct
                    NamePair {
                      nameC = Name "S7b_Deref",
                      nameHsIdent = Identifier
                        "S7b_Deref"}
                    (NameOriginGenerated
                      (AnonId
                        "simple_structs.h:35:9")))))},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "S7b",
          commentLocation = Just
            "simple_structs.h:35:38",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["simple_structs.h"],
              headerInclude =
              "simple_structs.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S7b",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S7b",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S7b",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S7b",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "S7b"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_S7b",
          hasFieldInstanceFieldType =
          HsPtr
            (HsPtr
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "S7b_Deref")))),
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
            (Name "@NsTypeConstr" "S7b"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_S7b",
          hasCFieldInstanceCFieldType =
          HsPtr
            (HsPtr
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "S7b_Deref")))),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing}]
