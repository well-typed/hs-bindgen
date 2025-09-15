[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S1",
      structConstr = HsName
        "@NsConstr"
        "S1",
      structFields = [
        Field {
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
              nameHsIdent = HsIdentifier
                "S1"},
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
                (HsName "@NsConstr" "S1"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:3:9",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = HsIdentifier
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
                      nameHsIdent = HsIdentifier
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
          structName = HsName
            "@NsTypeConstr"
            "S1",
          structConstr = HsName
            "@NsConstr"
            "S1",
          structFields = [
            Field {
              fieldName = HsName
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
                      nameHsIdent = HsIdentifier
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
              fieldName = HsName
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
                      nameHsIdent = HsIdentifier
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
                  nameHsIdent = HsIdentifier
                    "S1"},
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
                    (HsName "@NsConstr" "S1"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:3:9",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
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
                  structName = HsName
                    "@NsTypeConstr"
                    "S1",
                  structConstr = HsName
                    "@NsConstr"
                    "S1",
                  structFields = [
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
                            "S1"},
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
                            (HsName "@NsConstr" "S1"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:3:9",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
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
                        "simple_structs.h:2:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["simple_structs.h"],
                          headerInclude =
                          "simple_structs.h"},
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
                  structName = HsName
                    "@NsTypeConstr"
                    "S1",
                  structConstr = HsName
                    "@NsConstr"
                    "S1",
                  structFields = [
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
                            "S1"},
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
                            (HsName "@NsConstr" "S1"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:3:9",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S1",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S2_t",
      structConstr = HsName
        "@NsConstr"
        "S2_t",
      structFields = [
        Field {
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
              nameHsIdent = HsIdentifier
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
                (HsName "@NsConstr" "S2_t"),
              structSizeof = 12,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:9:10",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = HsIdentifier
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
                      nameHsIdent = HsIdentifier
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
                      nameHsIdent = HsIdentifier
                        "s2_t_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimFloat),
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
          structName = HsName
            "@NsTypeConstr"
            "S2_t",
          structConstr = HsName
            "@NsConstr"
            "S2_t",
          structFields = [
            Field {
              fieldName = HsName
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
                      nameHsIdent = HsIdentifier
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
              fieldName = HsName
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
                      nameHsIdent = HsIdentifier
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
              fieldName = HsName
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
                      nameHsIdent = HsIdentifier
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
                  nameHsIdent = HsIdentifier
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
                    (HsName "@NsConstr" "S2_t"),
                  structSizeof = 12,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:9:10",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
                            "s2_t_c"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimFloating PrimFloat),
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
                  structName = HsName
                    "@NsTypeConstr"
                    "S2_t",
                  structConstr = HsName
                    "@NsConstr"
                    "S2_t",
                  structFields = [
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
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
                            (HsName "@NsConstr" "S2_t"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:9:10",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
                                    "s2_t_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimFloat),
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
                  structName = HsName
                    "@NsTypeConstr"
                    "S2_t",
                  structConstr = HsName
                    "@NsConstr"
                    "S2_t",
                  structFields = [
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
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
                            (HsName "@NsConstr" "S2_t"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:9:10",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
                                    "s2_t_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimFloat),
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S2_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S2_t",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S3_t",
      structConstr = HsName
        "@NsConstr"
        "S3_t",
      structFields = [
        Field {
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
              nameHsIdent = HsIdentifier
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
                (HsName "@NsConstr" "S3_t"),
              structSizeof = 1,
              structAlignment = 1,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:16:10",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = HsIdentifier
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
          commentOrigin = Just "S3_t",
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
          structName = HsName
            "@NsTypeConstr"
            "S3_t",
          structConstr = HsName
            "@NsConstr"
            "S3_t",
          structFields = [
            Field {
              fieldName = HsName
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
                      nameHsIdent = HsIdentifier
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
                  nameHsIdent = HsIdentifier
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
                    (HsName "@NsConstr" "S3_t"),
                  structSizeof = 1,
                  structAlignment = 1,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:16:10",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = HsIdentifier
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
              commentOrigin = Just "S3_t",
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
                  structName = HsName
                    "@NsTypeConstr"
                    "S3_t",
                  structConstr = HsName
                    "@NsConstr"
                    "S3_t",
                  structFields = [
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
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
                            (HsName "@NsConstr" "S3_t"),
                          structSizeof = 1,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:16:10",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
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
                      commentOrigin = Just "S3_t",
                      commentLocation = Just
                        "simple_structs.h:15:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["simple_structs.h"],
                          headerInclude =
                          "simple_structs.h"},
                      commentChildren = []}})
              [PeekByteOff (Idx 0) 0]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "S3_t",
                  structConstr = HsName
                    "@NsConstr"
                    "S3_t",
                  structFields = [
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
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
                            (HsName "@NsConstr" "S3_t"),
                          structSizeof = 1,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:16:10",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
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
                      commentOrigin = Just "S3_t",
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S3_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S3_t",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S4",
      structConstr = HsName
        "@NsConstr"
        "S4",
      structFields = [
        Field {
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
              nameHsIdent = HsIdentifier
                "S4"},
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
                (HsName "@NsConstr" "S4"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:20:10",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = HsIdentifier
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
                      nameHsIdent = HsIdentifier
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
                      nameHsIdent = HsIdentifier
                        "s4_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypePrim
                      (PrimIntegral PrimInt Signed)),
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
          structName = HsName
            "@NsTypeConstr"
            "S4",
          structConstr = HsName
            "@NsConstr"
            "S4",
          structFields = [
            Field {
              fieldName = HsName
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
                      nameHsIdent = HsIdentifier
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
              fieldName = HsName
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
                      nameHsIdent = HsIdentifier
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
              fieldName = HsName
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
                      nameHsIdent = HsIdentifier
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
                  nameHsIdent = HsIdentifier
                    "S4"},
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
                    (HsName "@NsConstr" "S4"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:20:10",
                        fieldName = NamePair {
                          nameC = Name "b",
                          nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
                            "s4_c"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypePrim
                          (PrimIntegral PrimInt Signed)),
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
                  structName = HsName
                    "@NsTypeConstr"
                    "S4",
                  structConstr = HsName
                    "@NsConstr"
                    "S4",
                  structFields = [
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
                            "S4"},
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
                            (HsName "@NsConstr" "S4"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:20:10",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
                                    "s4_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
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
                  structName = HsName
                    "@NsTypeConstr"
                    "S4",
                  structConstr = HsName
                    "@NsConstr"
                    "S4",
                  structFields = [
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
                            "S4"},
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
                            (HsName "@NsConstr" "S4"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:20:10",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
                                    "s4_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S4",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S4",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S5",
      structConstr = HsName
        "@NsConstr"
        "S5",
      structFields = [
        Field {
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
              nameHsIdent = HsIdentifier
                "S5"},
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
                (HsName "@NsConstr" "S5"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:27:10",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = HsIdentifier
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
                      nameHsIdent = HsIdentifier
                        "s5_b"},
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
          structName = HsName
            "@NsTypeConstr"
            "S5",
          structConstr = HsName
            "@NsConstr"
            "S5",
          structFields = [
            Field {
              fieldName = HsName
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
                      nameHsIdent = HsIdentifier
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
              fieldName = HsName
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
                      nameHsIdent = HsIdentifier
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
                  nameHsIdent = HsIdentifier
                    "S5"},
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
                    (HsName "@NsConstr" "S5"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:27:10",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
                            "s5_b"},
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
                  structName = HsName
                    "@NsTypeConstr"
                    "S5",
                  structConstr = HsName
                    "@NsConstr"
                    "S5",
                  structFields = [
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
                            "S5"},
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
                            (HsName "@NsConstr" "S5"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:27:10",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
                                    "s5_b"},
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
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 4]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "S5",
                  structConstr = HsName
                    "@NsConstr"
                    "S5",
                  structFields = [
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
                            "S5"},
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
                            (HsName "@NsConstr" "S5"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:27:10",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
                                    "s5_b"},
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S5",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S5",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S6",
      structConstr = HsName
        "@NsConstr"
        "S6",
      structFields = [
        Field {
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
              nameHsIdent = HsIdentifier
                "S6"},
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
                (HsName "@NsConstr" "S6"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:31:18",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = HsIdentifier
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
                      nameHsIdent = HsIdentifier
                        "s6_b"},
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
          structName = HsName
            "@NsTypeConstr"
            "S6",
          structConstr = HsName
            "@NsConstr"
            "S6",
          structFields = [
            Field {
              fieldName = HsName
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
                      nameHsIdent = HsIdentifier
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
              fieldName = HsName
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
                      nameHsIdent = HsIdentifier
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
                  nameHsIdent = HsIdentifier
                    "S6"},
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
                    (HsName "@NsConstr" "S6"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:31:18",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
                            "s6_b"},
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
                  structName = HsName
                    "@NsTypeConstr"
                    "S6",
                  structConstr = HsName
                    "@NsConstr"
                    "S6",
                  structFields = [
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
                            "S6"},
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
                            (HsName "@NsConstr" "S6"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:31:18",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
                                    "s6_b"},
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
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 4]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "S6",
                  structConstr = HsName
                    "@NsConstr"
                    "S6",
                  structFields = [
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
                            "S6"},
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
                            (HsName "@NsConstr" "S6"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:31:18",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
                                    "s6_b"},
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S6",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S6",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S7a_Deref",
      structConstr = HsName
        "@NsConstr"
        "S7a_Deref",
      structFields = [
        Field {
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
              nameHsIdent = HsIdentifier
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
                (HsName
                  "@NsConstr"
                  "S7a_Deref"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:34:23",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = HsIdentifier
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
                      nameHsIdent = HsIdentifier
                        "s7a_Deref_b"},
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
          commentOrigin = Just
            "S7a_Deref",
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
          structName = HsName
            "@NsTypeConstr"
            "S7a_Deref",
          structConstr = HsName
            "@NsConstr"
            "S7a_Deref",
          structFields = [
            Field {
              fieldName = HsName
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
                      nameHsIdent = HsIdentifier
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
              fieldName = HsName
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
                      nameHsIdent = HsIdentifier
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
                  nameHsIdent = HsIdentifier
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
                    (HsName
                      "@NsConstr"
                      "S7a_Deref"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:34:23",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
                            "s7a_Deref_b"},
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
              commentOrigin = Just
                "S7a_Deref",
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
                  structName = HsName
                    "@NsTypeConstr"
                    "S7a_Deref",
                  structConstr = HsName
                    "@NsConstr"
                    "S7a_Deref",
                  structFields = [
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
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
                            (HsName
                              "@NsConstr"
                              "S7a_Deref"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:34:23",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
                                    "s7a_Deref_b"},
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
                      commentOrigin = Just
                        "S7a_Deref",
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
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 4]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "S7a_Deref",
                  structConstr = HsName
                    "@NsConstr"
                    "S7a_Deref",
                  structFields = [
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
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
                            (HsName
                              "@NsConstr"
                              "S7a_Deref"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:34:23",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
                                    "s7a_Deref_b"},
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
                      commentOrigin = Just
                        "S7a_Deref",
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S7a_Deref",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S7a_Deref",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "S7a",
      newtypeConstr = HsName
        "@NsConstr"
        "S7a",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_S7a",
        fieldType = HsPtr
          (HsTypRef
            (HsName
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
            nameHsIdent = HsIdentifier
              "S7a"},
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
              newtypeConstr = HsName
                "@NsConstr"
                "S7a",
              newtypeField = HsName
                "@NsVar"
                "un_S7a"},
            typedefType = TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name "S7a_Deref",
                  nameHsIdent = HsIdentifier
                    "S7a_Deref"}
                (NameOriginGenerated
                  (AnonId
                    "simple_structs.h:34:9")))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S7a",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S7a",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S7a",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S7a",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S7b_Deref",
      structConstr = HsName
        "@NsConstr"
        "S7b_Deref",
      structFields = [
        Field {
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
              nameHsIdent = HsIdentifier
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
                (HsName
                  "@NsConstr"
                  "S7b_Deref"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "simple_structs.h:35:23",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = HsIdentifier
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
                      nameHsIdent = HsIdentifier
                        "s7b_Deref_b"},
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
          commentOrigin = Just
            "S7b_Deref",
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
          structName = HsName
            "@NsTypeConstr"
            "S7b_Deref",
          structConstr = HsName
            "@NsConstr"
            "S7b_Deref",
          structFields = [
            Field {
              fieldName = HsName
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
                      nameHsIdent = HsIdentifier
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
              fieldName = HsName
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
                      nameHsIdent = HsIdentifier
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
                  nameHsIdent = HsIdentifier
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
                    (HsName
                      "@NsConstr"
                      "S7b_Deref"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "simple_structs.h:35:23",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
                            "s7b_Deref_b"},
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
              commentOrigin = Just
                "S7b_Deref",
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
                  structName = HsName
                    "@NsTypeConstr"
                    "S7b_Deref",
                  structConstr = HsName
                    "@NsConstr"
                    "S7b_Deref",
                  structFields = [
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
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
                            (HsName
                              "@NsConstr"
                              "S7b_Deref"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:35:23",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
                                    "s7b_Deref_b"},
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
                      commentOrigin = Just
                        "S7b_Deref",
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
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 4]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "S7b_Deref",
                  structConstr = HsName
                    "@NsConstr"
                    "S7b_Deref",
                  structFields = [
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
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
                            (HsName
                              "@NsConstr"
                              "S7b_Deref"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "simple_structs.h:35:23",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
                                    "s7b_Deref_b"},
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
                      commentOrigin = Just
                        "S7b_Deref",
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S7b_Deref",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S7b_Deref",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "S7b",
      newtypeConstr = HsName
        "@NsConstr"
        "S7b",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_S7b",
        fieldType = HsPtr
          (HsPtr
            (HsPtr
              (HsTypRef
                (HsName
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
            nameHsIdent = HsIdentifier
              "S7b"},
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
              newtypeConstr = HsName
                "@NsConstr"
                "S7b",
              newtypeField = HsName
                "@NsVar"
                "un_S7b"},
            typedefType = TypePointer
              (TypePointer
                (TypePointer
                  (TypeStruct
                    NamePair {
                      nameC = Name "S7b_Deref",
                      nameHsIdent = HsIdentifier
                        "S7b_Deref"}
                    (NameOriginGenerated
                      (AnonId
                        "simple_structs.h:35:9")))))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S7b",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S7b",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S7b",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S7b",
      deriveInstanceComment =
      Nothing}]
