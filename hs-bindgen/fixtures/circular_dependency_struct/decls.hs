[
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "B",
      structConstr = Name
        "@NsConstr"
        "B",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "b_toA",
          fieldType = HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "A")),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "circular_dependency_struct.h:4:13",
                fieldName = NamePair {
                  nameC = Name "toA",
                  nameHsIdent = Identifier
                    "b_toA"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "a",
                    nameHsIdent = Identifier "A"}
                  NameOriginInSource),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "toA",
              commentLocation = Just
                "circular_dependency_struct.h:4:13",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "circular_dependency_struct.h"],
                  headerInclude =
                  "circular_dependency_struct.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "circular_dependency_struct.h:3:8",
            declId = NamePair {
              nameC = Name "b",
              nameHsIdent = Identifier "B"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  [
                    "circular_dependency_struct.h"],
                headerInclude =
                "circular_dependency_struct.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "B"),
              structSizeof = 8,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "circular_dependency_struct.h:4:13",
                    fieldName = NamePair {
                      nameC = Name "toA",
                      nameHsIdent = Identifier
                        "b_toA"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "a",
                        nameHsIdent = Identifier "A"}
                      NameOriginInSource),
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
          commentOrigin = Just "b",
          commentLocation = Just
            "circular_dependency_struct.h:3:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "circular_dependency_struct.h"],
              headerInclude =
              "circular_dependency_struct.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "B",
          structConstr = Name
            "@NsConstr"
            "B",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "b_toA",
              fieldType = HsPtr
                (HsTypRef
                  (Name "@NsTypeConstr" "A")),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "circular_dependency_struct.h:4:13",
                    fieldName = NamePair {
                      nameC = Name "toA",
                      nameHsIdent = Identifier
                        "b_toA"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "a",
                        nameHsIdent = Identifier "A"}
                      NameOriginInSource),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "toA",
                  commentLocation = Just
                    "circular_dependency_struct.h:4:13",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "circular_dependency_struct.h"],
                      headerInclude =
                      "circular_dependency_struct.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "circular_dependency_struct.h:3:8",
                declId = NamePair {
                  nameC = Name "b",
                  nameHsIdent = Identifier "B"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      [
                        "circular_dependency_struct.h"],
                    headerInclude =
                    "circular_dependency_struct.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "B"),
                  structSizeof = 8,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "circular_dependency_struct.h:4:13",
                        fieldName = NamePair {
                          nameC = Name "toA",
                          nameHsIdent = Identifier
                            "b_toA"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeStruct
                          NamePair {
                            nameC = Name "a",
                            nameHsIdent = Identifier "A"}
                          NameOriginInSource),
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
              commentOrigin = Just "b",
              commentLocation = Just
                "circular_dependency_struct.h:3:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "circular_dependency_struct.h"],
                  headerInclude =
                  "circular_dependency_struct.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 8,
          storableAlignment = 8,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "B",
                  structConstr = Name
                    "@NsConstr"
                    "B",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "b_toA",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name "@NsTypeConstr" "A")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "circular_dependency_struct.h:4:13",
                            fieldName = NamePair {
                              nameC = Name "toA",
                              nameHsIdent = Identifier
                                "b_toA"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "a",
                                nameHsIdent = Identifier "A"}
                              NameOriginInSource),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "toA",
                          commentLocation = Just
                            "circular_dependency_struct.h:4:13",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "circular_dependency_struct.h"],
                              headerInclude =
                              "circular_dependency_struct.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "circular_dependency_struct.h:3:8",
                        declId = NamePair {
                          nameC = Name "b",
                          nameHsIdent = Identifier "B"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              [
                                "circular_dependency_struct.h"],
                            headerInclude =
                            "circular_dependency_struct.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "B"),
                          structSizeof = 8,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "circular_dependency_struct.h:4:13",
                                fieldName = NamePair {
                                  nameC = Name "toA",
                                  nameHsIdent = Identifier
                                    "b_toA"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "a",
                                    nameHsIdent = Identifier "A"}
                                  NameOriginInSource),
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
                      commentOrigin = Just "b",
                      commentLocation = Just
                        "circular_dependency_struct.h:3:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            [
                              "circular_dependency_struct.h"],
                          headerInclude =
                          "circular_dependency_struct.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "b_toA")
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
                    "B",
                  structConstr = Name
                    "@NsConstr"
                    "B",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "b_toA",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name "@NsTypeConstr" "A")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "circular_dependency_struct.h:4:13",
                            fieldName = NamePair {
                              nameC = Name "toA",
                              nameHsIdent = Identifier
                                "b_toA"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "a",
                                nameHsIdent = Identifier "A"}
                              NameOriginInSource),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "toA",
                          commentLocation = Just
                            "circular_dependency_struct.h:4:13",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "circular_dependency_struct.h"],
                              headerInclude =
                              "circular_dependency_struct.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "circular_dependency_struct.h:3:8",
                        declId = NamePair {
                          nameC = Name "b",
                          nameHsIdent = Identifier "B"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              [
                                "circular_dependency_struct.h"],
                            headerInclude =
                            "circular_dependency_struct.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "B"),
                          structSizeof = 8,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "circular_dependency_struct.h:4:13",
                                fieldName = NamePair {
                                  nameC = Name "toA",
                                  nameHsIdent = Identifier
                                    "b_toA"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "a",
                                    nameHsIdent = Identifier "A"}
                                  NameOriginInSource),
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
                      commentOrigin = Just "b",
                      commentLocation = Just
                        "circular_dependency_struct.h:3:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            [
                              "circular_dependency_struct.h"],
                          headerInclude =
                          "circular_dependency_struct.h"},
                      commentChildren = []}}
                (Add 1)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "b_toA")
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
        "B",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "B",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "B"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "b_toA",
          hasCFieldInstanceCFieldType =
          HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "A")),
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
            (Name "@NsTypeConstr" "B"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "b_toA",
          hasFieldInstanceFieldType =
          HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "A")),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "A",
      structConstr = Name
        "@NsConstr"
        "A",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "a_toB",
          fieldType = HsTypRef
            (Name "@NsTypeConstr" "B"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "circular_dependency_struct.h:8:12",
                fieldName = NamePair {
                  nameC = Name "toB",
                  nameHsIdent = Identifier
                    "a_toB"},
                fieldComment = Nothing},
              structFieldType = TypeStruct
                NamePair {
                  nameC = Name "b",
                  nameHsIdent = Identifier "B"}
                NameOriginInSource,
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "toB",
              commentLocation = Just
                "circular_dependency_struct.h:8:12",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "circular_dependency_struct.h"],
                  headerInclude =
                  "circular_dependency_struct.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "circular_dependency_struct.h:7:8",
            declId = NamePair {
              nameC = Name "a",
              nameHsIdent = Identifier "A"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  [
                    "circular_dependency_struct.h"],
                headerInclude =
                "circular_dependency_struct.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "A"),
              structSizeof = 8,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "circular_dependency_struct.h:8:12",
                    fieldName = NamePair {
                      nameC = Name "toB",
                      nameHsIdent = Identifier
                        "a_toB"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier "B"}
                    NameOriginInSource,
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
          commentOrigin = Just "a",
          commentLocation = Just
            "circular_dependency_struct.h:7:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "circular_dependency_struct.h"],
              headerInclude =
              "circular_dependency_struct.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "A",
          structConstr = Name
            "@NsConstr"
            "A",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "a_toB",
              fieldType = HsTypRef
                (Name "@NsTypeConstr" "B"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "circular_dependency_struct.h:8:12",
                    fieldName = NamePair {
                      nameC = Name "toB",
                      nameHsIdent = Identifier
                        "a_toB"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier "B"}
                    NameOriginInSource,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "toB",
                  commentLocation = Just
                    "circular_dependency_struct.h:8:12",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "circular_dependency_struct.h"],
                      headerInclude =
                      "circular_dependency_struct.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "circular_dependency_struct.h:7:8",
                declId = NamePair {
                  nameC = Name "a",
                  nameHsIdent = Identifier "A"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      [
                        "circular_dependency_struct.h"],
                    headerInclude =
                    "circular_dependency_struct.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "A"),
                  structSizeof = 8,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "circular_dependency_struct.h:8:12",
                        fieldName = NamePair {
                          nameC = Name "toB",
                          nameHsIdent = Identifier
                            "a_toB"},
                        fieldComment = Nothing},
                      structFieldType = TypeStruct
                        NamePair {
                          nameC = Name "b",
                          nameHsIdent = Identifier "B"}
                        NameOriginInSource,
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
              commentOrigin = Just "a",
              commentLocation = Just
                "circular_dependency_struct.h:7:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "circular_dependency_struct.h"],
                  headerInclude =
                  "circular_dependency_struct.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 8,
          storableAlignment = 8,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "A",
                  structConstr = Name
                    "@NsConstr"
                    "A",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_toB",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "B"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "circular_dependency_struct.h:8:12",
                            fieldName = NamePair {
                              nameC = Name "toB",
                              nameHsIdent = Identifier
                                "a_toB"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier "B"}
                            NameOriginInSource,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "toB",
                          commentLocation = Just
                            "circular_dependency_struct.h:8:12",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "circular_dependency_struct.h"],
                              headerInclude =
                              "circular_dependency_struct.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "circular_dependency_struct.h:7:8",
                        declId = NamePair {
                          nameC = Name "a",
                          nameHsIdent = Identifier "A"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              [
                                "circular_dependency_struct.h"],
                            headerInclude =
                            "circular_dependency_struct.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "A"),
                          structSizeof = 8,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "circular_dependency_struct.h:8:12",
                                fieldName = NamePair {
                                  nameC = Name "toB",
                                  nameHsIdent = Identifier
                                    "a_toB"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier "B"}
                                NameOriginInSource,
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
                      commentOrigin = Just "a",
                      commentLocation = Just
                        "circular_dependency_struct.h:7:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            [
                              "circular_dependency_struct.h"],
                          headerInclude =
                          "circular_dependency_struct.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "a_toB")
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
                    "A",
                  structConstr = Name
                    "@NsConstr"
                    "A",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_toB",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "B"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "circular_dependency_struct.h:8:12",
                            fieldName = NamePair {
                              nameC = Name "toB",
                              nameHsIdent = Identifier
                                "a_toB"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier "B"}
                            NameOriginInSource,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "toB",
                          commentLocation = Just
                            "circular_dependency_struct.h:8:12",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "circular_dependency_struct.h"],
                              headerInclude =
                              "circular_dependency_struct.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "circular_dependency_struct.h:7:8",
                        declId = NamePair {
                          nameC = Name "a",
                          nameHsIdent = Identifier "A"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              [
                                "circular_dependency_struct.h"],
                            headerInclude =
                            "circular_dependency_struct.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "A"),
                          structSizeof = 8,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "circular_dependency_struct.h:8:12",
                                fieldName = NamePair {
                                  nameC = Name "toB",
                                  nameHsIdent = Identifier
                                    "a_toB"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier "B"}
                                NameOriginInSource,
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
                      commentOrigin = Just "a",
                      commentLocation = Just
                        "circular_dependency_struct.h:7:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            [
                              "circular_dependency_struct.h"],
                          headerInclude =
                          "circular_dependency_struct.h"},
                      commentChildren = []}}
                (Add 1)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "a_toB")
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
        "A",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "A",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "A"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "a_toB",
          hasCFieldInstanceCFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "B"),
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
            (Name "@NsTypeConstr" "A"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "a_toB",
          hasFieldInstanceFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "B"),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing}]
