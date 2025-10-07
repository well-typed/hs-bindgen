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
              cTypeSpecModule = Nothing,
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
                  cTypeSpecModule = Nothing,
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
                          cTypeSpecModule = Nothing,
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
                          cTypeSpecModule = Nothing,
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
              cTypeSpecModule = Nothing,
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
                  cTypeSpecModule = Nothing,
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
                          cTypeSpecModule = Nothing,
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
                          cTypeSpecModule = Nothing,
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
      Nothing}]
