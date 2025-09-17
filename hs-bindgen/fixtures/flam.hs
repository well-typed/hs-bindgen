[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Pascal",
      structConstr = HsName
        "@NsConstr"
        "Pascal",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "pascal_len",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "flam.h:3:9",
                fieldName = NamePair {
                  nameC = Name "len",
                  nameHsIdent = HsIdentifier
                    "pascal_len"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "len",
              commentLocation = Just
                "flam.h:3:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["flam.h"],
                  headerInclude = "flam.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "flam.h:2:8",
            declId = NamePair {
              nameC = Name "pascal",
              nameHsIdent = HsIdentifier
                "Pascal"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["flam.h"],
                headerInclude = "flam.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Pascal"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "flam.h:3:9",
                    fieldName = NamePair {
                      nameC = Name "len",
                      nameHsIdent = HsIdentifier
                        "pascal_len"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing}],
              structFlam = Just
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "flam.h:4:10",
                    fieldName = NamePair {
                      nameC = Name "data",
                      nameHsIdent = HsIdentifier
                        "pascal_data"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing}},
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
          commentOrigin = Just "pascal",
          commentLocation = Just
            "flam.h:2:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["flam.h"],
              headerInclude = "flam.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Pascal",
          structConstr = HsName
            "@NsConstr"
            "Pascal",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "pascal_len",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "flam.h:3:9",
                    fieldName = NamePair {
                      nameC = Name "len",
                      nameHsIdent = HsIdentifier
                        "pascal_len"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "len",
                  commentLocation = Just
                    "flam.h:3:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["flam.h"],
                      headerInclude = "flam.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "flam.h:2:8",
                declId = NamePair {
                  nameC = Name "pascal",
                  nameHsIdent = HsIdentifier
                    "Pascal"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["flam.h"],
                    headerInclude = "flam.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Pascal"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "flam.h:3:9",
                        fieldName = NamePair {
                          nameC = Name "len",
                          nameHsIdent = HsIdentifier
                            "pascal_len"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Just
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "flam.h:4:10",
                        fieldName = NamePair {
                          nameC = Name "data",
                          nameHsIdent = HsIdentifier
                            "pascal_data"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing}},
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
              commentOrigin = Just "pascal",
              commentLocation = Just
                "flam.h:2:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["flam.h"],
                  headerInclude = "flam.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 4,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Pascal",
                  structConstr = HsName
                    "@NsConstr"
                    "Pascal",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "pascal_len",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "flam.h:3:9",
                            fieldName = NamePair {
                              nameC = Name "len",
                              nameHsIdent = HsIdentifier
                                "pascal_len"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "len",
                          commentLocation = Just
                            "flam.h:3:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["flam.h"],
                              headerInclude = "flam.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "flam.h:2:8",
                        declId = NamePair {
                          nameC = Name "pascal",
                          nameHsIdent = HsIdentifier
                            "Pascal"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["flam.h"],
                            headerInclude = "flam.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Pascal"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "flam.h:3:9",
                                fieldName = NamePair {
                                  nameC = Name "len",
                                  nameHsIdent = HsIdentifier
                                    "pascal_len"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Just
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "flam.h:4:10",
                                fieldName = NamePair {
                                  nameC = Name "data",
                                  nameHsIdent = HsIdentifier
                                    "pascal_data"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing}},
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
                      commentOrigin = Just "pascal",
                      commentLocation = Just
                        "flam.h:2:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["flam.h"],
                          headerInclude = "flam.h"},
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
                    "Pascal",
                  structConstr = HsName
                    "@NsConstr"
                    "Pascal",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "pascal_len",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "flam.h:3:9",
                            fieldName = NamePair {
                              nameC = Name "len",
                              nameHsIdent = HsIdentifier
                                "pascal_len"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "len",
                          commentLocation = Just
                            "flam.h:3:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["flam.h"],
                              headerInclude = "flam.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "flam.h:2:8",
                        declId = NamePair {
                          nameC = Name "pascal",
                          nameHsIdent = HsIdentifier
                            "Pascal"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["flam.h"],
                            headerInclude = "flam.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Pascal"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "flam.h:3:9",
                                fieldName = NamePair {
                                  nameC = Name "len",
                                  nameHsIdent = HsIdentifier
                                    "pascal_len"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Just
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "flam.h:4:10",
                                fieldName = NamePair {
                                  nameC = Name "data",
                                  nameHsIdent = HsIdentifier
                                    "pascal_data"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing}},
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
                      commentOrigin = Just "pascal",
                      commentLocation = Just
                        "flam.h:2:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["flam.h"],
                          headerInclude = "flam.h"},
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
        "Pascal",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Pascal",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasFLAM
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Pascal",
          structConstr = HsName
            "@NsConstr"
            "Pascal",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "pascal_len",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "flam.h:3:9",
                    fieldName = NamePair {
                      nameC = Name "len",
                      nameHsIdent = HsIdentifier
                        "pascal_len"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "len",
                  commentLocation = Just
                    "flam.h:3:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["flam.h"],
                      headerInclude = "flam.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "flam.h:2:8",
                declId = NamePair {
                  nameC = Name "pascal",
                  nameHsIdent = HsIdentifier
                    "Pascal"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["flam.h"],
                    headerInclude = "flam.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Pascal"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "flam.h:3:9",
                        fieldName = NamePair {
                          nameC = Name "len",
                          nameHsIdent = HsIdentifier
                            "pascal_len"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Just
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "flam.h:4:10",
                        fieldName = NamePair {
                          nameC = Name "data",
                          nameHsIdent = HsIdentifier
                            "pascal_data"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing}},
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
              commentOrigin = Just "pascal",
              commentLocation = Just
                "flam.h:2:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["flam.h"],
                  headerInclude = "flam.h"},
              commentChildren = []}}
        (HsPrimType HsPrimCChar)
        4,
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Foo_bar",
      structConstr = HsName
        "@NsConstr"
        "Foo_bar",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "foo_bar_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "flam.h:11:7",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier
                    "foo_bar_x"},
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
                "flam.h:11:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["flam.h"],
                  headerInclude = "flam.h"},
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "foo_bar_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "flam.h:12:7",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier
                    "foo_bar_y"},
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
                "flam.h:12:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["flam.h"],
                  headerInclude = "flam.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "flam.h:10:2",
            declId = NamePair {
              nameC = Name "foo_bar",
              nameHsIdent = HsIdentifier
                "Foo_bar"},
            declOrigin = NameOriginGenerated
              (AnonId "flam.h:10:2"),
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["flam.h"],
                headerInclude = "flam.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Foo_bar"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "flam.h:11:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "foo_bar_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "flam.h:12:7",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "foo_bar_y"},
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
          commentOrigin = Just "foo_bar",
          commentLocation = Just
            "flam.h:10:2",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["flam.h"],
              headerInclude = "flam.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Foo_bar",
          structConstr = HsName
            "@NsConstr"
            "Foo_bar",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "foo_bar_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "flam.h:11:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "foo_bar_x"},
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
                    "flam.h:11:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["flam.h"],
                      headerInclude = "flam.h"},
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "foo_bar_y",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "flam.h:12:7",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "foo_bar_y"},
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
                    "flam.h:12:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["flam.h"],
                      headerInclude = "flam.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "flam.h:10:2",
                declId = NamePair {
                  nameC = Name "foo_bar",
                  nameHsIdent = HsIdentifier
                    "Foo_bar"},
                declOrigin = NameOriginGenerated
                  (AnonId "flam.h:10:2"),
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["flam.h"],
                    headerInclude = "flam.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Foo_bar"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "flam.h:11:7",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "foo_bar_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "flam.h:12:7",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = HsIdentifier
                            "foo_bar_y"},
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
              commentOrigin = Just "foo_bar",
              commentLocation = Just
                "flam.h:10:2",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["flam.h"],
                  headerInclude = "flam.h"},
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
                    "Foo_bar",
                  structConstr = HsName
                    "@NsConstr"
                    "Foo_bar",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "foo_bar_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "flam.h:11:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "foo_bar_x"},
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
                            "flam.h:11:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["flam.h"],
                              headerInclude = "flam.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "foo_bar_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "flam.h:12:7",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "foo_bar_y"},
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
                            "flam.h:12:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["flam.h"],
                              headerInclude = "flam.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "flam.h:10:2",
                        declId = NamePair {
                          nameC = Name "foo_bar",
                          nameHsIdent = HsIdentifier
                            "Foo_bar"},
                        declOrigin = NameOriginGenerated
                          (AnonId "flam.h:10:2"),
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["flam.h"],
                            headerInclude = "flam.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Foo_bar"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "flam.h:11:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "foo_bar_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "flam.h:12:7",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "foo_bar_y"},
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
                      commentOrigin = Just "foo_bar",
                      commentLocation = Just
                        "flam.h:10:2",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["flam.h"],
                          headerInclude = "flam.h"},
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
                    "Foo_bar",
                  structConstr = HsName
                    "@NsConstr"
                    "Foo_bar",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "foo_bar_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "flam.h:11:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "foo_bar_x"},
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
                            "flam.h:11:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["flam.h"],
                              headerInclude = "flam.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "foo_bar_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "flam.h:12:7",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "foo_bar_y"},
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
                            "flam.h:12:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["flam.h"],
                              headerInclude = "flam.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "flam.h:10:2",
                        declId = NamePair {
                          nameC = Name "foo_bar",
                          nameHsIdent = HsIdentifier
                            "Foo_bar"},
                        declOrigin = NameOriginGenerated
                          (AnonId "flam.h:10:2"),
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["flam.h"],
                            headerInclude = "flam.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Foo_bar"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "flam.h:11:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "foo_bar_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "flam.h:12:7",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "foo_bar_y"},
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
                      commentOrigin = Just "foo_bar",
                      commentLocation = Just
                        "flam.h:10:2",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["flam.h"],
                          headerInclude = "flam.h"},
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
        "Foo_bar",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Foo_bar",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Foo",
      structConstr = HsName
        "@NsConstr"
        "Foo",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "foo_len",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "flam.h:9:6",
                fieldName = NamePair {
                  nameC = Name "len",
                  nameHsIdent = HsIdentifier
                    "foo_len"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "len",
              commentLocation = Just
                "flam.h:9:6",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["flam.h"],
                  headerInclude = "flam.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "flam.h:8:8",
            declId = NamePair {
              nameC = Name "foo",
              nameHsIdent = HsIdentifier
                "Foo"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["flam.h"],
                headerInclude = "flam.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Foo"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "flam.h:9:6",
                    fieldName = NamePair {
                      nameC = Name "len",
                      nameHsIdent = HsIdentifier
                        "foo_len"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing}],
              structFlam = Just
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "flam.h:13:4",
                    fieldName = NamePair {
                      nameC = Name "bar",
                      nameHsIdent = HsIdentifier
                        "foo_bar"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "foo_bar",
                      nameHsIdent = HsIdentifier
                        "Foo_bar"}
                    (NameOriginGenerated
                      (AnonId "flam.h:10:2")),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing}},
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
          commentOrigin = Just "foo",
          commentLocation = Just
            "flam.h:8:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["flam.h"],
              headerInclude = "flam.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Foo",
          structConstr = HsName
            "@NsConstr"
            "Foo",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "foo_len",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "flam.h:9:6",
                    fieldName = NamePair {
                      nameC = Name "len",
                      nameHsIdent = HsIdentifier
                        "foo_len"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "len",
                  commentLocation = Just
                    "flam.h:9:6",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["flam.h"],
                      headerInclude = "flam.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "flam.h:8:8",
                declId = NamePair {
                  nameC = Name "foo",
                  nameHsIdent = HsIdentifier
                    "Foo"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["flam.h"],
                    headerInclude = "flam.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Foo"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "flam.h:9:6",
                        fieldName = NamePair {
                          nameC = Name "len",
                          nameHsIdent = HsIdentifier
                            "foo_len"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Just
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "flam.h:13:4",
                        fieldName = NamePair {
                          nameC = Name "bar",
                          nameHsIdent = HsIdentifier
                            "foo_bar"},
                        fieldComment = Nothing},
                      structFieldType = TypeStruct
                        NamePair {
                          nameC = Name "foo_bar",
                          nameHsIdent = HsIdentifier
                            "Foo_bar"}
                        (NameOriginGenerated
                          (AnonId "flam.h:10:2")),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing}},
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
              commentOrigin = Just "foo",
              commentLocation = Just
                "flam.h:8:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["flam.h"],
                  headerInclude = "flam.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 4,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Foo",
                  structConstr = HsName
                    "@NsConstr"
                    "Foo",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "foo_len",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "flam.h:9:6",
                            fieldName = NamePair {
                              nameC = Name "len",
                              nameHsIdent = HsIdentifier
                                "foo_len"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "len",
                          commentLocation = Just
                            "flam.h:9:6",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["flam.h"],
                              headerInclude = "flam.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "flam.h:8:8",
                        declId = NamePair {
                          nameC = Name "foo",
                          nameHsIdent = HsIdentifier
                            "Foo"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["flam.h"],
                            headerInclude = "flam.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Foo"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "flam.h:9:6",
                                fieldName = NamePair {
                                  nameC = Name "len",
                                  nameHsIdent = HsIdentifier
                                    "foo_len"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Just
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "flam.h:13:4",
                                fieldName = NamePair {
                                  nameC = Name "bar",
                                  nameHsIdent = HsIdentifier
                                    "foo_bar"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "foo_bar",
                                  nameHsIdent = HsIdentifier
                                    "Foo_bar"}
                                (NameOriginGenerated
                                  (AnonId "flam.h:10:2")),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing}},
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
                      commentOrigin = Just "foo",
                      commentLocation = Just
                        "flam.h:8:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["flam.h"],
                          headerInclude = "flam.h"},
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
                    "Foo",
                  structConstr = HsName
                    "@NsConstr"
                    "Foo",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "foo_len",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "flam.h:9:6",
                            fieldName = NamePair {
                              nameC = Name "len",
                              nameHsIdent = HsIdentifier
                                "foo_len"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "len",
                          commentLocation = Just
                            "flam.h:9:6",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["flam.h"],
                              headerInclude = "flam.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "flam.h:8:8",
                        declId = NamePair {
                          nameC = Name "foo",
                          nameHsIdent = HsIdentifier
                            "Foo"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["flam.h"],
                            headerInclude = "flam.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Foo"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "flam.h:9:6",
                                fieldName = NamePair {
                                  nameC = Name "len",
                                  nameHsIdent = HsIdentifier
                                    "foo_len"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Just
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "flam.h:13:4",
                                fieldName = NamePair {
                                  nameC = Name "bar",
                                  nameHsIdent = HsIdentifier
                                    "foo_bar"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "foo_bar",
                                  nameHsIdent = HsIdentifier
                                    "Foo_bar"}
                                (NameOriginGenerated
                                  (AnonId "flam.h:10:2")),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing}},
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
                      commentOrigin = Just "foo",
                      commentLocation = Just
                        "flam.h:8:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["flam.h"],
                          headerInclude = "flam.h"},
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
        "Foo",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Foo",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasFLAM
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Foo",
          structConstr = HsName
            "@NsConstr"
            "Foo",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "foo_len",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "flam.h:9:6",
                    fieldName = NamePair {
                      nameC = Name "len",
                      nameHsIdent = HsIdentifier
                        "foo_len"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "len",
                  commentLocation = Just
                    "flam.h:9:6",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["flam.h"],
                      headerInclude = "flam.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "flam.h:8:8",
                declId = NamePair {
                  nameC = Name "foo",
                  nameHsIdent = HsIdentifier
                    "Foo"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["flam.h"],
                    headerInclude = "flam.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Foo"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "flam.h:9:6",
                        fieldName = NamePair {
                          nameC = Name "len",
                          nameHsIdent = HsIdentifier
                            "foo_len"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Just
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "flam.h:13:4",
                        fieldName = NamePair {
                          nameC = Name "bar",
                          nameHsIdent = HsIdentifier
                            "foo_bar"},
                        fieldComment = Nothing},
                      structFieldType = TypeStruct
                        NamePair {
                          nameC = Name "foo_bar",
                          nameHsIdent = HsIdentifier
                            "Foo_bar"}
                        (NameOriginGenerated
                          (AnonId "flam.h:10:2")),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing}},
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
              commentOrigin = Just "foo",
              commentLocation = Just
                "flam.h:8:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["flam.h"],
                  headerInclude = "flam.h"},
              commentChildren = []}}
        (HsTypRef
          (HsName
            "@NsTypeConstr"
            "Foo_bar"))
        4,
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Diff",
      structConstr = HsName
        "@NsConstr"
        "Diff",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "diff_first",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "flam.h:18:7",
                fieldName = NamePair {
                  nameC = Name "first",
                  nameHsIdent = HsIdentifier
                    "diff_first"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "first",
              commentLocation = Just
                "flam.h:18:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["flam.h"],
                  headerInclude = "flam.h"},
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "diff_second",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "flam.h:19:7",
                fieldName = NamePair {
                  nameC = Name "second",
                  nameHsIdent = HsIdentifier
                    "diff_second"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "second",
              commentLocation = Just
                "flam.h:19:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["flam.h"],
                  headerInclude = "flam.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "flam.h:17:8",
            declId = NamePair {
              nameC = Name "diff",
              nameHsIdent = HsIdentifier
                "Diff"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["flam.h"],
                headerInclude = "flam.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Diff"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "flam.h:18:7",
                    fieldName = NamePair {
                      nameC = Name "first",
                      nameHsIdent = HsIdentifier
                        "diff_first"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "flam.h:19:7",
                    fieldName = NamePair {
                      nameC = Name "second",
                      nameHsIdent = HsIdentifier
                        "diff_second"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing}],
              structFlam = Just
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "flam.h:20:7",
                    fieldName = NamePair {
                      nameC = Name "flam",
                      nameHsIdent = HsIdentifier
                        "diff_flam"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 72,
                  structFieldWidth = Nothing}},
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
          commentOrigin = Just "diff",
          commentLocation = Just
            "flam.h:17:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["flam.h"],
              headerInclude = "flam.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Diff",
          structConstr = HsName
            "@NsConstr"
            "Diff",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "diff_first",
              fieldType = HsPrimType
                HsPrimCLong,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "flam.h:18:7",
                    fieldName = NamePair {
                      nameC = Name "first",
                      nameHsIdent = HsIdentifier
                        "diff_first"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "first",
                  commentLocation = Just
                    "flam.h:18:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["flam.h"],
                      headerInclude = "flam.h"},
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "diff_second",
              fieldType = HsPrimType
                HsPrimCChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "flam.h:19:7",
                    fieldName = NamePair {
                      nameC = Name "second",
                      nameHsIdent = HsIdentifier
                        "diff_second"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "second",
                  commentLocation = Just
                    "flam.h:19:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["flam.h"],
                      headerInclude = "flam.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "flam.h:17:8",
                declId = NamePair {
                  nameC = Name "diff",
                  nameHsIdent = HsIdentifier
                    "Diff"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["flam.h"],
                    headerInclude = "flam.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Diff"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "flam.h:18:7",
                        fieldName = NamePair {
                          nameC = Name "first",
                          nameHsIdent = HsIdentifier
                            "diff_first"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimLong Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "flam.h:19:7",
                        fieldName = NamePair {
                          nameC = Name "second",
                          nameHsIdent = HsIdentifier
                            "diff_second"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 64,
                      structFieldWidth = Nothing}],
                  structFlam = Just
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "flam.h:20:7",
                        fieldName = NamePair {
                          nameC = Name "flam",
                          nameHsIdent = HsIdentifier
                            "diff_flam"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 72,
                      structFieldWidth = Nothing}},
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
              commentOrigin = Just "diff",
              commentLocation = Just
                "flam.h:17:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["flam.h"],
                  headerInclude = "flam.h"},
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
                    "Diff",
                  structConstr = HsName
                    "@NsConstr"
                    "Diff",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "diff_first",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "flam.h:18:7",
                            fieldName = NamePair {
                              nameC = Name "first",
                              nameHsIdent = HsIdentifier
                                "diff_first"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "first",
                          commentLocation = Just
                            "flam.h:18:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["flam.h"],
                              headerInclude = "flam.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "diff_second",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "flam.h:19:7",
                            fieldName = NamePair {
                              nameC = Name "second",
                              nameHsIdent = HsIdentifier
                                "diff_second"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "second",
                          commentLocation = Just
                            "flam.h:19:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["flam.h"],
                              headerInclude = "flam.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "flam.h:17:8",
                        declId = NamePair {
                          nameC = Name "diff",
                          nameHsIdent = HsIdentifier
                            "Diff"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["flam.h"],
                            headerInclude = "flam.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Diff"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "flam.h:18:7",
                                fieldName = NamePair {
                                  nameC = Name "first",
                                  nameHsIdent = HsIdentifier
                                    "diff_first"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "flam.h:19:7",
                                fieldName = NamePair {
                                  nameC = Name "second",
                                  nameHsIdent = HsIdentifier
                                    "diff_second"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing}],
                          structFlam = Just
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "flam.h:20:7",
                                fieldName = NamePair {
                                  nameC = Name "flam",
                                  nameHsIdent = HsIdentifier
                                    "diff_flam"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 72,
                              structFieldWidth = Nothing}},
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
                      commentOrigin = Just "diff",
                      commentLocation = Just
                        "flam.h:17:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["flam.h"],
                          headerInclude = "flam.h"},
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
                  structName = HsName
                    "@NsTypeConstr"
                    "Diff",
                  structConstr = HsName
                    "@NsConstr"
                    "Diff",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "diff_first",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "flam.h:18:7",
                            fieldName = NamePair {
                              nameC = Name "first",
                              nameHsIdent = HsIdentifier
                                "diff_first"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "first",
                          commentLocation = Just
                            "flam.h:18:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["flam.h"],
                              headerInclude = "flam.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "diff_second",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "flam.h:19:7",
                            fieldName = NamePair {
                              nameC = Name "second",
                              nameHsIdent = HsIdentifier
                                "diff_second"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "second",
                          commentLocation = Just
                            "flam.h:19:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["flam.h"],
                              headerInclude = "flam.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "flam.h:17:8",
                        declId = NamePair {
                          nameC = Name "diff",
                          nameHsIdent = HsIdentifier
                            "Diff"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["flam.h"],
                            headerInclude = "flam.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Diff"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "flam.h:18:7",
                                fieldName = NamePair {
                                  nameC = Name "first",
                                  nameHsIdent = HsIdentifier
                                    "diff_first"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "flam.h:19:7",
                                fieldName = NamePair {
                                  nameC = Name "second",
                                  nameHsIdent = HsIdentifier
                                    "diff_second"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing}],
                          structFlam = Just
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "flam.h:20:7",
                                fieldName = NamePair {
                                  nameC = Name "flam",
                                  nameHsIdent = HsIdentifier
                                    "diff_flam"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 72,
                              structFieldWidth = Nothing}},
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
                      commentOrigin = Just "diff",
                      commentLocation = Just
                        "flam.h:17:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["flam.h"],
                          headerInclude = "flam.h"},
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Diff",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Diff",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasFLAM
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Diff",
          structConstr = HsName
            "@NsConstr"
            "Diff",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "diff_first",
              fieldType = HsPrimType
                HsPrimCLong,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "flam.h:18:7",
                    fieldName = NamePair {
                      nameC = Name "first",
                      nameHsIdent = HsIdentifier
                        "diff_first"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "first",
                  commentLocation = Just
                    "flam.h:18:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["flam.h"],
                      headerInclude = "flam.h"},
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "diff_second",
              fieldType = HsPrimType
                HsPrimCChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "flam.h:19:7",
                    fieldName = NamePair {
                      nameC = Name "second",
                      nameHsIdent = HsIdentifier
                        "diff_second"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "second",
                  commentLocation = Just
                    "flam.h:19:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["flam.h"],
                      headerInclude = "flam.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "flam.h:17:8",
                declId = NamePair {
                  nameC = Name "diff",
                  nameHsIdent = HsIdentifier
                    "Diff"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["flam.h"],
                    headerInclude = "flam.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Diff"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "flam.h:18:7",
                        fieldName = NamePair {
                          nameC = Name "first",
                          nameHsIdent = HsIdentifier
                            "diff_first"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimLong Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "flam.h:19:7",
                        fieldName = NamePair {
                          nameC = Name "second",
                          nameHsIdent = HsIdentifier
                            "diff_second"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 64,
                      structFieldWidth = Nothing}],
                  structFlam = Just
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "flam.h:20:7",
                        fieldName = NamePair {
                          nameC = Name "flam",
                          nameHsIdent = HsIdentifier
                            "diff_flam"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 72,
                      structFieldWidth = Nothing}},
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
              commentOrigin = Just "diff",
              commentLocation = Just
                "flam.h:17:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["flam.h"],
                  headerInclude = "flam.h"},
              commentChildren = []}}
        (HsPrimType HsPrimCChar)
        9,
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Triplets",
      structConstr = HsName
        "@NsConstr"
        "Triplets",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "triplets_len",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "flam.h:27:7",
                fieldName = NamePair {
                  nameC = Name "len",
                  nameHsIdent = HsIdentifier
                    "triplets_len"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "len",
              commentLocation = Just
                "flam.h:27:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["flam.h"],
                  headerInclude = "flam.h"},
              commentChildren = []}}],
      structOrigin =
      Just
        Decl {
          declInfo =
          DeclInfo {
            declLoc = "flam.h:26:8",
            declId = NamePair {
              nameC = Name "triplets",
              nameHsIdent = HsIdentifier
                "Triplets"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["flam.h"],
                headerInclude = "flam.h"},
            declComment =
            Just
              (Comment
                [
                  Paragraph
                    [
                      TextContent
                        "The flexible array member is a multi-dimensional array of unknown size. In",
                      TextContent
                        "particular, it is a is an array of unknown size, where each element is of",
                      TextContent
                        "type length-3-array-of-int."]])},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Triplets"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "flam.h:27:7",
                    fieldName = NamePair {
                      nameC = Name "len",
                      nameHsIdent = HsIdentifier
                        "triplets_len"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing}],
              structFlam = Just
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "flam.h:28:7",
                    fieldName = NamePair {
                      nameC = Name "flam",
                      nameHsIdent = HsIdentifier
                        "triplets_flam"},
                    fieldComment = Nothing},
                  structFieldType = TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral PrimInt Signed)),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing}},
          declSpec = DeclSpec
            TypeSpec {
              typeSpecModule = Nothing,
              typeSpecIdentifier = Nothing,
              typeSpecInstances = Map.fromList
                []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment =
      Just
        Comment {
          commentTitle =
          Just
            [
              TextContent
                "The flexible array member is a multi-dimensional array of unknown size. In",
              TextContent
                "particular, it is a is an array of unknown size, where each element is of",
              TextContent
                "type length-3-array-of-int."],
          commentOrigin = Just "triplets",
          commentLocation = Just
            "flam.h:26:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["flam.h"],
              headerInclude = "flam.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Triplets",
          structConstr = HsName
            "@NsConstr"
            "Triplets",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "triplets_len",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "flam.h:27:7",
                    fieldName = NamePair {
                      nameC = Name "len",
                      nameHsIdent = HsIdentifier
                        "triplets_len"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "len",
                  commentLocation = Just
                    "flam.h:27:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["flam.h"],
                      headerInclude = "flam.h"},
                  commentChildren = []}}],
          structOrigin =
          Just
            Decl {
              declInfo =
              DeclInfo {
                declLoc = "flam.h:26:8",
                declId = NamePair {
                  nameC = Name "triplets",
                  nameHsIdent = HsIdentifier
                    "Triplets"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["flam.h"],
                    headerInclude = "flam.h"},
                declComment =
                Just
                  (Comment
                    [
                      Paragraph
                        [
                          TextContent
                            "The flexible array member is a multi-dimensional array of unknown size. In",
                          TextContent
                            "particular, it is a is an array of unknown size, where each element is of",
                          TextContent
                            "type length-3-array-of-int."]])},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Triplets"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "flam.h:27:7",
                        fieldName = NamePair {
                          nameC = Name "len",
                          nameHsIdent = HsIdentifier
                            "triplets_len"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Just
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "flam.h:28:7",
                        fieldName = NamePair {
                          nameC = Name "flam",
                          nameHsIdent = HsIdentifier
                            "triplets_flam"},
                        fieldComment = Nothing},
                      structFieldType = TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral PrimInt Signed)),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing}},
              declSpec = DeclSpec
                TypeSpec {
                  typeSpecModule = Nothing,
                  typeSpecIdentifier = Nothing,
                  typeSpecInstances = Map.fromList
                    []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment =
          Just
            Comment {
              commentTitle =
              Just
                [
                  TextContent
                    "The flexible array member is a multi-dimensional array of unknown size. In",
                  TextContent
                    "particular, it is a is an array of unknown size, where each element is of",
                  TextContent
                    "type length-3-array-of-int."],
              commentOrigin = Just "triplets",
              commentLocation = Just
                "flam.h:26:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["flam.h"],
                  headerInclude = "flam.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 4,
          storableAlignment = 4,
          storablePeek =
          Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Triplets",
                  structConstr = HsName
                    "@NsConstr"
                    "Triplets",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "triplets_len",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "flam.h:27:7",
                            fieldName = NamePair {
                              nameC = Name "len",
                              nameHsIdent = HsIdentifier
                                "triplets_len"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "len",
                          commentLocation = Just
                            "flam.h:27:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["flam.h"],
                              headerInclude = "flam.h"},
                          commentChildren = []}}],
                  structOrigin =
                  Just
                    Decl {
                      declInfo =
                      DeclInfo {
                        declLoc = "flam.h:26:8",
                        declId = NamePair {
                          nameC = Name "triplets",
                          nameHsIdent = HsIdentifier
                            "Triplets"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["flam.h"],
                            headerInclude = "flam.h"},
                        declComment =
                        Just
                          (Comment
                            [
                              Paragraph
                                [
                                  TextContent
                                    "The flexible array member is a multi-dimensional array of unknown size. In",
                                  TextContent
                                    "particular, it is a is an array of unknown size, where each element is of",
                                  TextContent
                                    "type length-3-array-of-int."]])},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Triplets"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "flam.h:27:7",
                                fieldName = NamePair {
                                  nameC = Name "len",
                                  nameHsIdent = HsIdentifier
                                    "triplets_len"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Just
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "flam.h:28:7",
                                fieldName = NamePair {
                                  nameC = Name "flam",
                                  nameHsIdent = HsIdentifier
                                    "triplets_flam"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                3
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing}},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment =
                  Just
                    Comment {
                      commentTitle =
                      Just
                        [
                          TextContent
                            "The flexible array member is a multi-dimensional array of unknown size. In",
                          TextContent
                            "particular, it is a is an array of unknown size, where each element is of",
                          TextContent
                            "type length-3-array-of-int."],
                      commentOrigin = Just "triplets",
                      commentLocation = Just
                        "flam.h:26:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["flam.h"],
                          headerInclude = "flam.h"},
                      commentChildren = []}})
              [PeekByteOff (Idx 0) 0]),
          storablePoke =
          Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Triplets",
                  structConstr = HsName
                    "@NsConstr"
                    "Triplets",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "triplets_len",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "flam.h:27:7",
                            fieldName = NamePair {
                              nameC = Name "len",
                              nameHsIdent = HsIdentifier
                                "triplets_len"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "len",
                          commentLocation = Just
                            "flam.h:27:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["flam.h"],
                              headerInclude = "flam.h"},
                          commentChildren = []}}],
                  structOrigin =
                  Just
                    Decl {
                      declInfo =
                      DeclInfo {
                        declLoc = "flam.h:26:8",
                        declId = NamePair {
                          nameC = Name "triplets",
                          nameHsIdent = HsIdentifier
                            "Triplets"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["flam.h"],
                            headerInclude = "flam.h"},
                        declComment =
                        Just
                          (Comment
                            [
                              Paragraph
                                [
                                  TextContent
                                    "The flexible array member is a multi-dimensional array of unknown size. In",
                                  TextContent
                                    "particular, it is a is an array of unknown size, where each element is of",
                                  TextContent
                                    "type length-3-array-of-int."]])},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Triplets"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "flam.h:27:7",
                                fieldName = NamePair {
                                  nameC = Name "len",
                                  nameHsIdent = HsIdentifier
                                    "triplets_len"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Just
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "flam.h:28:7",
                                fieldName = NamePair {
                                  nameC = Name "flam",
                                  nameHsIdent = HsIdentifier
                                    "triplets_flam"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                3
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing}},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment =
                  Just
                    Comment {
                      commentTitle =
                      Just
                        [
                          TextContent
                            "The flexible array member is a multi-dimensional array of unknown size. In",
                          TextContent
                            "particular, it is a is an array of unknown size, where each element is of",
                          TextContent
                            "type length-3-array-of-int."],
                      commentOrigin = Just "triplets",
                      commentLocation = Just
                        "flam.h:26:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["flam.h"],
                          headerInclude = "flam.h"},
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
        "Triplets",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Triplets",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasFLAM
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Triplets",
          structConstr = HsName
            "@NsConstr"
            "Triplets",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "triplets_len",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "flam.h:27:7",
                    fieldName = NamePair {
                      nameC = Name "len",
                      nameHsIdent = HsIdentifier
                        "triplets_len"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "len",
                  commentLocation = Just
                    "flam.h:27:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["flam.h"],
                      headerInclude = "flam.h"},
                  commentChildren = []}}],
          structOrigin =
          Just
            Decl {
              declInfo =
              DeclInfo {
                declLoc = "flam.h:26:8",
                declId = NamePair {
                  nameC = Name "triplets",
                  nameHsIdent = HsIdentifier
                    "Triplets"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["flam.h"],
                    headerInclude = "flam.h"},
                declComment =
                Just
                  (Comment
                    [
                      Paragraph
                        [
                          TextContent
                            "The flexible array member is a multi-dimensional array of unknown size. In",
                          TextContent
                            "particular, it is a is an array of unknown size, where each element is of",
                          TextContent
                            "type length-3-array-of-int."]])},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Triplets"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "flam.h:27:7",
                        fieldName = NamePair {
                          nameC = Name "len",
                          nameHsIdent = HsIdentifier
                            "triplets_len"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Just
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "flam.h:28:7",
                        fieldName = NamePair {
                          nameC = Name "flam",
                          nameHsIdent = HsIdentifier
                            "triplets_flam"},
                        fieldComment = Nothing},
                      structFieldType = TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral PrimInt Signed)),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing}},
              declSpec = DeclSpec
                TypeSpec {
                  typeSpecModule = Nothing,
                  typeSpecIdentifier = Nothing,
                  typeSpecInstances = Map.fromList
                    []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment =
          Just
            Comment {
              commentTitle =
              Just
                [
                  TextContent
                    "The flexible array member is a multi-dimensional array of unknown size. In",
                  TextContent
                    "particular, it is a is an array of unknown size, where each element is of",
                  TextContent
                    "type length-3-array-of-int."],
              commentOrigin = Just "triplets",
              commentLocation = Just
                "flam.h:26:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["flam.h"],
                  headerInclude = "flam.h"},
              commentChildren = []}}
        (HsConstArray
          3
          (HsPrimType HsPrimCInt))
        4,
      defineInstanceComment =
      Nothing}]
