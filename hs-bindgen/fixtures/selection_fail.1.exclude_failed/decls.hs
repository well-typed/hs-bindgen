[
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "OkBefore",
      structConstr = Name
        "@NsConstr"
        "OkBefore",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "okBefore_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "selection_fail.h:2:7",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "okBefore_x"},
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
                "selection_fail.h:2:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["selection_fail.h"],
                  headerInclude =
                  "selection_fail.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "selection_fail.h:1:8",
            declId = NamePair {
              nameC = Name "OkBefore",
              nameHsIdent = Identifier
                "OkBefore"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["selection_fail.h"],
                headerInclude =
                "selection_fail.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "OkBefore"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "selection_fail.h:2:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "okBefore_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec {
            declSpecC = Nothing,
            declSpecHs = Nothing}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "OkBefore",
          commentLocation = Just
            "selection_fail.h:1:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["selection_fail.h"],
              headerInclude =
              "selection_fail.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "OkBefore",
          structConstr = Name
            "@NsConstr"
            "OkBefore",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "okBefore_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "selection_fail.h:2:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "okBefore_x"},
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
                    "selection_fail.h:2:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["selection_fail.h"],
                      headerInclude =
                      "selection_fail.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "selection_fail.h:1:8",
                declId = NamePair {
                  nameC = Name "OkBefore",
                  nameHsIdent = Identifier
                    "OkBefore"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["selection_fail.h"],
                    headerInclude =
                    "selection_fail.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "OkBefore"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "selection_fail.h:2:7",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "okBefore_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec {
                declSpecC = Nothing,
                declSpecHs = Nothing}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "OkBefore",
              commentLocation = Just
                "selection_fail.h:1:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["selection_fail.h"],
                  headerInclude =
                  "selection_fail.h"},
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
                    "OkBefore",
                  structConstr = Name
                    "@NsConstr"
                    "OkBefore",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "okBefore_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "selection_fail.h:2:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "okBefore_x"},
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
                            "selection_fail.h:2:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["selection_fail.h"],
                              headerInclude =
                              "selection_fail.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "selection_fail.h:1:8",
                        declId = NamePair {
                          nameC = Name "OkBefore",
                          nameHsIdent = Identifier
                            "OkBefore"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["selection_fail.h"],
                            headerInclude =
                            "selection_fail.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "OkBefore"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "selection_fail.h:2:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "okBefore_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "OkBefore",
                      commentLocation = Just
                        "selection_fail.h:1:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["selection_fail.h"],
                          headerInclude =
                          "selection_fail.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "okBefore_x")
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
                    "OkBefore",
                  structConstr = Name
                    "@NsConstr"
                    "OkBefore",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "okBefore_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "selection_fail.h:2:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "okBefore_x"},
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
                            "selection_fail.h:2:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["selection_fail.h"],
                              headerInclude =
                              "selection_fail.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "selection_fail.h:1:8",
                        declId = NamePair {
                          nameC = Name "OkBefore",
                          nameHsIdent = Identifier
                            "OkBefore"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["selection_fail.h"],
                            headerInclude =
                            "selection_fail.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "OkBefore"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "selection_fail.h:2:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "okBefore_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "OkBefore",
                      commentLocation = Just
                        "selection_fail.h:1:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["selection_fail.h"],
                          headerInclude =
                          "selection_fail.h"},
                      commentChildren = []}}
                (Add 1)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "okBefore_x")
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
        "OkBefore",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "OkBefore",
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
              "OkBefore"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "okBefore_x",
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
            (Name
              "@NsTypeConstr"
              "OkBefore"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "okBefore_x",
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
        "OkAfter",
      structConstr = Name
        "@NsConstr"
        "OkAfter",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "okAfter_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "selection_fail.h:27:7",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "okAfter_x"},
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
                "selection_fail.h:27:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["selection_fail.h"],
                  headerInclude =
                  "selection_fail.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "selection_fail.h:26:8",
            declId = NamePair {
              nameC = Name "OkAfter",
              nameHsIdent = Identifier
                "OkAfter"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["selection_fail.h"],
                headerInclude =
                "selection_fail.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "OkAfter"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "selection_fail.h:27:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "okAfter_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec {
            declSpecC = Nothing,
            declSpecHs = Nothing}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "OkAfter",
          commentLocation = Just
            "selection_fail.h:26:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["selection_fail.h"],
              headerInclude =
              "selection_fail.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "OkAfter",
          structConstr = Name
            "@NsConstr"
            "OkAfter",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "okAfter_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "selection_fail.h:27:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "okAfter_x"},
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
                    "selection_fail.h:27:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["selection_fail.h"],
                      headerInclude =
                      "selection_fail.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "selection_fail.h:26:8",
                declId = NamePair {
                  nameC = Name "OkAfter",
                  nameHsIdent = Identifier
                    "OkAfter"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["selection_fail.h"],
                    headerInclude =
                    "selection_fail.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "OkAfter"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "selection_fail.h:27:7",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "okAfter_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec {
                declSpecC = Nothing,
                declSpecHs = Nothing}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "OkAfter",
              commentLocation = Just
                "selection_fail.h:26:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["selection_fail.h"],
                  headerInclude =
                  "selection_fail.h"},
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
                    "OkAfter",
                  structConstr = Name
                    "@NsConstr"
                    "OkAfter",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "okAfter_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "selection_fail.h:27:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "okAfter_x"},
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
                            "selection_fail.h:27:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["selection_fail.h"],
                              headerInclude =
                              "selection_fail.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "selection_fail.h:26:8",
                        declId = NamePair {
                          nameC = Name "OkAfter",
                          nameHsIdent = Identifier
                            "OkAfter"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["selection_fail.h"],
                            headerInclude =
                            "selection_fail.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "OkAfter"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "selection_fail.h:27:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "okAfter_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "OkAfter",
                      commentLocation = Just
                        "selection_fail.h:26:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["selection_fail.h"],
                          headerInclude =
                          "selection_fail.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "okAfter_x")
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
                    "OkAfter",
                  structConstr = Name
                    "@NsConstr"
                    "OkAfter",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "okAfter_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "selection_fail.h:27:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "okAfter_x"},
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
                            "selection_fail.h:27:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["selection_fail.h"],
                              headerInclude =
                              "selection_fail.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "selection_fail.h:26:8",
                        declId = NamePair {
                          nameC = Name "OkAfter",
                          nameHsIdent = Identifier
                            "OkAfter"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["selection_fail.h"],
                            headerInclude =
                            "selection_fail.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "OkAfter"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "selection_fail.h:27:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "okAfter_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "OkAfter",
                      commentLocation = Just
                        "selection_fail.h:26:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["selection_fail.h"],
                          headerInclude =
                          "selection_fail.h"},
                      commentChildren = []}}
                (Add 1)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "okAfter_x")
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
        "OkAfter",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "OkAfter",
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
              "OkAfter"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "okAfter_x",
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
            (Name
              "@NsTypeConstr"
              "OkAfter"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "okAfter_x",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing}]
