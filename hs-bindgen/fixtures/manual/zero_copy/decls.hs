[
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Point",
      structConstr = Name
        "@NsConstr"
        "Point",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "point_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "zero_copy.h:13:7",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "point_x"},
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
                "zero_copy.h:13:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "point_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "zero_copy.h:14:7",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier
                    "point_y"},
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
                "zero_copy.h:14:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "zero_copy.h:12:8",
            declId = NamePair {
              nameC = Name "point",
              nameHsIdent = Identifier
                "Point"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["manual/zero_copy.h"],
                headerInclude =
                "manual/zero_copy.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Point"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:13:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "point_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:14:7",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "point_y"},
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
          commentOrigin = Just "point",
          commentLocation = Just
            "zero_copy.h:12:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/zero_copy.h"],
              headerInclude =
              "manual/zero_copy.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Point",
          structConstr = Name
            "@NsConstr"
            "Point",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "point_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:13:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "point_x"},
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
                    "zero_copy.h:13:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["manual/zero_copy.h"],
                      headerInclude =
                      "manual/zero_copy.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "point_y",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:14:7",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "point_y"},
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
                    "zero_copy.h:14:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["manual/zero_copy.h"],
                      headerInclude =
                      "manual/zero_copy.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "zero_copy.h:12:8",
                declId = NamePair {
                  nameC = Name "point",
                  nameHsIdent = Identifier
                    "Point"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["manual/zero_copy.h"],
                    headerInclude =
                    "manual/zero_copy.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Point"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "zero_copy.h:13:7",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "point_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "zero_copy.h:14:7",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = Identifier
                            "point_y"},
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
              commentOrigin = Just "point",
              commentLocation = Just
                "zero_copy.h:12:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
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
                    "Point",
                  structConstr = Name
                    "@NsConstr"
                    "Point",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "point_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:13:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "point_x"},
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
                            "zero_copy.h:13:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "point_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:14:7",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "point_y"},
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
                            "zero_copy.h:14:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "zero_copy.h:12:8",
                        declId = NamePair {
                          nameC = Name "point",
                          nameHsIdent = Identifier
                            "Point"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["manual/zero_copy.h"],
                            headerInclude =
                            "manual/zero_copy.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Point"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:13:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "point_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:14:7",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "point_y"},
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
                      commentOrigin = Just "point",
                      commentLocation = Just
                        "zero_copy.h:12:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["manual/zero_copy.h"],
                          headerInclude =
                          "manual/zero_copy.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "point_x")
                  (Idx 0),
                PeekCField
                  (HsStrLit "point_y")
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
                    "Point",
                  structConstr = Name
                    "@NsConstr"
                    "Point",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "point_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:13:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "point_x"},
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
                            "zero_copy.h:13:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "point_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:14:7",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "point_y"},
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
                            "zero_copy.h:14:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "zero_copy.h:12:8",
                        declId = NamePair {
                          nameC = Name "point",
                          nameHsIdent = Identifier
                            "Point"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["manual/zero_copy.h"],
                            headerInclude =
                            "manual/zero_copy.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Point"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:13:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "point_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:14:7",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "point_y"},
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
                      commentOrigin = Just "point",
                      commentLocation = Just
                        "zero_copy.h:12:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["manual/zero_copy.h"],
                          headerInclude =
                          "manual/zero_copy.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "point_x")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "point_y")
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
        "Point",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Point",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Point"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "point_x",
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
            (Name "@NsTypeConstr" "Point"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "point_x",
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
            (Name "@NsTypeConstr" "Point"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "point_y",
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
            (Name "@NsTypeConstr" "Point"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "point_y",
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
        "Rectangle",
      structConstr = Name
        "@NsConstr"
        "Rectangle",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "rectangle_topleft",
          fieldType = HsTypRef
            (Name "@NsTypeConstr" "Point"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "zero_copy.h:18:16",
                fieldName = NamePair {
                  nameC = Name "topleft",
                  nameHsIdent = Identifier
                    "rectangle_topleft"},
                fieldComment = Nothing},
              structFieldType = TypeStruct
                NamePair {
                  nameC = Name "point",
                  nameHsIdent = Identifier
                    "Point"}
                NameOriginInSource,
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "topleft",
              commentLocation = Just
                "zero_copy.h:18:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "rectangle_bottomright",
          fieldType = HsTypRef
            (Name "@NsTypeConstr" "Point"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "zero_copy.h:19:16",
                fieldName = NamePair {
                  nameC = Name "bottomright",
                  nameHsIdent = Identifier
                    "rectangle_bottomright"},
                fieldComment = Nothing},
              structFieldType = TypeStruct
                NamePair {
                  nameC = Name "point",
                  nameHsIdent = Identifier
                    "Point"}
                NameOriginInSource,
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "bottomright",
              commentLocation = Just
                "zero_copy.h:19:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "zero_copy.h:17:8",
            declId = NamePair {
              nameC = Name "rectangle",
              nameHsIdent = Identifier
                "Rectangle"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["manual/zero_copy.h"],
                headerInclude =
                "manual/zero_copy.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Rectangle"),
              structSizeof = 16,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:18:16",
                    fieldName = NamePair {
                      nameC = Name "topleft",
                      nameHsIdent = Identifier
                        "rectangle_topleft"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "point",
                      nameHsIdent = Identifier
                        "Point"}
                    NameOriginInSource,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:19:16",
                    fieldName = NamePair {
                      nameC = Name "bottomright",
                      nameHsIdent = Identifier
                        "rectangle_bottomright"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "point",
                      nameHsIdent = Identifier
                        "Point"}
                    NameOriginInSource,
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
          commentOrigin = Just
            "rectangle",
          commentLocation = Just
            "zero_copy.h:17:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/zero_copy.h"],
              headerInclude =
              "manual/zero_copy.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Rectangle",
          structConstr = Name
            "@NsConstr"
            "Rectangle",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "rectangle_topleft",
              fieldType = HsTypRef
                (Name "@NsTypeConstr" "Point"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:18:16",
                    fieldName = NamePair {
                      nameC = Name "topleft",
                      nameHsIdent = Identifier
                        "rectangle_topleft"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "point",
                      nameHsIdent = Identifier
                        "Point"}
                    NameOriginInSource,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "topleft",
                  commentLocation = Just
                    "zero_copy.h:18:16",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["manual/zero_copy.h"],
                      headerInclude =
                      "manual/zero_copy.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "rectangle_bottomright",
              fieldType = HsTypRef
                (Name "@NsTypeConstr" "Point"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:19:16",
                    fieldName = NamePair {
                      nameC = Name "bottomright",
                      nameHsIdent = Identifier
                        "rectangle_bottomright"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "point",
                      nameHsIdent = Identifier
                        "Point"}
                    NameOriginInSource,
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just
                    "bottomright",
                  commentLocation = Just
                    "zero_copy.h:19:16",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["manual/zero_copy.h"],
                      headerInclude =
                      "manual/zero_copy.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "zero_copy.h:17:8",
                declId = NamePair {
                  nameC = Name "rectangle",
                  nameHsIdent = Identifier
                    "Rectangle"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["manual/zero_copy.h"],
                    headerInclude =
                    "manual/zero_copy.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Rectangle"),
                  structSizeof = 16,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "zero_copy.h:18:16",
                        fieldName = NamePair {
                          nameC = Name "topleft",
                          nameHsIdent = Identifier
                            "rectangle_topleft"},
                        fieldComment = Nothing},
                      structFieldType = TypeStruct
                        NamePair {
                          nameC = Name "point",
                          nameHsIdent = Identifier
                            "Point"}
                        NameOriginInSource,
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "zero_copy.h:19:16",
                        fieldName = NamePair {
                          nameC = Name "bottomright",
                          nameHsIdent = Identifier
                            "rectangle_bottomright"},
                        fieldComment = Nothing},
                      structFieldType = TypeStruct
                        NamePair {
                          nameC = Name "point",
                          nameHsIdent = Identifier
                            "Point"}
                        NameOriginInSource,
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
              commentOrigin = Just
                "rectangle",
              commentLocation = Just
                "zero_copy.h:17:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 16,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Rectangle",
                  structConstr = Name
                    "@NsConstr"
                    "Rectangle",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "rectangle_topleft",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "Point"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:18:16",
                            fieldName = NamePair {
                              nameC = Name "topleft",
                              nameHsIdent = Identifier
                                "rectangle_topleft"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "point",
                              nameHsIdent = Identifier
                                "Point"}
                            NameOriginInSource,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "topleft",
                          commentLocation = Just
                            "zero_copy.h:18:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "rectangle_bottomright",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "Point"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:19:16",
                            fieldName = NamePair {
                              nameC = Name "bottomright",
                              nameHsIdent = Identifier
                                "rectangle_bottomright"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "point",
                              nameHsIdent = Identifier
                                "Point"}
                            NameOriginInSource,
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "bottomright",
                          commentLocation = Just
                            "zero_copy.h:19:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "zero_copy.h:17:8",
                        declId = NamePair {
                          nameC = Name "rectangle",
                          nameHsIdent = Identifier
                            "Rectangle"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["manual/zero_copy.h"],
                            headerInclude =
                            "manual/zero_copy.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Rectangle"),
                          structSizeof = 16,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:18:16",
                                fieldName = NamePair {
                                  nameC = Name "topleft",
                                  nameHsIdent = Identifier
                                    "rectangle_topleft"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "point",
                                  nameHsIdent = Identifier
                                    "Point"}
                                NameOriginInSource,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:19:16",
                                fieldName = NamePair {
                                  nameC = Name "bottomright",
                                  nameHsIdent = Identifier
                                    "rectangle_bottomright"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "point",
                                  nameHsIdent = Identifier
                                    "Point"}
                                NameOriginInSource,
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
                      commentOrigin = Just
                        "rectangle",
                      commentLocation = Just
                        "zero_copy.h:17:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["manual/zero_copy.h"],
                          headerInclude =
                          "manual/zero_copy.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "rectangle_topleft")
                  (Idx 0),
                PeekCField
                  (HsStrLit
                    "rectangle_bottomright")
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
                    "Rectangle",
                  structConstr = Name
                    "@NsConstr"
                    "Rectangle",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "rectangle_topleft",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "Point"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:18:16",
                            fieldName = NamePair {
                              nameC = Name "topleft",
                              nameHsIdent = Identifier
                                "rectangle_topleft"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "point",
                              nameHsIdent = Identifier
                                "Point"}
                            NameOriginInSource,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "topleft",
                          commentLocation = Just
                            "zero_copy.h:18:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "rectangle_bottomright",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "Point"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:19:16",
                            fieldName = NamePair {
                              nameC = Name "bottomright",
                              nameHsIdent = Identifier
                                "rectangle_bottomright"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "point",
                              nameHsIdent = Identifier
                                "Point"}
                            NameOriginInSource,
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "bottomright",
                          commentLocation = Just
                            "zero_copy.h:19:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "zero_copy.h:17:8",
                        declId = NamePair {
                          nameC = Name "rectangle",
                          nameHsIdent = Identifier
                            "Rectangle"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["manual/zero_copy.h"],
                            headerInclude =
                            "manual/zero_copy.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Rectangle"),
                          structSizeof = 16,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:18:16",
                                fieldName = NamePair {
                                  nameC = Name "topleft",
                                  nameHsIdent = Identifier
                                    "rectangle_topleft"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "point",
                                  nameHsIdent = Identifier
                                    "Point"}
                                NameOriginInSource,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:19:16",
                                fieldName = NamePair {
                                  nameC = Name "bottomright",
                                  nameHsIdent = Identifier
                                    "rectangle_bottomright"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "point",
                                  nameHsIdent = Identifier
                                    "Point"}
                                NameOriginInSource,
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
                      commentOrigin = Just
                        "rectangle",
                      commentLocation = Just
                        "zero_copy.h:17:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["manual/zero_copy.h"],
                          headerInclude =
                          "manual/zero_copy.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "rectangle_topleft")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit
                        "rectangle_bottomright")
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
        "Rectangle",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Rectangle",
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
              "Rectangle"),
          hasCFieldInstanceFieldName =
          Name
            "@NsVar"
            "rectangle_topleft",
          hasCFieldInstanceCFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "Point"),
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
              "Rectangle"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "rectangle_topleft",
          hasFieldInstanceFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "Point"),
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
              "Rectangle"),
          hasCFieldInstanceFieldName =
          Name
            "@NsVar"
            "rectangle_bottomright",
          hasCFieldInstanceCFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "Point"),
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
            (Name
              "@NsTypeConstr"
              "Rectangle"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "rectangle_bottomright",
          hasFieldInstanceFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "Point"),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Circle",
      structConstr = Name
        "@NsConstr"
        "Circle",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "circle_midpoint",
          fieldType = HsTypRef
            (Name "@NsTypeConstr" "Point"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "zero_copy.h:23:16",
                fieldName = NamePair {
                  nameC = Name "midpoint",
                  nameHsIdent = Identifier
                    "circle_midpoint"},
                fieldComment = Nothing},
              structFieldType = TypeStruct
                NamePair {
                  nameC = Name "point",
                  nameHsIdent = Identifier
                    "Point"}
                NameOriginInSource,
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "midpoint",
              commentLocation = Just
                "zero_copy.h:23:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "circle_radius",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "zero_copy.h:24:7",
                fieldName = NamePair {
                  nameC = Name "radius",
                  nameHsIdent = Identifier
                    "circle_radius"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "radius",
              commentLocation = Just
                "zero_copy.h:24:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "zero_copy.h:22:8",
            declId = NamePair {
              nameC = Name "circle",
              nameHsIdent = Identifier
                "Circle"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["manual/zero_copy.h"],
                headerInclude =
                "manual/zero_copy.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Circle"),
              structSizeof = 12,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:23:16",
                    fieldName = NamePair {
                      nameC = Name "midpoint",
                      nameHsIdent = Identifier
                        "circle_midpoint"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "point",
                      nameHsIdent = Identifier
                        "Point"}
                    NameOriginInSource,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:24:7",
                    fieldName = NamePair {
                      nameC = Name "radius",
                      nameHsIdent = Identifier
                        "circle_radius"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
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
          commentOrigin = Just "circle",
          commentLocation = Just
            "zero_copy.h:22:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/zero_copy.h"],
              headerInclude =
              "manual/zero_copy.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Circle",
          structConstr = Name
            "@NsConstr"
            "Circle",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "circle_midpoint",
              fieldType = HsTypRef
                (Name "@NsTypeConstr" "Point"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:23:16",
                    fieldName = NamePair {
                      nameC = Name "midpoint",
                      nameHsIdent = Identifier
                        "circle_midpoint"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "point",
                      nameHsIdent = Identifier
                        "Point"}
                    NameOriginInSource,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "midpoint",
                  commentLocation = Just
                    "zero_copy.h:23:16",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["manual/zero_copy.h"],
                      headerInclude =
                      "manual/zero_copy.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "circle_radius",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:24:7",
                    fieldName = NamePair {
                      nameC = Name "radius",
                      nameHsIdent = Identifier
                        "circle_radius"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "radius",
                  commentLocation = Just
                    "zero_copy.h:24:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["manual/zero_copy.h"],
                      headerInclude =
                      "manual/zero_copy.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "zero_copy.h:22:8",
                declId = NamePair {
                  nameC = Name "circle",
                  nameHsIdent = Identifier
                    "Circle"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["manual/zero_copy.h"],
                    headerInclude =
                    "manual/zero_copy.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Circle"),
                  structSizeof = 12,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "zero_copy.h:23:16",
                        fieldName = NamePair {
                          nameC = Name "midpoint",
                          nameHsIdent = Identifier
                            "circle_midpoint"},
                        fieldComment = Nothing},
                      structFieldType = TypeStruct
                        NamePair {
                          nameC = Name "point",
                          nameHsIdent = Identifier
                            "Point"}
                        NameOriginInSource,
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "zero_copy.h:24:7",
                        fieldName = NamePair {
                          nameC = Name "radius",
                          nameHsIdent = Identifier
                            "circle_radius"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
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
              commentOrigin = Just "circle",
              commentLocation = Just
                "zero_copy.h:22:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
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
                    "Circle",
                  structConstr = Name
                    "@NsConstr"
                    "Circle",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "circle_midpoint",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "Point"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:23:16",
                            fieldName = NamePair {
                              nameC = Name "midpoint",
                              nameHsIdent = Identifier
                                "circle_midpoint"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "point",
                              nameHsIdent = Identifier
                                "Point"}
                            NameOriginInSource,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "midpoint",
                          commentLocation = Just
                            "zero_copy.h:23:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "circle_radius",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:24:7",
                            fieldName = NamePair {
                              nameC = Name "radius",
                              nameHsIdent = Identifier
                                "circle_radius"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "radius",
                          commentLocation = Just
                            "zero_copy.h:24:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "zero_copy.h:22:8",
                        declId = NamePair {
                          nameC = Name "circle",
                          nameHsIdent = Identifier
                            "Circle"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["manual/zero_copy.h"],
                            headerInclude =
                            "manual/zero_copy.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Circle"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:23:16",
                                fieldName = NamePair {
                                  nameC = Name "midpoint",
                                  nameHsIdent = Identifier
                                    "circle_midpoint"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "point",
                                  nameHsIdent = Identifier
                                    "Point"}
                                NameOriginInSource,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:24:7",
                                fieldName = NamePair {
                                  nameC = Name "radius",
                                  nameHsIdent = Identifier
                                    "circle_radius"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
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
                      commentOrigin = Just "circle",
                      commentLocation = Just
                        "zero_copy.h:22:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["manual/zero_copy.h"],
                          headerInclude =
                          "manual/zero_copy.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "circle_midpoint")
                  (Idx 0),
                PeekCField
                  (HsStrLit "circle_radius")
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
                    "Circle",
                  structConstr = Name
                    "@NsConstr"
                    "Circle",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "circle_midpoint",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "Point"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:23:16",
                            fieldName = NamePair {
                              nameC = Name "midpoint",
                              nameHsIdent = Identifier
                                "circle_midpoint"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "point",
                              nameHsIdent = Identifier
                                "Point"}
                            NameOriginInSource,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "midpoint",
                          commentLocation = Just
                            "zero_copy.h:23:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "circle_radius",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:24:7",
                            fieldName = NamePair {
                              nameC = Name "radius",
                              nameHsIdent = Identifier
                                "circle_radius"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "radius",
                          commentLocation = Just
                            "zero_copy.h:24:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "zero_copy.h:22:8",
                        declId = NamePair {
                          nameC = Name "circle",
                          nameHsIdent = Identifier
                            "Circle"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["manual/zero_copy.h"],
                            headerInclude =
                            "manual/zero_copy.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Circle"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:23:16",
                                fieldName = NamePair {
                                  nameC = Name "midpoint",
                                  nameHsIdent = Identifier
                                    "circle_midpoint"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "point",
                                  nameHsIdent = Identifier
                                    "Point"}
                                NameOriginInSource,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:24:7",
                                fieldName = NamePair {
                                  nameC = Name "radius",
                                  nameHsIdent = Identifier
                                    "circle_radius"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
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
                      commentOrigin = Just "circle",
                      commentLocation = Just
                        "zero_copy.h:22:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["manual/zero_copy.h"],
                          headerInclude =
                          "manual/zero_copy.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "circle_midpoint")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "circle_radius")
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
        "Circle",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Circle",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Circle"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "circle_midpoint",
          hasCFieldInstanceCFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "Point"),
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
            (Name "@NsTypeConstr" "Circle"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "circle_midpoint",
          hasFieldInstanceFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "Point"),
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
            (Name "@NsTypeConstr" "Circle"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "circle_radius",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCInt,
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
            (Name "@NsTypeConstr" "Circle"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "circle_radius",
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
        "Shape",
      newtypeConstr = Name
        "@NsConstr"
        "Shape",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Shape",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "zero_copy.h:30:7",
          declId = NamePair {
            nameC = Name "shape",
            nameHsIdent = Identifier
              "Shape"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/zero_copy.h"],
              headerInclude =
              "manual/zero_copy.h"},
          declComment = Nothing},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Shape",
              newtypeField = Name
                "@NsVar"
                "un_Shape"},
            unionSizeof = 16,
            unionAlignment = 4,
            unionFields = [
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc = "zero_copy.h:31:20",
                  fieldName = NamePair {
                    nameC = Name "rectangle",
                    nameHsIdent = Identifier
                      "shape_rectangle"},
                  fieldComment = Nothing},
                unionFieldType = TypeStruct
                  NamePair {
                    nameC = Name "rectangle",
                    nameHsIdent = Identifier
                      "Rectangle"}
                  NameOriginInSource},
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc = "zero_copy.h:32:17",
                  fieldName = NamePair {
                    nameC = Name "circle",
                    nameHsIdent = Identifier
                      "shape_circle"},
                  fieldComment = Nothing},
                unionFieldType = TypeStruct
                  NamePair {
                    nameC = Name "circle",
                    nameHsIdent = Identifier
                      "Circle"}
                  NameOriginInSource}]},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "shape",
          commentLocation = Just
            "zero_copy.h:30:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/zero_copy.h"],
              headerInclude =
              "manual/zero_copy.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveVia
        (HsSizedByteArray 16 4),
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Shape",
      deriveInstanceComment =
      Nothing},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = Name
        "@NsVar"
        "get_shape_rectangle",
      unionGetterType = HsTypRef
        (Name
          "@NsTypeConstr"
          "Rectangle"),
      unionGetterConstr = Name
        "@NsTypeConstr"
        "Shape",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "rectangle",
          commentLocation = Just
            "zero_copy.h:31:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/zero_copy.h"],
              headerInclude =
              "manual/zero_copy.h"},
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_shape_rectangle"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = Name
        "@NsVar"
        "set_shape_rectangle",
      unionSetterType = HsTypRef
        (Name
          "@NsTypeConstr"
          "Rectangle"),
      unionSetterConstr = Name
        "@NsTypeConstr"
        "Shape",
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
                  "get_shape_rectangle"]]}},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = Name
        "@NsVar"
        "get_shape_circle",
      unionGetterType = HsTypRef
        (Name "@NsTypeConstr" "Circle"),
      unionGetterConstr = Name
        "@NsTypeConstr"
        "Shape",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "circle",
          commentLocation = Just
            "zero_copy.h:32:17",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/zero_copy.h"],
              headerInclude =
              "manual/zero_copy.h"},
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_shape_circle"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = Name
        "@NsVar"
        "set_shape_circle",
      unionSetterType = HsTypRef
        (Name "@NsTypeConstr" "Circle"),
      unionSetterConstr = Name
        "@NsTypeConstr"
        "Shape",
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
                  "get_shape_circle"]]}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Shape"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "shape_rectangle",
          hasCFieldInstanceCFieldType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Rectangle"),
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
            (Name "@NsTypeConstr" "Shape"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "shape_rectangle",
          hasFieldInstanceFieldType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Rectangle"),
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
            (Name "@NsTypeConstr" "Shape"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "shape_circle",
          hasCFieldInstanceCFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "Circle"),
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
            (Name "@NsTypeConstr" "Shape"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "shape_circle",
          hasFieldInstanceFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "Circle"),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Colour",
      structConstr = Name
        "@NsConstr"
        "Colour",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "colour_opacity",
          fieldType = HsPrimType
            HsPrimCUInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "zero_copy.h:40:16",
                fieldName = NamePair {
                  nameC = Name "opacity",
                  nameHsIdent = Identifier
                    "colour_opacity"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Unsigned),
              structFieldOffset = 0,
              structFieldWidth = Just 2},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "opacity",
              commentLocation = Just
                "zero_copy.h:40:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "colour_brightness",
          fieldType = HsPrimType
            HsPrimCUInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "zero_copy.h:41:16",
                fieldName = NamePair {
                  nameC = Name "brightness",
                  nameHsIdent = Identifier
                    "colour_brightness"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Unsigned),
              structFieldOffset = 2,
              structFieldWidth = Just 3},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "brightness",
              commentLocation = Just
                "zero_copy.h:41:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "colour_red",
          fieldType = HsPrimType
            HsPrimCUInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "zero_copy.h:42:16",
                fieldName = NamePair {
                  nameC = Name "red",
                  nameHsIdent = Identifier
                    "colour_red"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Unsigned),
              structFieldOffset = 5,
              structFieldWidth = Just 8},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "red",
              commentLocation = Just
                "zero_copy.h:42:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "colour_green",
          fieldType = HsPrimType
            HsPrimCUInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "zero_copy.h:43:16",
                fieldName = NamePair {
                  nameC = Name "green",
                  nameHsIdent = Identifier
                    "colour_green"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Unsigned),
              structFieldOffset = 13,
              structFieldWidth = Just 8},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "green",
              commentLocation = Just
                "zero_copy.h:43:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "colour_blue",
          fieldType = HsPrimType
            HsPrimCUInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "zero_copy.h:44:16",
                fieldName = NamePair {
                  nameC = Name "blue",
                  nameHsIdent = Identifier
                    "colour_blue"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Unsigned),
              structFieldOffset = 21,
              structFieldWidth = Just 8},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "blue",
              commentLocation = Just
                "zero_copy.h:44:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "zero_copy.h:39:8",
            declId = NamePair {
              nameC = Name "colour",
              nameHsIdent = Identifier
                "Colour"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["manual/zero_copy.h"],
                headerInclude =
                "manual/zero_copy.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Colour"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:40:16",
                    fieldName = NamePair {
                      nameC = Name "opacity",
                      nameHsIdent = Identifier
                        "colour_opacity"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 0,
                  structFieldWidth = Just 2},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:41:16",
                    fieldName = NamePair {
                      nameC = Name "brightness",
                      nameHsIdent = Identifier
                        "colour_brightness"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 2,
                  structFieldWidth = Just 3},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:42:16",
                    fieldName = NamePair {
                      nameC = Name "red",
                      nameHsIdent = Identifier
                        "colour_red"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 5,
                  structFieldWidth = Just 8},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:43:16",
                    fieldName = NamePair {
                      nameC = Name "green",
                      nameHsIdent = Identifier
                        "colour_green"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 13,
                  structFieldWidth = Just 8},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:44:16",
                    fieldName = NamePair {
                      nameC = Name "blue",
                      nameHsIdent = Identifier
                        "colour_blue"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 21,
                  structFieldWidth = Just 8}],
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
          commentOrigin = Just "colour",
          commentLocation = Just
            "zero_copy.h:39:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/zero_copy.h"],
              headerInclude =
              "manual/zero_copy.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Colour",
          structConstr = Name
            "@NsConstr"
            "Colour",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "colour_opacity",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:40:16",
                    fieldName = NamePair {
                      nameC = Name "opacity",
                      nameHsIdent = Identifier
                        "colour_opacity"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 0,
                  structFieldWidth = Just 2},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "opacity",
                  commentLocation = Just
                    "zero_copy.h:40:16",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["manual/zero_copy.h"],
                      headerInclude =
                      "manual/zero_copy.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "colour_brightness",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:41:16",
                    fieldName = NamePair {
                      nameC = Name "brightness",
                      nameHsIdent = Identifier
                        "colour_brightness"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 2,
                  structFieldWidth = Just 3},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just
                    "brightness",
                  commentLocation = Just
                    "zero_copy.h:41:16",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["manual/zero_copy.h"],
                      headerInclude =
                      "manual/zero_copy.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "colour_red",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:42:16",
                    fieldName = NamePair {
                      nameC = Name "red",
                      nameHsIdent = Identifier
                        "colour_red"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 5,
                  structFieldWidth = Just 8},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "red",
                  commentLocation = Just
                    "zero_copy.h:42:16",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["manual/zero_copy.h"],
                      headerInclude =
                      "manual/zero_copy.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "colour_green",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:43:16",
                    fieldName = NamePair {
                      nameC = Name "green",
                      nameHsIdent = Identifier
                        "colour_green"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 13,
                  structFieldWidth = Just 8},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "green",
                  commentLocation = Just
                    "zero_copy.h:43:16",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["manual/zero_copy.h"],
                      headerInclude =
                      "manual/zero_copy.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "colour_blue",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:44:16",
                    fieldName = NamePair {
                      nameC = Name "blue",
                      nameHsIdent = Identifier
                        "colour_blue"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 21,
                  structFieldWidth = Just 8},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "blue",
                  commentLocation = Just
                    "zero_copy.h:44:16",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["manual/zero_copy.h"],
                      headerInclude =
                      "manual/zero_copy.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "zero_copy.h:39:8",
                declId = NamePair {
                  nameC = Name "colour",
                  nameHsIdent = Identifier
                    "Colour"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["manual/zero_copy.h"],
                    headerInclude =
                    "manual/zero_copy.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Colour"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "zero_copy.h:40:16",
                        fieldName = NamePair {
                          nameC = Name "opacity",
                          nameHsIdent = Identifier
                            "colour_opacity"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Unsigned),
                      structFieldOffset = 0,
                      structFieldWidth = Just 2},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "zero_copy.h:41:16",
                        fieldName = NamePair {
                          nameC = Name "brightness",
                          nameHsIdent = Identifier
                            "colour_brightness"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Unsigned),
                      structFieldOffset = 2,
                      structFieldWidth = Just 3},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "zero_copy.h:42:16",
                        fieldName = NamePair {
                          nameC = Name "red",
                          nameHsIdent = Identifier
                            "colour_red"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Unsigned),
                      structFieldOffset = 5,
                      structFieldWidth = Just 8},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "zero_copy.h:43:16",
                        fieldName = NamePair {
                          nameC = Name "green",
                          nameHsIdent = Identifier
                            "colour_green"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Unsigned),
                      structFieldOffset = 13,
                      structFieldWidth = Just 8},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "zero_copy.h:44:16",
                        fieldName = NamePair {
                          nameC = Name "blue",
                          nameHsIdent = Identifier
                            "colour_blue"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Unsigned),
                      structFieldOffset = 21,
                      structFieldWidth = Just 8}],
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
              commentOrigin = Just "colour",
              commentLocation = Just
                "zero_copy.h:39:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
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
                    "Colour",
                  structConstr = Name
                    "@NsConstr"
                    "Colour",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "colour_opacity",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:40:16",
                            fieldName = NamePair {
                              nameC = Name "opacity",
                              nameHsIdent = Identifier
                                "colour_opacity"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Unsigned),
                          structFieldOffset = 0,
                          structFieldWidth = Just 2},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "opacity",
                          commentLocation = Just
                            "zero_copy.h:40:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "colour_brightness",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:41:16",
                            fieldName = NamePair {
                              nameC = Name "brightness",
                              nameHsIdent = Identifier
                                "colour_brightness"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Unsigned),
                          structFieldOffset = 2,
                          structFieldWidth = Just 3},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "brightness",
                          commentLocation = Just
                            "zero_copy.h:41:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "colour_red",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:42:16",
                            fieldName = NamePair {
                              nameC = Name "red",
                              nameHsIdent = Identifier
                                "colour_red"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Unsigned),
                          structFieldOffset = 5,
                          structFieldWidth = Just 8},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "red",
                          commentLocation = Just
                            "zero_copy.h:42:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "colour_green",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:43:16",
                            fieldName = NamePair {
                              nameC = Name "green",
                              nameHsIdent = Identifier
                                "colour_green"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Unsigned),
                          structFieldOffset = 13,
                          structFieldWidth = Just 8},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "green",
                          commentLocation = Just
                            "zero_copy.h:43:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "colour_blue",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:44:16",
                            fieldName = NamePair {
                              nameC = Name "blue",
                              nameHsIdent = Identifier
                                "colour_blue"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Unsigned),
                          structFieldOffset = 21,
                          structFieldWidth = Just 8},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "blue",
                          commentLocation = Just
                            "zero_copy.h:44:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "zero_copy.h:39:8",
                        declId = NamePair {
                          nameC = Name "colour",
                          nameHsIdent = Identifier
                            "Colour"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["manual/zero_copy.h"],
                            headerInclude =
                            "manual/zero_copy.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Colour"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:40:16",
                                fieldName = NamePair {
                                  nameC = Name "opacity",
                                  nameHsIdent = Identifier
                                    "colour_opacity"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Unsigned),
                              structFieldOffset = 0,
                              structFieldWidth = Just 2},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:41:16",
                                fieldName = NamePair {
                                  nameC = Name "brightness",
                                  nameHsIdent = Identifier
                                    "colour_brightness"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Unsigned),
                              structFieldOffset = 2,
                              structFieldWidth = Just 3},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:42:16",
                                fieldName = NamePair {
                                  nameC = Name "red",
                                  nameHsIdent = Identifier
                                    "colour_red"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Unsigned),
                              structFieldOffset = 5,
                              structFieldWidth = Just 8},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:43:16",
                                fieldName = NamePair {
                                  nameC = Name "green",
                                  nameHsIdent = Identifier
                                    "colour_green"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Unsigned),
                              structFieldOffset = 13,
                              structFieldWidth = Just 8},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:44:16",
                                fieldName = NamePair {
                                  nameC = Name "blue",
                                  nameHsIdent = Identifier
                                    "colour_blue"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Unsigned),
                              structFieldOffset = 21,
                              structFieldWidth = Just 8}],
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
                      commentOrigin = Just "colour",
                      commentLocation = Just
                        "zero_copy.h:39:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["manual/zero_copy.h"],
                          headerInclude =
                          "manual/zero_copy.h"},
                      commentChildren = []}})
              [
                PeekCBitfield
                  (HsStrLit "colour_opacity")
                  (Idx 0),
                PeekCBitfield
                  (HsStrLit "colour_brightness")
                  (Idx 0),
                PeekCBitfield
                  (HsStrLit "colour_red")
                  (Idx 0),
                PeekCBitfield
                  (HsStrLit "colour_green")
                  (Idx 0),
                PeekCBitfield
                  (HsStrLit "colour_blue")
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
                    "Colour",
                  structConstr = Name
                    "@NsConstr"
                    "Colour",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "colour_opacity",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:40:16",
                            fieldName = NamePair {
                              nameC = Name "opacity",
                              nameHsIdent = Identifier
                                "colour_opacity"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Unsigned),
                          structFieldOffset = 0,
                          structFieldWidth = Just 2},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "opacity",
                          commentLocation = Just
                            "zero_copy.h:40:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "colour_brightness",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:41:16",
                            fieldName = NamePair {
                              nameC = Name "brightness",
                              nameHsIdent = Identifier
                                "colour_brightness"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Unsigned),
                          structFieldOffset = 2,
                          structFieldWidth = Just 3},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "brightness",
                          commentLocation = Just
                            "zero_copy.h:41:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "colour_red",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:42:16",
                            fieldName = NamePair {
                              nameC = Name "red",
                              nameHsIdent = Identifier
                                "colour_red"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Unsigned),
                          structFieldOffset = 5,
                          structFieldWidth = Just 8},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "red",
                          commentLocation = Just
                            "zero_copy.h:42:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "colour_green",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:43:16",
                            fieldName = NamePair {
                              nameC = Name "green",
                              nameHsIdent = Identifier
                                "colour_green"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Unsigned),
                          structFieldOffset = 13,
                          structFieldWidth = Just 8},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "green",
                          commentLocation = Just
                            "zero_copy.h:43:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "colour_blue",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:44:16",
                            fieldName = NamePair {
                              nameC = Name "blue",
                              nameHsIdent = Identifier
                                "colour_blue"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Unsigned),
                          structFieldOffset = 21,
                          structFieldWidth = Just 8},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "blue",
                          commentLocation = Just
                            "zero_copy.h:44:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "zero_copy.h:39:8",
                        declId = NamePair {
                          nameC = Name "colour",
                          nameHsIdent = Identifier
                            "Colour"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["manual/zero_copy.h"],
                            headerInclude =
                            "manual/zero_copy.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Colour"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:40:16",
                                fieldName = NamePair {
                                  nameC = Name "opacity",
                                  nameHsIdent = Identifier
                                    "colour_opacity"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Unsigned),
                              structFieldOffset = 0,
                              structFieldWidth = Just 2},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:41:16",
                                fieldName = NamePair {
                                  nameC = Name "brightness",
                                  nameHsIdent = Identifier
                                    "colour_brightness"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Unsigned),
                              structFieldOffset = 2,
                              structFieldWidth = Just 3},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:42:16",
                                fieldName = NamePair {
                                  nameC = Name "red",
                                  nameHsIdent = Identifier
                                    "colour_red"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Unsigned),
                              structFieldOffset = 5,
                              structFieldWidth = Just 8},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:43:16",
                                fieldName = NamePair {
                                  nameC = Name "green",
                                  nameHsIdent = Identifier
                                    "colour_green"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Unsigned),
                              structFieldOffset = 13,
                              structFieldWidth = Just 8},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:44:16",
                                fieldName = NamePair {
                                  nameC = Name "blue",
                                  nameHsIdent = Identifier
                                    "colour_blue"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Unsigned),
                              structFieldOffset = 21,
                              structFieldWidth = Just 8}],
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
                      commentOrigin = Just "colour",
                      commentLocation = Just
                        "zero_copy.h:39:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["manual/zero_copy.h"],
                          headerInclude =
                          "manual/zero_copy.h"},
                      commentChildren = []}}
                (Add 5)
                (Seq
                  [
                    PokeCBitfield
                      (HsStrLit "colour_opacity")
                      (Idx 6)
                      (Idx 0),
                    PokeCBitfield
                      (HsStrLit "colour_brightness")
                      (Idx 6)
                      (Idx 1),
                    PokeCBitfield
                      (HsStrLit "colour_red")
                      (Idx 6)
                      (Idx 2),
                    PokeCBitfield
                      (HsStrLit "colour_green")
                      (Idx 6)
                      (Idx 3),
                    PokeCBitfield
                      (HsStrLit "colour_blue")
                      (Idx 6)
                      (Idx 4)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Colour",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Colour",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Colour"),
          hasCBitfieldInstanceFieldName =
          Name "@NsVar" "colour_opacity",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCUInt,
          hasCBitfieldInstanceBitOffset =
          0,
          hasCBitfieldInstanceBitWidth =
          2},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Colour"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "colour_opacity",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCUInt,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Colour"),
          hasCBitfieldInstanceFieldName =
          Name
            "@NsVar"
            "colour_brightness",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCUInt,
          hasCBitfieldInstanceBitOffset =
          2,
          hasCBitfieldInstanceBitWidth =
          3},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Colour"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "colour_brightness",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCUInt,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Colour"),
          hasCBitfieldInstanceFieldName =
          Name "@NsVar" "colour_red",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCUInt,
          hasCBitfieldInstanceBitOffset =
          5,
          hasCBitfieldInstanceBitWidth =
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
            (Name "@NsTypeConstr" "Colour"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "colour_red",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCUInt,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Colour"),
          hasCBitfieldInstanceFieldName =
          Name "@NsVar" "colour_green",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCUInt,
          hasCBitfieldInstanceBitOffset =
          13,
          hasCBitfieldInstanceBitWidth =
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
            (Name "@NsTypeConstr" "Colour"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "colour_green",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCUInt,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Colour"),
          hasCBitfieldInstanceFieldName =
          Name "@NsVar" "colour_blue",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCUInt,
          hasCBitfieldInstanceBitOffset =
          21,
          hasCBitfieldInstanceBitWidth =
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
            (Name "@NsTypeConstr" "Colour"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "colour_blue",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCUInt,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "MyInt",
      newtypeConstr = Name
        "@NsConstr"
        "MyInt",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_MyInt",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "zero_copy.h:50:13",
          declId = NamePair {
            nameC = Name "myInt",
            nameHsIdent = Identifier
              "MyInt"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/zero_copy.h"],
              headerInclude =
              "manual/zero_copy.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "MyInt",
              newtypeField = Name
                "@NsVar"
                "un_MyInt"},
            typedefType = TypePrim
              (PrimIntegral PrimInt Signed)},
        declSpec = DeclSpec
          CTypeSpec {
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
          commentOrigin = Just "myInt",
          commentLocation = Just
            "zero_copy.h:50:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/zero_copy.h"],
              headerInclude =
              "manual/zero_copy.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MyInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MyInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MyInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MyInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MyInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MyInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MyInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MyInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MyInt",
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
        "MyInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MyInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MyInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MyInt",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "MyInt"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_MyInt",
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
            (Name "@NsTypeConstr" "MyInt"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_MyInt",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCInt,
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Drawing",
      structConstr = Name
        "@NsConstr"
        "Drawing",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "drawing_shape",
          fieldType = HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "Shape")),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "zero_copy.h:56:16",
                fieldName = NamePair {
                  nameC = Name "shape",
                  nameHsIdent = Identifier
                    "drawing_shape"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeUnion
                  NamePair {
                    nameC = Name "shape",
                    nameHsIdent = Identifier
                      "Shape"}
                  NameOriginInSource),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "shape",
              commentLocation = Just
                "zero_copy.h:56:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "drawing_colour",
          fieldType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Colour")),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "zero_copy.h:57:18",
                fieldName = NamePair {
                  nameC = Name "colour",
                  nameHsIdent = Identifier
                    "drawing_colour"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "colour",
                    nameHsIdent = Identifier
                      "Colour"}
                  NameOriginInSource),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "colour",
              commentLocation = Just
                "zero_copy.h:57:18",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "zero_copy.h:55:16",
            declId = NamePair {
              nameC = Name "drawing",
              nameHsIdent = Identifier
                "Drawing"},
            declOrigin = NameOriginInSource,
            declAliases = [Name "drawing"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["manual/zero_copy.h"],
                headerInclude =
                "manual/zero_copy.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Drawing"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:56:16",
                    fieldName = NamePair {
                      nameC = Name "shape",
                      nameHsIdent = Identifier
                        "drawing_shape"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeUnion
                      NamePair {
                        nameC = Name "shape",
                        nameHsIdent = Identifier
                          "Shape"}
                      NameOriginInSource),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:57:18",
                    fieldName = NamePair {
                      nameC = Name "colour",
                      nameHsIdent = Identifier
                        "drawing_colour"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "colour",
                        nameHsIdent = Identifier
                          "Colour"}
                      NameOriginInSource),
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
          commentOrigin = Just "drawing",
          commentLocation = Just
            "zero_copy.h:55:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/zero_copy.h"],
              headerInclude =
              "manual/zero_copy.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Drawing",
          structConstr = Name
            "@NsConstr"
            "Drawing",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "drawing_shape",
              fieldType = HsPtr
                (HsTypRef
                  (Name "@NsTypeConstr" "Shape")),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:56:16",
                    fieldName = NamePair {
                      nameC = Name "shape",
                      nameHsIdent = Identifier
                        "drawing_shape"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeUnion
                      NamePair {
                        nameC = Name "shape",
                        nameHsIdent = Identifier
                          "Shape"}
                      NameOriginInSource),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "shape",
                  commentLocation = Just
                    "zero_copy.h:56:16",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["manual/zero_copy.h"],
                      headerInclude =
                      "manual/zero_copy.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "drawing_colour",
              fieldType = HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Colour")),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:57:18",
                    fieldName = NamePair {
                      nameC = Name "colour",
                      nameHsIdent = Identifier
                        "drawing_colour"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "colour",
                        nameHsIdent = Identifier
                          "Colour"}
                      NameOriginInSource),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "colour",
                  commentLocation = Just
                    "zero_copy.h:57:18",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["manual/zero_copy.h"],
                      headerInclude =
                      "manual/zero_copy.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "zero_copy.h:55:16",
                declId = NamePair {
                  nameC = Name "drawing",
                  nameHsIdent = Identifier
                    "Drawing"},
                declOrigin = NameOriginInSource,
                declAliases = [Name "drawing"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["manual/zero_copy.h"],
                    headerInclude =
                    "manual/zero_copy.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Drawing"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "zero_copy.h:56:16",
                        fieldName = NamePair {
                          nameC = Name "shape",
                          nameHsIdent = Identifier
                            "drawing_shape"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeUnion
                          NamePair {
                            nameC = Name "shape",
                            nameHsIdent = Identifier
                              "Shape"}
                          NameOriginInSource),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "zero_copy.h:57:18",
                        fieldName = NamePair {
                          nameC = Name "colour",
                          nameHsIdent = Identifier
                            "drawing_colour"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeStruct
                          NamePair {
                            nameC = Name "colour",
                            nameHsIdent = Identifier
                              "Colour"}
                          NameOriginInSource),
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
              commentOrigin = Just "drawing",
              commentLocation = Just
                "zero_copy.h:55:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
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
                    "Drawing",
                  structConstr = Name
                    "@NsConstr"
                    "Drawing",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "drawing_shape",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name "@NsTypeConstr" "Shape")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:56:16",
                            fieldName = NamePair {
                              nameC = Name "shape",
                              nameHsIdent = Identifier
                                "drawing_shape"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeUnion
                              NamePair {
                                nameC = Name "shape",
                                nameHsIdent = Identifier
                                  "Shape"}
                              NameOriginInSource),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "shape",
                          commentLocation = Just
                            "zero_copy.h:56:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "drawing_colour",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name
                            "@NsTypeConstr"
                            "Colour")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:57:18",
                            fieldName = NamePair {
                              nameC = Name "colour",
                              nameHsIdent = Identifier
                                "drawing_colour"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "colour",
                                nameHsIdent = Identifier
                                  "Colour"}
                              NameOriginInSource),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "colour",
                          commentLocation = Just
                            "zero_copy.h:57:18",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "zero_copy.h:55:16",
                        declId = NamePair {
                          nameC = Name "drawing",
                          nameHsIdent = Identifier
                            "Drawing"},
                        declOrigin = NameOriginInSource,
                        declAliases = [Name "drawing"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["manual/zero_copy.h"],
                            headerInclude =
                            "manual/zero_copy.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Drawing"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:56:16",
                                fieldName = NamePair {
                                  nameC = Name "shape",
                                  nameHsIdent = Identifier
                                    "drawing_shape"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeUnion
                                  NamePair {
                                    nameC = Name "shape",
                                    nameHsIdent = Identifier
                                      "Shape"}
                                  NameOriginInSource),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:57:18",
                                fieldName = NamePair {
                                  nameC = Name "colour",
                                  nameHsIdent = Identifier
                                    "drawing_colour"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "colour",
                                    nameHsIdent = Identifier
                                      "Colour"}
                                  NameOriginInSource),
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
                      commentOrigin = Just "drawing",
                      commentLocation = Just
                        "zero_copy.h:55:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["manual/zero_copy.h"],
                          headerInclude =
                          "manual/zero_copy.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "drawing_shape")
                  (Idx 0),
                PeekCField
                  (HsStrLit "drawing_colour")
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
                    "Drawing",
                  structConstr = Name
                    "@NsConstr"
                    "Drawing",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "drawing_shape",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name "@NsTypeConstr" "Shape")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:56:16",
                            fieldName = NamePair {
                              nameC = Name "shape",
                              nameHsIdent = Identifier
                                "drawing_shape"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeUnion
                              NamePair {
                                nameC = Name "shape",
                                nameHsIdent = Identifier
                                  "Shape"}
                              NameOriginInSource),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "shape",
                          commentLocation = Just
                            "zero_copy.h:56:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "drawing_colour",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name
                            "@NsTypeConstr"
                            "Colour")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:57:18",
                            fieldName = NamePair {
                              nameC = Name "colour",
                              nameHsIdent = Identifier
                                "drawing_colour"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "colour",
                                nameHsIdent = Identifier
                                  "Colour"}
                              NameOriginInSource),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "colour",
                          commentLocation = Just
                            "zero_copy.h:57:18",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "zero_copy.h:55:16",
                        declId = NamePair {
                          nameC = Name "drawing",
                          nameHsIdent = Identifier
                            "Drawing"},
                        declOrigin = NameOriginInSource,
                        declAliases = [Name "drawing"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["manual/zero_copy.h"],
                            headerInclude =
                            "manual/zero_copy.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Drawing"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:56:16",
                                fieldName = NamePair {
                                  nameC = Name "shape",
                                  nameHsIdent = Identifier
                                    "drawing_shape"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeUnion
                                  NamePair {
                                    nameC = Name "shape",
                                    nameHsIdent = Identifier
                                      "Shape"}
                                  NameOriginInSource),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:57:18",
                                fieldName = NamePair {
                                  nameC = Name "colour",
                                  nameHsIdent = Identifier
                                    "drawing_colour"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "colour",
                                    nameHsIdent = Identifier
                                      "Colour"}
                                  NameOriginInSource),
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
                      commentOrigin = Just "drawing",
                      commentLocation = Just
                        "zero_copy.h:55:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["manual/zero_copy.h"],
                          headerInclude =
                          "manual/zero_copy.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "drawing_shape")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "drawing_colour")
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
        "Drawing",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Drawing",
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
              "Drawing"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "drawing_shape",
          hasCFieldInstanceCFieldType =
          HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "Shape")),
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
              "Drawing"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "drawing_shape",
          hasFieldInstanceFieldType =
          HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "Shape")),
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
              "Drawing"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "drawing_colour",
          hasCFieldInstanceCFieldType =
          HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Colour")),
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
            (Name
              "@NsTypeConstr"
              "Drawing"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "drawing_colour",
          hasFieldInstanceFieldType =
          HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Colour")),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Tic_tac_toe",
      structConstr = Name
        "@NsConstr"
        "Tic_tac_toe",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "tic_tac_toe_row1",
          fieldType = HsConstArray
            3
            (HsPrimType HsPrimCInt),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "zero_copy.h:64:7",
                fieldName = NamePair {
                  nameC = Name "row1",
                  nameHsIdent = Identifier
                    "tic_tac_toe_row1"},
                fieldComment = Nothing},
              structFieldType = TypeConstArray
                3
                (TypePrim
                  (PrimIntegral PrimInt Signed)),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "row1",
              commentLocation = Just
                "zero_copy.h:64:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "tic_tac_toe_row2",
          fieldType = HsConstArray
            3
            (HsPrimType HsPrimCInt),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "zero_copy.h:65:7",
                fieldName = NamePair {
                  nameC = Name "row2",
                  nameHsIdent = Identifier
                    "tic_tac_toe_row2"},
                fieldComment = Nothing},
              structFieldType = TypeConstArray
                3
                (TypePrim
                  (PrimIntegral PrimInt Signed)),
              structFieldOffset = 96,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "row2",
              commentLocation = Just
                "zero_copy.h:65:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "tic_tac_toe_row3",
          fieldType = HsConstArray
            3
            (HsPrimType HsPrimCInt),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "zero_copy.h:66:7",
                fieldName = NamePair {
                  nameC = Name "row3",
                  nameHsIdent = Identifier
                    "tic_tac_toe_row3"},
                fieldComment = Nothing},
              structFieldType = TypeConstArray
                3
                (TypePrim
                  (PrimIntegral PrimInt Signed)),
              structFieldOffset = 192,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "row3",
              commentLocation = Just
                "zero_copy.h:66:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "zero_copy.h:63:16",
            declId = NamePair {
              nameC = Name "tic_tac_toe",
              nameHsIdent = Identifier
                "Tic_tac_toe"},
            declOrigin = NameOriginInSource,
            declAliases = [
              Name "tic_tac_toe"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["manual/zero_copy.h"],
                headerInclude =
                "manual/zero_copy.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name
                  "@NsConstr"
                  "Tic_tac_toe"),
              structSizeof = 36,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:64:7",
                    fieldName = NamePair {
                      nameC = Name "row1",
                      nameHsIdent = Identifier
                        "tic_tac_toe_row1"},
                    fieldComment = Nothing},
                  structFieldType = TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral PrimInt Signed)),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:65:7",
                    fieldName = NamePair {
                      nameC = Name "row2",
                      nameHsIdent = Identifier
                        "tic_tac_toe_row2"},
                    fieldComment = Nothing},
                  structFieldType = TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral PrimInt Signed)),
                  structFieldOffset = 96,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:66:7",
                    fieldName = NamePair {
                      nameC = Name "row3",
                      nameHsIdent = Identifier
                        "tic_tac_toe_row3"},
                    fieldComment = Nothing},
                  structFieldType = TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral PrimInt Signed)),
                  structFieldOffset = 192,
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
          commentOrigin = Just
            "tic_tac_toe",
          commentLocation = Just
            "zero_copy.h:63:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/zero_copy.h"],
              headerInclude =
              "manual/zero_copy.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Tic_tac_toe",
          structConstr = Name
            "@NsConstr"
            "Tic_tac_toe",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "tic_tac_toe_row1",
              fieldType = HsConstArray
                3
                (HsPrimType HsPrimCInt),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:64:7",
                    fieldName = NamePair {
                      nameC = Name "row1",
                      nameHsIdent = Identifier
                        "tic_tac_toe_row1"},
                    fieldComment = Nothing},
                  structFieldType = TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral PrimInt Signed)),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "row1",
                  commentLocation = Just
                    "zero_copy.h:64:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["manual/zero_copy.h"],
                      headerInclude =
                      "manual/zero_copy.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "tic_tac_toe_row2",
              fieldType = HsConstArray
                3
                (HsPrimType HsPrimCInt),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:65:7",
                    fieldName = NamePair {
                      nameC = Name "row2",
                      nameHsIdent = Identifier
                        "tic_tac_toe_row2"},
                    fieldComment = Nothing},
                  structFieldType = TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral PrimInt Signed)),
                  structFieldOffset = 96,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "row2",
                  commentLocation = Just
                    "zero_copy.h:65:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["manual/zero_copy.h"],
                      headerInclude =
                      "manual/zero_copy.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "tic_tac_toe_row3",
              fieldType = HsConstArray
                3
                (HsPrimType HsPrimCInt),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:66:7",
                    fieldName = NamePair {
                      nameC = Name "row3",
                      nameHsIdent = Identifier
                        "tic_tac_toe_row3"},
                    fieldComment = Nothing},
                  structFieldType = TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral PrimInt Signed)),
                  structFieldOffset = 192,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "row3",
                  commentLocation = Just
                    "zero_copy.h:66:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["manual/zero_copy.h"],
                      headerInclude =
                      "manual/zero_copy.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "zero_copy.h:63:16",
                declId = NamePair {
                  nameC = Name "tic_tac_toe",
                  nameHsIdent = Identifier
                    "Tic_tac_toe"},
                declOrigin = NameOriginInSource,
                declAliases = [
                  Name "tic_tac_toe"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["manual/zero_copy.h"],
                    headerInclude =
                    "manual/zero_copy.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name
                      "@NsConstr"
                      "Tic_tac_toe"),
                  structSizeof = 36,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "zero_copy.h:64:7",
                        fieldName = NamePair {
                          nameC = Name "row1",
                          nameHsIdent = Identifier
                            "tic_tac_toe_row1"},
                        fieldComment = Nothing},
                      structFieldType = TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral PrimInt Signed)),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "zero_copy.h:65:7",
                        fieldName = NamePair {
                          nameC = Name "row2",
                          nameHsIdent = Identifier
                            "tic_tac_toe_row2"},
                        fieldComment = Nothing},
                      structFieldType = TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral PrimInt Signed)),
                      structFieldOffset = 96,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "zero_copy.h:66:7",
                        fieldName = NamePair {
                          nameC = Name "row3",
                          nameHsIdent = Identifier
                            "tic_tac_toe_row3"},
                        fieldComment = Nothing},
                      structFieldType = TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral PrimInt Signed)),
                      structFieldOffset = 192,
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
              commentOrigin = Just
                "tic_tac_toe",
              commentLocation = Just
                "zero_copy.h:63:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 36,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Tic_tac_toe",
                  structConstr = Name
                    "@NsConstr"
                    "Tic_tac_toe",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "tic_tac_toe_row1",
                      fieldType = HsConstArray
                        3
                        (HsPrimType HsPrimCInt),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:64:7",
                            fieldName = NamePair {
                              nameC = Name "row1",
                              nameHsIdent = Identifier
                                "tic_tac_toe_row1"},
                            fieldComment = Nothing},
                          structFieldType = TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral PrimInt Signed)),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "row1",
                          commentLocation = Just
                            "zero_copy.h:64:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "tic_tac_toe_row2",
                      fieldType = HsConstArray
                        3
                        (HsPrimType HsPrimCInt),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:65:7",
                            fieldName = NamePair {
                              nameC = Name "row2",
                              nameHsIdent = Identifier
                                "tic_tac_toe_row2"},
                            fieldComment = Nothing},
                          structFieldType = TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral PrimInt Signed)),
                          structFieldOffset = 96,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "row2",
                          commentLocation = Just
                            "zero_copy.h:65:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "tic_tac_toe_row3",
                      fieldType = HsConstArray
                        3
                        (HsPrimType HsPrimCInt),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:66:7",
                            fieldName = NamePair {
                              nameC = Name "row3",
                              nameHsIdent = Identifier
                                "tic_tac_toe_row3"},
                            fieldComment = Nothing},
                          structFieldType = TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral PrimInt Signed)),
                          structFieldOffset = 192,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "row3",
                          commentLocation = Just
                            "zero_copy.h:66:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "zero_copy.h:63:16",
                        declId = NamePair {
                          nameC = Name "tic_tac_toe",
                          nameHsIdent = Identifier
                            "Tic_tac_toe"},
                        declOrigin = NameOriginInSource,
                        declAliases = [
                          Name "tic_tac_toe"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["manual/zero_copy.h"],
                            headerInclude =
                            "manual/zero_copy.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Tic_tac_toe"),
                          structSizeof = 36,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:64:7",
                                fieldName = NamePair {
                                  nameC = Name "row1",
                                  nameHsIdent = Identifier
                                    "tic_tac_toe_row1"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                3
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:65:7",
                                fieldName = NamePair {
                                  nameC = Name "row2",
                                  nameHsIdent = Identifier
                                    "tic_tac_toe_row2"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                3
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
                              structFieldOffset = 96,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:66:7",
                                fieldName = NamePair {
                                  nameC = Name "row3",
                                  nameHsIdent = Identifier
                                    "tic_tac_toe_row3"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                3
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
                              structFieldOffset = 192,
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
                      commentOrigin = Just
                        "tic_tac_toe",
                      commentLocation = Just
                        "zero_copy.h:63:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["manual/zero_copy.h"],
                          headerInclude =
                          "manual/zero_copy.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "tic_tac_toe_row1")
                  (Idx 0),
                PeekCField
                  (HsStrLit "tic_tac_toe_row2")
                  (Idx 0),
                PeekCField
                  (HsStrLit "tic_tac_toe_row3")
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
                    "Tic_tac_toe",
                  structConstr = Name
                    "@NsConstr"
                    "Tic_tac_toe",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "tic_tac_toe_row1",
                      fieldType = HsConstArray
                        3
                        (HsPrimType HsPrimCInt),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:64:7",
                            fieldName = NamePair {
                              nameC = Name "row1",
                              nameHsIdent = Identifier
                                "tic_tac_toe_row1"},
                            fieldComment = Nothing},
                          structFieldType = TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral PrimInt Signed)),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "row1",
                          commentLocation = Just
                            "zero_copy.h:64:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "tic_tac_toe_row2",
                      fieldType = HsConstArray
                        3
                        (HsPrimType HsPrimCInt),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:65:7",
                            fieldName = NamePair {
                              nameC = Name "row2",
                              nameHsIdent = Identifier
                                "tic_tac_toe_row2"},
                            fieldComment = Nothing},
                          structFieldType = TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral PrimInt Signed)),
                          structFieldOffset = 96,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "row2",
                          commentLocation = Just
                            "zero_copy.h:65:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "tic_tac_toe_row3",
                      fieldType = HsConstArray
                        3
                        (HsPrimType HsPrimCInt),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:66:7",
                            fieldName = NamePair {
                              nameC = Name "row3",
                              nameHsIdent = Identifier
                                "tic_tac_toe_row3"},
                            fieldComment = Nothing},
                          structFieldType = TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral PrimInt Signed)),
                          structFieldOffset = 192,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "row3",
                          commentLocation = Just
                            "zero_copy.h:66:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "zero_copy.h:63:16",
                        declId = NamePair {
                          nameC = Name "tic_tac_toe",
                          nameHsIdent = Identifier
                            "Tic_tac_toe"},
                        declOrigin = NameOriginInSource,
                        declAliases = [
                          Name "tic_tac_toe"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["manual/zero_copy.h"],
                            headerInclude =
                            "manual/zero_copy.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Tic_tac_toe"),
                          structSizeof = 36,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:64:7",
                                fieldName = NamePair {
                                  nameC = Name "row1",
                                  nameHsIdent = Identifier
                                    "tic_tac_toe_row1"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                3
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:65:7",
                                fieldName = NamePair {
                                  nameC = Name "row2",
                                  nameHsIdent = Identifier
                                    "tic_tac_toe_row2"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                3
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
                              structFieldOffset = 96,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:66:7",
                                fieldName = NamePair {
                                  nameC = Name "row3",
                                  nameHsIdent = Identifier
                                    "tic_tac_toe_row3"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                3
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
                              structFieldOffset = 192,
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
                      commentOrigin = Just
                        "tic_tac_toe",
                      commentLocation = Just
                        "zero_copy.h:63:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["manual/zero_copy.h"],
                          headerInclude =
                          "manual/zero_copy.h"},
                      commentChildren = []}}
                (Add 3)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "tic_tac_toe_row1")
                      (Idx 4)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "tic_tac_toe_row2")
                      (Idx 4)
                      (Idx 1),
                    PokeCField
                      (HsStrLit "tic_tac_toe_row3")
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
        "Tic_tac_toe",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Tic_tac_toe",
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
              "Tic_tac_toe"),
          hasCFieldInstanceFieldName =
          Name
            "@NsVar"
            "tic_tac_toe_row1",
          hasCFieldInstanceCFieldType =
          HsConstArray
            3
            (HsPrimType HsPrimCInt),
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
              "Tic_tac_toe"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "tic_tac_toe_row1",
          hasFieldInstanceFieldType =
          HsConstArray
            3
            (HsPrimType HsPrimCInt),
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
              "Tic_tac_toe"),
          hasCFieldInstanceFieldName =
          Name
            "@NsVar"
            "tic_tac_toe_row2",
          hasCFieldInstanceCFieldType =
          HsConstArray
            3
            (HsPrimType HsPrimCInt),
          hasCFieldInstanceFieldOffset =
          12},
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
              "Tic_tac_toe"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "tic_tac_toe_row2",
          hasFieldInstanceFieldType =
          HsConstArray
            3
            (HsPrimType HsPrimCInt),
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
              "Tic_tac_toe"),
          hasCFieldInstanceFieldName =
          Name
            "@NsVar"
            "tic_tac_toe_row3",
          hasCFieldInstanceCFieldType =
          HsConstArray
            3
            (HsPrimType HsPrimCInt),
          hasCFieldInstanceFieldOffset =
          24},
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
              "Tic_tac_toe"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "tic_tac_toe_row3",
          hasFieldInstanceFieldType =
          HsConstArray
            3
            (HsPrimType HsPrimCInt),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Vector",
      structConstr = Name
        "@NsConstr"
        "Vector",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "vector_len",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "zero_copy.h:73:7",
                fieldName = NamePair {
                  nameC = Name "len",
                  nameHsIdent = Identifier
                    "vector_len"},
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
                "zero_copy.h:73:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "zero_copy.h:72:8",
            declId = NamePair {
              nameC = Name "vector",
              nameHsIdent = Identifier
                "Vector"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["manual/zero_copy.h"],
                headerInclude =
                "manual/zero_copy.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Vector"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:73:7",
                    fieldName = NamePair {
                      nameC = Name "len",
                      nameHsIdent = Identifier
                        "vector_len"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing}],
              structFlam = Just
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:74:8",
                    fieldName = NamePair {
                      nameC = Name "data",
                      nameHsIdent = Identifier
                        "vector_data"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing}},
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
          commentOrigin = Just "vector",
          commentLocation = Just
            "zero_copy.h:72:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/zero_copy.h"],
              headerInclude =
              "manual/zero_copy.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Vector",
          structConstr = Name
            "@NsConstr"
            "Vector",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "vector_len",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:73:7",
                    fieldName = NamePair {
                      nameC = Name "len",
                      nameHsIdent = Identifier
                        "vector_len"},
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
                    "zero_copy.h:73:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["manual/zero_copy.h"],
                      headerInclude =
                      "manual/zero_copy.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "zero_copy.h:72:8",
                declId = NamePair {
                  nameC = Name "vector",
                  nameHsIdent = Identifier
                    "Vector"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["manual/zero_copy.h"],
                    headerInclude =
                    "manual/zero_copy.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Vector"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "zero_copy.h:73:7",
                        fieldName = NamePair {
                          nameC = Name "len",
                          nameHsIdent = Identifier
                            "vector_len"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Just
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "zero_copy.h:74:8",
                        fieldName = NamePair {
                          nameC = Name "data",
                          nameHsIdent = Identifier
                            "vector_data"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing}},
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
              commentOrigin = Just "vector",
              commentLocation = Just
                "zero_copy.h:72:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
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
                    "Vector",
                  structConstr = Name
                    "@NsConstr"
                    "Vector",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "vector_len",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:73:7",
                            fieldName = NamePair {
                              nameC = Name "len",
                              nameHsIdent = Identifier
                                "vector_len"},
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
                            "zero_copy.h:73:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "zero_copy.h:72:8",
                        declId = NamePair {
                          nameC = Name "vector",
                          nameHsIdent = Identifier
                            "Vector"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["manual/zero_copy.h"],
                            headerInclude =
                            "manual/zero_copy.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Vector"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:73:7",
                                fieldName = NamePair {
                                  nameC = Name "len",
                                  nameHsIdent = Identifier
                                    "vector_len"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Just
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:74:8",
                                fieldName = NamePair {
                                  nameC = Name "data",
                                  nameHsIdent = Identifier
                                    "vector_data"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing}},
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
                      commentOrigin = Just "vector",
                      commentLocation = Just
                        "zero_copy.h:72:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["manual/zero_copy.h"],
                          headerInclude =
                          "manual/zero_copy.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "vector_len")
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
                    "Vector",
                  structConstr = Name
                    "@NsConstr"
                    "Vector",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "vector_len",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "zero_copy.h:73:7",
                            fieldName = NamePair {
                              nameC = Name "len",
                              nameHsIdent = Identifier
                                "vector_len"},
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
                            "zero_copy.h:73:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/zero_copy.h"],
                              headerInclude =
                              "manual/zero_copy.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "zero_copy.h:72:8",
                        declId = NamePair {
                          nameC = Name "vector",
                          nameHsIdent = Identifier
                            "Vector"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["manual/zero_copy.h"],
                            headerInclude =
                            "manual/zero_copy.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Vector"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:73:7",
                                fieldName = NamePair {
                                  nameC = Name "len",
                                  nameHsIdent = Identifier
                                    "vector_len"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Just
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "zero_copy.h:74:8",
                                fieldName = NamePair {
                                  nameC = Name "data",
                                  nameHsIdent = Identifier
                                    "vector_data"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing}},
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
                      commentOrigin = Just "vector",
                      commentLocation = Just
                        "zero_copy.h:72:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["manual/zero_copy.h"],
                          headerInclude =
                          "manual/zero_copy.h"},
                      commentChildren = []}}
                (Add 1)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "vector_len")
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
        "Vector",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Vector",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasFLAM
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Vector",
          structConstr = Name
            "@NsConstr"
            "Vector",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "vector_len",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "zero_copy.h:73:7",
                    fieldName = NamePair {
                      nameC = Name "len",
                      nameHsIdent = Identifier
                        "vector_len"},
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
                    "zero_copy.h:73:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["manual/zero_copy.h"],
                      headerInclude =
                      "manual/zero_copy.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "zero_copy.h:72:8",
                declId = NamePair {
                  nameC = Name "vector",
                  nameHsIdent = Identifier
                    "Vector"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["manual/zero_copy.h"],
                    headerInclude =
                    "manual/zero_copy.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Vector"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "zero_copy.h:73:7",
                        fieldName = NamePair {
                          nameC = Name "len",
                          nameHsIdent = Identifier
                            "vector_len"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Just
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "zero_copy.h:74:8",
                        fieldName = NamePair {
                          nameC = Name "data",
                          nameHsIdent = Identifier
                            "vector_data"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing}},
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
              commentOrigin = Just "vector",
              commentLocation = Just
                "zero_copy.h:72:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/zero_copy.h"],
                  headerInclude =
                  "manual/zero_copy.h"},
              commentChildren = []}}
        (HsPrimType HsPrimCChar)
        4,
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Vector"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "vector_len",
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
            (Name "@NsTypeConstr" "Vector"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "vector_len",
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
        "Triplet",
      newtypeConstr = Name
        "@NsConstr"
        "Triplet",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Triplet",
        fieldType = HsConstArray
          3
          (HsPrimType HsPrimCInt),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "zero_copy.h:82:13",
          declId = NamePair {
            nameC = Name "triplet",
            nameHsIdent = Identifier
              "Triplet"},
          declOrigin = NameOriginInSource,
          declAliases = [Name "matrix"],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/zero_copy.h"],
              headerInclude =
              "manual/zero_copy.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Triplet",
              newtypeField = Name
                "@NsVar"
                "un_Triplet"},
            typedefType = TypeConstArray
              3
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "triplet",
          commentLocation = Just
            "zero_copy.h:82:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/zero_copy.h"],
              headerInclude =
              "manual/zero_copy.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Triplet",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Triplet",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Triplet",
      deriveInstanceComment =
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
              "Triplet"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_Triplet",
          hasFieldInstanceFieldType =
          HsConstArray
            3
            (HsPrimType HsPrimCInt),
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
              "Triplet"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_Triplet",
          hasCFieldInstanceCFieldType =
          HsConstArray
            3
            (HsPrimType HsPrimCInt),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Matrix",
      newtypeConstr = Name
        "@NsConstr"
        "Matrix",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Matrix",
        fieldType = HsConstArray
          3
          (HsTypRef
            (Name
              "@NsTypeConstr"
              "Triplet")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "zero_copy.h:83:17",
          declId = NamePair {
            nameC = Name "matrix",
            nameHsIdent = Identifier
              "Matrix"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/zero_copy.h"],
              headerInclude =
              "manual/zero_copy.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Matrix",
              newtypeField = Name
                "@NsVar"
                "un_Matrix"},
            typedefType = TypeConstArray
              3
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "triplet",
                    nameHsIdent = Identifier
                      "Triplet"}
                  (TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "matrix",
          commentLocation = Just
            "zero_copy.h:83:17",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/zero_copy.h"],
              headerInclude =
              "manual/zero_copy.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Matrix",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Matrix",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Matrix",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Matrix"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_Matrix",
          hasFieldInstanceFieldType =
          HsConstArray
            3
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")),
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
            (Name "@NsTypeConstr" "Matrix"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_Matrix",
          hasCFieldInstanceCFieldType =
          HsConstArray
            3
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "reverse",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "input"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Vector")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "input",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "output"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Vector")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "output",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_manualzero_copy_a617e0cb5d95cd52",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "signed int hs_bindgen_test_manualzero_copy_a617e0cb5d95cd52 (\n",
              "  struct vector const *arg1,\n",
              "  struct vector *arg2\n",
              ")\n",
              "{\n",
              "  return reverse(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "manual/zero_copy.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "input",
                  nameHsIdent = Identifier
                    "input"})
              (TypePointer
                (TypeQualified
                  TypeQualifierConst
                  (TypeStruct
                    NamePair {
                      nameC = Name "vector",
                      nameHsIdent = Identifier
                        "Vector"}
                    NameOriginInSource))),
            _×_
              (Just
                NamePair {
                  nameC = Name "output",
                  nameHsIdent = Identifier
                    "output"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "vector",
                    nameHsIdent = Identifier
                      "Vector"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "reverse",
          commentLocation = Just
            "zero_copy.h:77:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/zero_copy.h"],
              headerInclude =
              "manual/zero_copy.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "transpose_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "input"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "output"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_manualzero_copy_f3d0c8dd1a83b3d0",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "void hs_bindgen_test_manualzero_copy_f3d0c8dd1a83b3d0 (\n",
              "  triplet *arg1,\n",
              "  triplet *arg2\n",
              ")\n",
              "{\n",
              "  transpose(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "manual/zero_copy.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "input",
                  nameHsIdent = Identifier
                    "input"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "matrix",
                      nameHsIdent = Identifier
                        "Matrix"}
                    (TypeConstArray
                      3
                      (TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "triplet",
                            nameHsIdent = Identifier
                              "Triplet"}
                          (TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral
                                PrimInt
                                Signed))))))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "output",
                  nameHsIdent = Identifier
                    "output"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "matrix",
                    nameHsIdent = Identifier
                      "Matrix"}
                  (TypeConstArray
                    3
                    (TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "triplet",
                          nameHsIdent = Identifier
                            "Triplet"}
                        (TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral
                              PrimInt
                              Signed))))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Pointer-based API for",
              Identifier "transpose"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "transpose",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "input"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Matrix"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "input",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "output"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "output",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimUnit),
      functionDeclBody =
      `ELam "x" (ELam "x" (EApp (EApp (EGlobal ConstantArray_withPtr) (EBound 1)) (ELam "ptr" (EApp (EApp (EFree "transpose_wrapper") (EBound 0)) (EBound 1)))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "input",
                  nameHsIdent = Identifier
                    "input"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "matrix",
                      nameHsIdent = Identifier
                        "Matrix"}
                    (TypeConstArray
                      3
                      (TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "triplet",
                            nameHsIdent = Identifier
                              "Triplet"}
                          (TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral
                                PrimInt
                                Signed))))))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "output",
                  nameHsIdent = Identifier
                    "output"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "matrix",
                    nameHsIdent = Identifier
                      "Matrix"}
                  (TypeConstArray
                    3
                    (TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "triplet",
                          nameHsIdent = Identifier
                            "Triplet"}
                        (TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral
                              PrimInt
                              Signed))))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      functionDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "transpose",
          commentLocation = Just
            "zero_copy.h:85:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/zero_copy.h"],
              headerInclude =
              "manual/zero_copy.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "reverse",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "input"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Vector")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "input",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "output"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Vector")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "output",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_manualzero_copy_ca203e332b4afe73",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "signed int hs_bindgen_test_manualzero_copy_ca203e332b4afe73 (\n",
              "  struct vector const *arg1,\n",
              "  struct vector *arg2\n",
              ")\n",
              "{\n",
              "  return reverse(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "manual/zero_copy.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "input",
                  nameHsIdent = Identifier
                    "input"})
              (TypePointer
                (TypeQualified
                  TypeQualifierConst
                  (TypeStruct
                    NamePair {
                      nameC = Name "vector",
                      nameHsIdent = Identifier
                        "Vector"}
                    NameOriginInSource))),
            _×_
              (Just
                NamePair {
                  nameC = Name "output",
                  nameHsIdent = Identifier
                    "output"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "vector",
                    nameHsIdent = Identifier
                      "Vector"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "reverse",
          commentLocation = Just
            "zero_copy.h:77:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/zero_copy.h"],
              headerInclude =
              "manual/zero_copy.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "transpose_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "input"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "output"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_manualzero_copy_d7aa7016f1b951b2",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "void hs_bindgen_test_manualzero_copy_d7aa7016f1b951b2 (\n",
              "  triplet *arg1,\n",
              "  triplet *arg2\n",
              ")\n",
              "{\n",
              "  transpose(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "manual/zero_copy.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "input",
                  nameHsIdent = Identifier
                    "input"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "matrix",
                      nameHsIdent = Identifier
                        "Matrix"}
                    (TypeConstArray
                      3
                      (TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "triplet",
                            nameHsIdent = Identifier
                              "Triplet"}
                          (TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral
                                PrimInt
                                Signed))))))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "output",
                  nameHsIdent = Identifier
                    "output"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "matrix",
                    nameHsIdent = Identifier
                      "Matrix"}
                  (TypeConstArray
                    3
                    (TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "triplet",
                          nameHsIdent = Identifier
                            "Triplet"}
                        (TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral
                              PrimInt
                              Signed))))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Pointer-based API for",
              Identifier "transpose"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "transpose",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "input"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Matrix"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "input",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "output"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "output",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimUnit),
      functionDeclBody =
      `ELam "x" (ELam "x" (EApp (EApp (EGlobal ConstantArray_withPtr) (EBound 1)) (ELam "ptr" (EApp (EApp (EFree "transpose_wrapper") (EBound 0)) (EBound 1)))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "input",
                  nameHsIdent = Identifier
                    "input"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "matrix",
                      nameHsIdent = Identifier
                        "Matrix"}
                    (TypeConstArray
                      3
                      (TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "triplet",
                            nameHsIdent = Identifier
                              "Triplet"}
                          (TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral
                                PrimInt
                                Signed))))))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "output",
                  nameHsIdent = Identifier
                    "output"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "matrix",
                    nameHsIdent = Identifier
                      "Matrix"}
                  (TypeConstArray
                    3
                    (TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "triplet",
                          nameHsIdent = Identifier
                            "Triplet"}
                        (TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral
                              PrimInt
                              Signed))))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      functionDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "transpose",
          commentLocation = Just
            "zero_copy.h:85:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/zero_copy.h"],
              headerInclude =
              "manual/zero_copy.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_manualzero_copy_c3d266ec84fe0678",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Vector")))
              (HsFun
                (HsPtr
                  (HsTypRef
                    (Name
                      "@NsTypeConstr"
                      "Vector")))
                (HsIO
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_manualzero_copy_c3d266ec84fe0678",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_reverse_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*hs_bindgen_test_manualzero_copy_c3d266ec84fe0678 (void)) (\n",
              "  struct vector const *arg1,\n",
              "  struct vector *arg2\n",
              ")\n",
              "{\n",
              "  return &reverse;\n",
              "}"],
          capiWrapperImport =
          "manual/zero_copy.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeQualified
                TypeQualifierConst
                (TypeStruct
                  NamePair {
                    nameC = Name "vector",
                    nameHsIdent = Identifier
                      "Vector"}
                  NameOriginInSource)),
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name "vector",
                  nameHsIdent = Identifier
                    "Vector"}
                NameOriginInSource)]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_manualzero_copy_fc1dad225b555299",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "Matrix"))
              (HsFun
                (HsTypRef
                  (Name "@NsTypeConstr" "Matrix"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_manualzero_copy_fc1dad225b555299",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_transpose_ptr */\n",
              "__attribute__ ((const))\n",
              "void (*hs_bindgen_test_manualzero_copy_fc1dad225b555299 (void)) (\n",
              "  matrix const arg1,\n",
              "  matrix arg2\n",
              ")\n",
              "{\n",
              "  return &transpose;\n",
              "}"],
          capiWrapperImport =
          "manual/zero_copy.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeQualified
              TypeQualifierConst
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "matrix",
                    nameHsIdent = Identifier
                      "Matrix"}
                  (TypeConstArray
                    3
                    (TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "triplet",
                          nameHsIdent = Identifier
                            "Triplet"}
                        (TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral
                              PrimInt
                              Signed)))))))),
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "matrix",
                  nameHsIdent = Identifier
                    "Matrix"}
                (TypeConstArray
                  3
                  (TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "triplet",
                        nameHsIdent = Identifier
                          "Triplet"}
                      (TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral
                            PrimInt
                            Signed)))))))]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
