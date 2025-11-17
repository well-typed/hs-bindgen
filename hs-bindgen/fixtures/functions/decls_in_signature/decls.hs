[
  DeclEmpty
    EmptyData {
      emptyDataName = Name
        "@NsTypeConstr"
        "Opaque",
      emptyDataOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "decls_in_signature.h:2:8",
          declId = NamePair {
            nameC = Name "opaque",
            nameHsIdent = Identifier
              "Opaque"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "functions/decls_in_signature.h"],
              headerInclude =
              "functions/decls_in_signature.h"},
          declComment = Nothing},
        declKind = Opaque
          (NameKindTagged TagKindStruct),
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      emptyDataComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "opaque",
          commentLocation = Just
            "decls_in_signature.h:2:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "functions/decls_in_signature.h"],
              headerInclude =
              "functions/decls_in_signature.h"},
          commentChildren = []}},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Outside",
      structConstr = Name
        "@NsConstr"
        "Outside",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "outside_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "decls_in_signature.h:4:7",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "outside_x"},
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
                "decls_in_signature.h:4:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "functions/decls_in_signature.h"],
                  headerInclude =
                  "functions/decls_in_signature.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "outside_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "decls_in_signature.h:5:7",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier
                    "outside_y"},
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
                "decls_in_signature.h:5:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "functions/decls_in_signature.h"],
                  headerInclude =
                  "functions/decls_in_signature.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "decls_in_signature.h:3:8",
            declId = NamePair {
              nameC = Name "outside",
              nameHsIdent = Identifier
                "Outside"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  [
                    "functions/decls_in_signature.h"],
                headerInclude =
                "functions/decls_in_signature.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Outside"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "decls_in_signature.h:4:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "outside_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "decls_in_signature.h:5:7",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "outside_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
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
          commentOrigin = Just "outside",
          commentLocation = Just
            "decls_in_signature.h:3:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "functions/decls_in_signature.h"],
              headerInclude =
              "functions/decls_in_signature.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Outside",
          structConstr = Name
            "@NsConstr"
            "Outside",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "outside_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "decls_in_signature.h:4:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "outside_x"},
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
                    "decls_in_signature.h:4:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "functions/decls_in_signature.h"],
                      headerInclude =
                      "functions/decls_in_signature.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "outside_y",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "decls_in_signature.h:5:7",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "outside_y"},
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
                    "decls_in_signature.h:5:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "functions/decls_in_signature.h"],
                      headerInclude =
                      "functions/decls_in_signature.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "decls_in_signature.h:3:8",
                declId = NamePair {
                  nameC = Name "outside",
                  nameHsIdent = Identifier
                    "Outside"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      [
                        "functions/decls_in_signature.h"],
                    headerInclude =
                    "functions/decls_in_signature.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Outside"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "decls_in_signature.h:4:7",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "outside_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "decls_in_signature.h:5:7",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = Identifier
                            "outside_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 32,
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
              commentOrigin = Just "outside",
              commentLocation = Just
                "decls_in_signature.h:3:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "functions/decls_in_signature.h"],
                  headerInclude =
                  "functions/decls_in_signature.h"},
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
                    "Outside",
                  structConstr = Name
                    "@NsConstr"
                    "Outside",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "outside_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "decls_in_signature.h:4:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "outside_x"},
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
                            "decls_in_signature.h:4:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "functions/decls_in_signature.h"],
                              headerInclude =
                              "functions/decls_in_signature.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "outside_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "decls_in_signature.h:5:7",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "outside_y"},
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
                            "decls_in_signature.h:5:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "functions/decls_in_signature.h"],
                              headerInclude =
                              "functions/decls_in_signature.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "decls_in_signature.h:3:8",
                        declId = NamePair {
                          nameC = Name "outside",
                          nameHsIdent = Identifier
                            "Outside"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              [
                                "functions/decls_in_signature.h"],
                            headerInclude =
                            "functions/decls_in_signature.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Outside"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "decls_in_signature.h:4:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "outside_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "decls_in_signature.h:5:7",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "outside_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
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
                      commentOrigin = Just "outside",
                      commentLocation = Just
                        "decls_in_signature.h:3:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            [
                              "functions/decls_in_signature.h"],
                          headerInclude =
                          "functions/decls_in_signature.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "outside_x")
                  (Idx 0),
                PeekCField
                  (HsStrLit "outside_y")
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
                    "Outside",
                  structConstr = Name
                    "@NsConstr"
                    "Outside",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "outside_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "decls_in_signature.h:4:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "outside_x"},
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
                            "decls_in_signature.h:4:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "functions/decls_in_signature.h"],
                              headerInclude =
                              "functions/decls_in_signature.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "outside_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "decls_in_signature.h:5:7",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "outside_y"},
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
                            "decls_in_signature.h:5:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "functions/decls_in_signature.h"],
                              headerInclude =
                              "functions/decls_in_signature.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "decls_in_signature.h:3:8",
                        declId = NamePair {
                          nameC = Name "outside",
                          nameHsIdent = Identifier
                            "Outside"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              [
                                "functions/decls_in_signature.h"],
                            headerInclude =
                            "functions/decls_in_signature.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Outside"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "decls_in_signature.h:4:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "outside_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "decls_in_signature.h:5:7",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "outside_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
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
                      commentOrigin = Just "outside",
                      commentLocation = Just
                        "decls_in_signature.h:3:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            [
                              "functions/decls_in_signature.h"],
                          headerInclude =
                          "functions/decls_in_signature.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "outside_x")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "outside_y")
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
        "Outside",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Outside",
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
              "Outside"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "outside_x",
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
              "Outside"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "outside_x",
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
            (Name
              "@NsTypeConstr"
              "Outside"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "outside_y",
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
              "Outside"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "outside_y",
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
        "Named_struct",
      structConstr = Name
        "@NsConstr"
        "Named_struct",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "named_struct_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "decls_in_signature.h:17:35",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "named_struct_x"},
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
                "decls_in_signature.h:17:35",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "functions/decls_in_signature.h"],
                  headerInclude =
                  "functions/decls_in_signature.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "named_struct_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "decls_in_signature.h:17:42",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier
                    "named_struct_y"},
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
                "decls_in_signature.h:17:42",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "functions/decls_in_signature.h"],
                  headerInclude =
                  "functions/decls_in_signature.h"},
              commentChildren = []}}],
      structOrigin =
      Just
        Decl {
          declInfo =
          DeclInfo {
            declLoc =
            "decls_in_signature.h:17:16",
            declId = NamePair {
              nameC = Name "named_struct",
              nameHsIdent = Identifier
                "Named_struct"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  [
                    "functions/decls_in_signature.h"],
                headerInclude =
                "functions/decls_in_signature.h"},
            declComment =
            Just
              (Comment
                [
                  Paragraph
                    [TextContent "Error cases"],
                  Paragraph
                    [
                      TextContent
                        "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                      TextContent
                        "and the edge cases below)."]])},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name
                  "@NsConstr"
                  "Named_struct"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "decls_in_signature.h:17:35",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "named_struct_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "decls_in_signature.h:17:42",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "named_struct_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec {
            declSpecC = Nothing,
            declSpecHs = Nothing}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment =
      Just
        Comment {
          commentTitle = Just
            [TextContent "Error cases"],
          commentOrigin = Just
            "named_struct",
          commentLocation = Just
            "decls_in_signature.h:17:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "functions/decls_in_signature.h"],
              headerInclude =
              "functions/decls_in_signature.h"},
          commentChildren =
          [
            Paragraph
              [
                TextContent
                  "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                TextContent
                  "and the edge cases below)."]]}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Named_struct",
          structConstr = Name
            "@NsConstr"
            "Named_struct",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "named_struct_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "decls_in_signature.h:17:35",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "named_struct_x"},
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
                    "decls_in_signature.h:17:35",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "functions/decls_in_signature.h"],
                      headerInclude =
                      "functions/decls_in_signature.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "named_struct_y",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "decls_in_signature.h:17:42",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "named_struct_y"},
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
                    "decls_in_signature.h:17:42",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "functions/decls_in_signature.h"],
                      headerInclude =
                      "functions/decls_in_signature.h"},
                  commentChildren = []}}],
          structOrigin =
          Just
            Decl {
              declInfo =
              DeclInfo {
                declLoc =
                "decls_in_signature.h:17:16",
                declId = NamePair {
                  nameC = Name "named_struct",
                  nameHsIdent = Identifier
                    "Named_struct"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      [
                        "functions/decls_in_signature.h"],
                    headerInclude =
                    "functions/decls_in_signature.h"},
                declComment =
                Just
                  (Comment
                    [
                      Paragraph
                        [TextContent "Error cases"],
                      Paragraph
                        [
                          TextContent
                            "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                          TextContent
                            "and the edge cases below)."]])},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name
                      "@NsConstr"
                      "Named_struct"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "decls_in_signature.h:17:35",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "named_struct_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "decls_in_signature.h:17:42",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = Identifier
                            "named_struct_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec {
                declSpecC = Nothing,
                declSpecHs = Nothing}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment =
          Just
            Comment {
              commentTitle = Just
                [TextContent "Error cases"],
              commentOrigin = Just
                "named_struct",
              commentLocation = Just
                "decls_in_signature.h:17:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "functions/decls_in_signature.h"],
                  headerInclude =
                  "functions/decls_in_signature.h"},
              commentChildren =
              [
                Paragraph
                  [
                    TextContent
                      "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                    TextContent
                      "and the edge cases below)."]]}}
        StorableInstance {
          storableSizeOf = 8,
          storableAlignment = 4,
          storablePeek =
          Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Named_struct",
                  structConstr = Name
                    "@NsConstr"
                    "Named_struct",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "named_struct_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "decls_in_signature.h:17:35",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "named_struct_x"},
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
                            "decls_in_signature.h:17:35",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "functions/decls_in_signature.h"],
                              headerInclude =
                              "functions/decls_in_signature.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "named_struct_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "decls_in_signature.h:17:42",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "named_struct_y"},
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
                            "decls_in_signature.h:17:42",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "functions/decls_in_signature.h"],
                              headerInclude =
                              "functions/decls_in_signature.h"},
                          commentChildren = []}}],
                  structOrigin =
                  Just
                    Decl {
                      declInfo =
                      DeclInfo {
                        declLoc =
                        "decls_in_signature.h:17:16",
                        declId = NamePair {
                          nameC = Name "named_struct",
                          nameHsIdent = Identifier
                            "Named_struct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              [
                                "functions/decls_in_signature.h"],
                            headerInclude =
                            "functions/decls_in_signature.h"},
                        declComment =
                        Just
                          (Comment
                            [
                              Paragraph
                                [TextContent "Error cases"],
                              Paragraph
                                [
                                  TextContent
                                    "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                                  TextContent
                                    "and the edge cases below)."]])},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Named_struct"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "decls_in_signature.h:17:35",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "named_struct_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "decls_in_signature.h:17:42",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "named_struct_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment =
                  Just
                    Comment {
                      commentTitle = Just
                        [TextContent "Error cases"],
                      commentOrigin = Just
                        "named_struct",
                      commentLocation = Just
                        "decls_in_signature.h:17:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            [
                              "functions/decls_in_signature.h"],
                          headerInclude =
                          "functions/decls_in_signature.h"},
                      commentChildren =
                      [
                        Paragraph
                          [
                            TextContent
                              "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                            TextContent
                              "and the edge cases below)."]]}})
              [
                PeekCField
                  (HsStrLit "named_struct_x")
                  (Idx 0),
                PeekCField
                  (HsStrLit "named_struct_y")
                  (Idx 0)]),
          storablePoke =
          Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Named_struct",
                  structConstr = Name
                    "@NsConstr"
                    "Named_struct",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "named_struct_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "decls_in_signature.h:17:35",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "named_struct_x"},
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
                            "decls_in_signature.h:17:35",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "functions/decls_in_signature.h"],
                              headerInclude =
                              "functions/decls_in_signature.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "named_struct_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "decls_in_signature.h:17:42",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "named_struct_y"},
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
                            "decls_in_signature.h:17:42",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "functions/decls_in_signature.h"],
                              headerInclude =
                              "functions/decls_in_signature.h"},
                          commentChildren = []}}],
                  structOrigin =
                  Just
                    Decl {
                      declInfo =
                      DeclInfo {
                        declLoc =
                        "decls_in_signature.h:17:16",
                        declId = NamePair {
                          nameC = Name "named_struct",
                          nameHsIdent = Identifier
                            "Named_struct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              [
                                "functions/decls_in_signature.h"],
                            headerInclude =
                            "functions/decls_in_signature.h"},
                        declComment =
                        Just
                          (Comment
                            [
                              Paragraph
                                [TextContent "Error cases"],
                              Paragraph
                                [
                                  TextContent
                                    "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                                  TextContent
                                    "and the edge cases below)."]])},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Named_struct"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "decls_in_signature.h:17:35",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "named_struct_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "decls_in_signature.h:17:42",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "named_struct_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment =
                  Just
                    Comment {
                      commentTitle = Just
                        [TextContent "Error cases"],
                      commentOrigin = Just
                        "named_struct",
                      commentLocation = Just
                        "decls_in_signature.h:17:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            [
                              "functions/decls_in_signature.h"],
                          headerInclude =
                          "functions/decls_in_signature.h"},
                      commentChildren =
                      [
                        Paragraph
                          [
                            TextContent
                              "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                            TextContent
                              "and the edge cases below)."]]}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "named_struct_x")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "named_struct_y")
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
        "Named_struct",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Named_struct",
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
              "Named_struct"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "named_struct_x",
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
              "Named_struct"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "named_struct_x",
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
            (Name
              "@NsTypeConstr"
              "Named_struct"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "named_struct_y",
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
              "Named_struct"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "named_struct_y",
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
        "Named_union",
      newtypeConstr = Name
        "@NsConstr"
        "Named_union",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Named_union",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "decls_in_signature.h:20:15",
          declId = NamePair {
            nameC = Name "named_union",
            nameHsIdent = Identifier
              "Named_union"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "functions/decls_in_signature.h"],
              headerInclude =
              "functions/decls_in_signature.h"},
          declComment = Nothing},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Named_union",
              newtypeField = Name
                "@NsVar"
                "un_Named_union"},
            unionSizeof = 4,
            unionAlignment = 4,
            unionFields = [
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc =
                  "decls_in_signature.h:20:33",
                  fieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = Identifier
                      "named_union_x"},
                  fieldComment = Nothing},
                unionFieldType = TypePrim
                  (PrimIntegral PrimInt Signed)},
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc =
                  "decls_in_signature.h:20:41",
                  fieldName = NamePair {
                    nameC = Name "y",
                    nameHsIdent = Identifier
                      "named_union_y"},
                  fieldComment = Nothing},
                unionFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))}]},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "named_union",
          commentLocation = Just
            "decls_in_signature.h:20:15",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "functions/decls_in_signature.h"],
              headerInclude =
              "functions/decls_in_signature.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveVia
        (HsSizedByteArray 4 4),
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Named_union",
      deriveInstanceComment =
      Nothing},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = Name
        "@NsVar"
        "get_named_union_x",
      unionGetterType = HsPrimType
        HsPrimCInt,
      unionGetterConstr = Name
        "@NsTypeConstr"
        "Named_union",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "x",
          commentLocation = Just
            "decls_in_signature.h:20:33",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "functions/decls_in_signature.h"],
              headerInclude =
              "functions/decls_in_signature.h"},
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_named_union_x"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = Name
        "@NsVar"
        "set_named_union_x",
      unionSetterType = HsPrimType
        HsPrimCInt,
      unionSetterConstr = Name
        "@NsTypeConstr"
        "Named_union",
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
                  "get_named_union_x"]]}},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = Name
        "@NsVar"
        "get_named_union_y",
      unionGetterType = HsPrimType
        HsPrimCChar,
      unionGetterConstr = Name
        "@NsTypeConstr"
        "Named_union",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "y",
          commentLocation = Just
            "decls_in_signature.h:20:41",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "functions/decls_in_signature.h"],
              headerInclude =
              "functions/decls_in_signature.h"},
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_named_union_y"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = Name
        "@NsVar"
        "set_named_union_y",
      unionSetterType = HsPrimType
        HsPrimCChar,
      unionSetterConstr = Name
        "@NsTypeConstr"
        "Named_union",
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
                  "get_named_union_y"]]}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Named_union"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "named_union_x",
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
              "Named_union"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "named_union_x",
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
            (Name
              "@NsTypeConstr"
              "Named_union"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "named_union_y",
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
              "Named_union"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "named_union_y",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCChar,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "normal_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "ptr_to_opaque"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Opaque")),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name
              "@NsVar"
              "ptr_to_defined"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Outside")),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "by_value"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Outside")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsdecls_in_signature_001a08d4459ec455",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_functionsdecls_in_signature_001a08d4459ec455 (\n",
              "  struct opaque *arg1,\n",
              "  struct outside *arg2,\n",
              "  struct outside *arg3\n",
              ")\n",
              "{\n",
              "  normal(arg1, arg2, *arg3);\n",
              "}"],
          capiWrapperImport =
          "functions/decls_in_signature.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "ptr_to_opaque",
                  nameHsIdent = Identifier
                    "ptr_to_opaque"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "opaque",
                    nameHsIdent = Identifier
                      "Opaque"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "ptr_to_defined",
                  nameHsIdent = Identifier
                    "ptr_to_defined"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "outside",
                    nameHsIdent = Identifier
                      "Outside"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "by_value",
                  nameHsIdent = Identifier
                    "by_value"})
              (TypeStruct
                NamePair {
                  nameC = Name "outside",
                  nameHsIdent = Identifier
                    "Outside"}
                NameOriginInSource)],
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
              Identifier "normal"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "normal",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "ptr_to_opaque"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Opaque")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "ptr_to_opaque",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name
              "@NsVar"
              "ptr_to_defined"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Outside")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "ptr_to_defined",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "by_value"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Outside"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "by_value",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimUnit),
      functionDeclBody =
      `ELam "x" (ELam "x" (ELam "x" (EApp (EApp (EGlobal CAPI_with) (EBound 0)) (ELam "y" (EApp (EApp (EApp (EFree "normal_wrapper") (EBound 3)) (EBound 2)) (EBound 0))))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "ptr_to_opaque",
                  nameHsIdent = Identifier
                    "ptr_to_opaque"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "opaque",
                    nameHsIdent = Identifier
                      "Opaque"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "ptr_to_defined",
                  nameHsIdent = Identifier
                    "ptr_to_defined"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "outside",
                    nameHsIdent = Identifier
                      "Outside"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "by_value",
                  nameHsIdent = Identifier
                    "by_value"})
              (TypeStruct
                NamePair {
                  nameC = Name "outside",
                  nameHsIdent = Identifier
                    "Outside"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      functionDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "normal",
          commentLocation = Just
            "decls_in_signature.h:7:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "functions/decls_in_signature.h"],
              headerInclude =
              "functions/decls_in_signature.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "f1_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "arg"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Named_struct")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsdecls_in_signature_a2f84d2570ef3892",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_functionsdecls_in_signature_a2f84d2570ef3892 (\n",
              "  struct named_struct *arg1\n",
              ")\n",
              "{\n",
              "  f1(*arg1);\n",
              "}"],
          capiWrapperImport =
          "functions/decls_in_signature.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg",
                  nameHsIdent = Identifier "arg"})
              (TypeStruct
                NamePair {
                  nameC = Name "named_struct",
                  nameHsIdent = Identifier
                    "Named_struct"}
                NameOriginInSource)],
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
              Identifier "f1"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "f1",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "arg"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Named_struct"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimUnit),
      functionDeclBody =
      `ELam "x" (EApp (EApp (EGlobal CAPI_with) (EBound 0)) (ELam "y" (EApp (EFree "f1_wrapper") (EBound 0))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg",
                  nameHsIdent = Identifier "arg"})
              (TypeStruct
                NamePair {
                  nameC = Name "named_struct",
                  nameHsIdent = Identifier
                    "Named_struct"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      functionDeclComment =
      Just
        Comment {
          commentTitle = Just
            [TextContent "Error cases"],
          commentOrigin = Just "f1",
          commentLocation = Just
            "decls_in_signature.h:17:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "functions/decls_in_signature.h"],
              headerInclude =
              "functions/decls_in_signature.h"},
          commentChildren =
          [
            Paragraph
              [
                TextContent
                  "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                TextContent
                  "and the edge cases below)."]]}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "f2_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "arg"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Named_union")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsdecls_in_signature_1d043de05a457e90",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_functionsdecls_in_signature_1d043de05a457e90 (\n",
              "  union named_union *arg1\n",
              ")\n",
              "{\n",
              "  f2(*arg1);\n",
              "}"],
          capiWrapperImport =
          "functions/decls_in_signature.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg",
                  nameHsIdent = Identifier "arg"})
              (TypeUnion
                NamePair {
                  nameC = Name "named_union",
                  nameHsIdent = Identifier
                    "Named_union"}
                NameOriginInSource)],
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
              Identifier "f2"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "f2",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "arg"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Named_union"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimUnit),
      functionDeclBody =
      `ELam "x" (EApp (EApp (EGlobal CAPI_with) (EBound 0)) (ELam "y" (EApp (EFree "f2_wrapper") (EBound 0))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg",
                  nameHsIdent = Identifier "arg"})
              (TypeUnion
                NamePair {
                  nameC = Name "named_union",
                  nameHsIdent = Identifier
                    "Named_union"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      functionDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "f2",
          commentLocation = Just
            "decls_in_signature.h:20:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "functions/decls_in_signature.h"],
              headerInclude =
              "functions/decls_in_signature.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "normal_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "ptr_to_opaque"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Opaque")),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name
              "@NsVar"
              "ptr_to_defined"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Outside")),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "by_value"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Outside")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsdecls_in_signature_35c28995abc46de4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_functionsdecls_in_signature_35c28995abc46de4 (\n",
              "  struct opaque *arg1,\n",
              "  struct outside *arg2,\n",
              "  struct outside *arg3\n",
              ")\n",
              "{\n",
              "  normal(arg1, arg2, *arg3);\n",
              "}"],
          capiWrapperImport =
          "functions/decls_in_signature.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "ptr_to_opaque",
                  nameHsIdent = Identifier
                    "ptr_to_opaque"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "opaque",
                    nameHsIdent = Identifier
                      "Opaque"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "ptr_to_defined",
                  nameHsIdent = Identifier
                    "ptr_to_defined"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "outside",
                    nameHsIdent = Identifier
                      "Outside"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "by_value",
                  nameHsIdent = Identifier
                    "by_value"})
              (TypeStruct
                NamePair {
                  nameC = Name "outside",
                  nameHsIdent = Identifier
                    "Outside"}
                NameOriginInSource)],
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
              Identifier "normal"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "normal",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "ptr_to_opaque"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Opaque")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "ptr_to_opaque",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name
              "@NsVar"
              "ptr_to_defined"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Outside")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "ptr_to_defined",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "by_value"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Outside"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "by_value",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimUnit),
      functionDeclBody =
      `ELam "x" (ELam "x" (ELam "x" (EApp (EApp (EGlobal CAPI_with) (EBound 0)) (ELam "y" (EApp (EApp (EApp (EFree "normal_wrapper") (EBound 3)) (EBound 2)) (EBound 0))))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "ptr_to_opaque",
                  nameHsIdent = Identifier
                    "ptr_to_opaque"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "opaque",
                    nameHsIdent = Identifier
                      "Opaque"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "ptr_to_defined",
                  nameHsIdent = Identifier
                    "ptr_to_defined"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "outside",
                    nameHsIdent = Identifier
                      "Outside"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "by_value",
                  nameHsIdent = Identifier
                    "by_value"})
              (TypeStruct
                NamePair {
                  nameC = Name "outside",
                  nameHsIdent = Identifier
                    "Outside"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      functionDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "normal",
          commentLocation = Just
            "decls_in_signature.h:7:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "functions/decls_in_signature.h"],
              headerInclude =
              "functions/decls_in_signature.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "f1_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "arg"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Named_struct")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsdecls_in_signature_c1788128a5b1c813",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_functionsdecls_in_signature_c1788128a5b1c813 (\n",
              "  struct named_struct *arg1\n",
              ")\n",
              "{\n",
              "  f1(*arg1);\n",
              "}"],
          capiWrapperImport =
          "functions/decls_in_signature.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg",
                  nameHsIdent = Identifier "arg"})
              (TypeStruct
                NamePair {
                  nameC = Name "named_struct",
                  nameHsIdent = Identifier
                    "Named_struct"}
                NameOriginInSource)],
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
              Identifier "f1"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "f1",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "arg"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Named_struct"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimUnit),
      functionDeclBody =
      `ELam "x" (EApp (EApp (EGlobal CAPI_with) (EBound 0)) (ELam "y" (EApp (EFree "f1_wrapper") (EBound 0))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg",
                  nameHsIdent = Identifier "arg"})
              (TypeStruct
                NamePair {
                  nameC = Name "named_struct",
                  nameHsIdent = Identifier
                    "Named_struct"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      functionDeclComment =
      Just
        Comment {
          commentTitle = Just
            [TextContent "Error cases"],
          commentOrigin = Just "f1",
          commentLocation = Just
            "decls_in_signature.h:17:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "functions/decls_in_signature.h"],
              headerInclude =
              "functions/decls_in_signature.h"},
          commentChildren =
          [
            Paragraph
              [
                TextContent
                  "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                TextContent
                  "and the edge cases below)."]]}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "f2_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "arg"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Named_union")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_functionsdecls_in_signature_14361e995fb5684a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_functionsdecls_in_signature_14361e995fb5684a (\n",
              "  union named_union *arg1\n",
              ")\n",
              "{\n",
              "  f2(*arg1);\n",
              "}"],
          capiWrapperImport =
          "functions/decls_in_signature.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg",
                  nameHsIdent = Identifier "arg"})
              (TypeUnion
                NamePair {
                  nameC = Name "named_union",
                  nameHsIdent = Identifier
                    "Named_union"}
                NameOriginInSource)],
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
              Identifier "f2"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "f2",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "arg"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Named_union"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimUnit),
      functionDeclBody =
      `ELam "x" (EApp (EApp (EGlobal CAPI_with) (EBound 0)) (ELam "y" (EApp (EFree "f2_wrapper") (EBound 0))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg",
                  nameHsIdent = Identifier "arg"})
              (TypeUnion
                NamePair {
                  nameC = Name "named_union",
                  nameHsIdent = Identifier
                    "Named_union"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      functionDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "f2",
          commentLocation = Just
            "decls_in_signature.h:20:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "functions/decls_in_signature.h"],
              headerInclude =
              "functions/decls_in_signature.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_functionsdecls_in_signature_b040d51578b7b05e",
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
                    "Opaque")))
              (HsFun
                (HsPtr
                  (HsTypRef
                    (Name
                      "@NsTypeConstr"
                      "Outside")))
                (HsFun
                  (HsTypRef
                    (Name
                      "@NsTypeConstr"
                      "Outside"))
                  (HsIO
                    (HsPrimType HsPrimUnit))))))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsdecls_in_signature_b040d51578b7b05e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_normal_ptr */\n",
              "__attribute__ ((const))\n",
              "void (*hs_bindgen_test_functionsdecls_in_signature_b040d51578b7b05e (void)) (\n",
              "  struct opaque *arg1,\n",
              "  struct outside *arg2,\n",
              "  struct outside arg3\n",
              ")\n",
              "{\n",
              "  return &normal;\n",
              "}"],
          capiWrapperImport =
          "functions/decls_in_signature.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name "opaque",
                  nameHsIdent = Identifier
                    "Opaque"}
                NameOriginInSource),
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name "outside",
                  nameHsIdent = Identifier
                    "Outside"}
                NameOriginInSource),
            TypeStruct
              NamePair {
                nameC = Name "outside",
                nameHsIdent = Identifier
                  "Outside"}
              NameOriginInSource]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_functionsdecls_in_signature_5469bdc0395f86c1",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Named_struct"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsdecls_in_signature_5469bdc0395f86c1",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_f1_ptr */\n",
              "__attribute__ ((const))\n",
              "void (*hs_bindgen_test_functionsdecls_in_signature_5469bdc0395f86c1 (void)) (\n",
              "  struct named_struct arg1\n",
              ")\n",
              "{\n",
              "  return &f1;\n",
              "}"],
          capiWrapperImport =
          "functions/decls_in_signature.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeStruct
              NamePair {
                nameC = Name "named_struct",
                nameHsIdent = Identifier
                  "Named_struct"}
              NameOriginInSource]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_functionsdecls_in_signature_490ca7e8c8282a69",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Named_union"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_functionsdecls_in_signature_490ca7e8c8282a69",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_f2_ptr */\n",
              "__attribute__ ((const))\n",
              "void (*hs_bindgen_test_functionsdecls_in_signature_490ca7e8c8282a69 (void)) (\n",
              "  union named_union arg1\n",
              ")\n",
              "{\n",
              "  return &f2;\n",
              "}"],
          capiWrapperImport =
          "functions/decls_in_signature.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeUnion
              NamePair {
                nameC = Name "named_union",
                nameHsIdent = Identifier
                  "Named_union"}
              NameOriginInSource]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
