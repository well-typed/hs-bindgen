[
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Foo",
      structConstr = Name
        "@NsConstr"
        "Foo",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "foo_c",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "attributes.h:11:10",
                fieldName = NamePair {
                  nameC = Name "c",
                  nameHsIdent = Identifier
                    "foo_c"},
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
              commentOrigin = Just "c",
              commentLocation = Just
                "attributes.h:11:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["attributes.h"],
                  headerInclude = "attributes.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "foo_i",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "attributes.h:12:10",
                fieldName = NamePair {
                  nameC = Name "i",
                  nameHsIdent = Identifier
                    "foo_i"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 8,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "i",
              commentLocation = Just
                "attributes.h:12:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["attributes.h"],
                  headerInclude = "attributes.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "attributes.h:10:36",
            declId = NamePair {
              nameC = Name "foo",
              nameHsIdent = Identifier "Foo"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["attributes.h"],
                headerInclude = "attributes.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Foo"),
              structSizeof = 5,
              structAlignment = 1,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "attributes.h:11:10",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = Identifier
                        "foo_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "attributes.h:12:10",
                    fieldName = NamePair {
                      nameC = Name "i",
                      nameHsIdent = Identifier
                        "foo_i"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 8,
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
          commentOrigin = Just "foo",
          commentLocation = Just
            "attributes.h:10:36",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["attributes.h"],
              headerInclude = "attributes.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Foo",
          structConstr = Name
            "@NsConstr"
            "Foo",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "foo_c",
              fieldType = HsPrimType
                HsPrimCChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "attributes.h:11:10",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = Identifier
                        "foo_c"},
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
                  commentOrigin = Just "c",
                  commentLocation = Just
                    "attributes.h:11:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["attributes.h"],
                      headerInclude = "attributes.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "foo_i",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "attributes.h:12:10",
                    fieldName = NamePair {
                      nameC = Name "i",
                      nameHsIdent = Identifier
                        "foo_i"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 8,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "i",
                  commentLocation = Just
                    "attributes.h:12:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["attributes.h"],
                      headerInclude = "attributes.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "attributes.h:10:36",
                declId = NamePair {
                  nameC = Name "foo",
                  nameHsIdent = Identifier "Foo"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["attributes.h"],
                    headerInclude = "attributes.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Foo"),
                  structSizeof = 5,
                  structAlignment = 1,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "attributes.h:11:10",
                        fieldName = NamePair {
                          nameC = Name "c",
                          nameHsIdent = Identifier
                            "foo_c"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "attributes.h:12:10",
                        fieldName = NamePair {
                          nameC = Name "i",
                          nameHsIdent = Identifier
                            "foo_i"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 8,
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
              commentOrigin = Just "foo",
              commentLocation = Just
                "attributes.h:10:36",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["attributes.h"],
                  headerInclude = "attributes.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 5,
          storableAlignment = 1,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Foo",
                  structConstr = Name
                    "@NsConstr"
                    "Foo",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "foo_c",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "attributes.h:11:10",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = Identifier
                                "foo_c"},
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
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "attributes.h:11:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["attributes.h"],
                              headerInclude = "attributes.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "foo_i",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "attributes.h:12:10",
                            fieldName = NamePair {
                              nameC = Name "i",
                              nameHsIdent = Identifier
                                "foo_i"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 8,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "i",
                          commentLocation = Just
                            "attributes.h:12:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["attributes.h"],
                              headerInclude = "attributes.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "attributes.h:10:36",
                        declId = NamePair {
                          nameC = Name "foo",
                          nameHsIdent = Identifier "Foo"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["attributes.h"],
                            headerInclude = "attributes.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Foo"),
                          structSizeof = 5,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "attributes.h:11:10",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = Identifier
                                    "foo_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "attributes.h:12:10",
                                fieldName = NamePair {
                                  nameC = Name "i",
                                  nameHsIdent = Identifier
                                    "foo_i"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 8,
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
                      commentOrigin = Just "foo",
                      commentLocation = Just
                        "attributes.h:10:36",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["attributes.h"],
                          headerInclude = "attributes.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "foo_c")
                  (Idx 0),
                PeekCField
                  (HsStrLit "foo_i")
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
                    "Foo",
                  structConstr = Name
                    "@NsConstr"
                    "Foo",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "foo_c",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "attributes.h:11:10",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = Identifier
                                "foo_c"},
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
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "attributes.h:11:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["attributes.h"],
                              headerInclude = "attributes.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "foo_i",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "attributes.h:12:10",
                            fieldName = NamePair {
                              nameC = Name "i",
                              nameHsIdent = Identifier
                                "foo_i"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 8,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "i",
                          commentLocation = Just
                            "attributes.h:12:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["attributes.h"],
                              headerInclude = "attributes.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "attributes.h:10:36",
                        declId = NamePair {
                          nameC = Name "foo",
                          nameHsIdent = Identifier "Foo"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["attributes.h"],
                            headerInclude = "attributes.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Foo"),
                          structSizeof = 5,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "attributes.h:11:10",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = Identifier
                                    "foo_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "attributes.h:12:10",
                                fieldName = NamePair {
                                  nameC = Name "i",
                                  nameHsIdent = Identifier
                                    "foo_i"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 8,
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
                      commentOrigin = Just "foo",
                      commentLocation = Just
                        "attributes.h:10:36",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["attributes.h"],
                          headerInclude = "attributes.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "foo_c")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "foo_i")
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
        "Foo",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Foo"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "foo_c",
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
            (Name "@NsTypeConstr" "Foo"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "foo_c",
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
            (Name "@NsTypeConstr" "Foo"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "foo_i",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCInt,
          hasCFieldInstanceFieldOffset =
          1},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Foo"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "foo_i",
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
        "Bar",
      structConstr = Name
        "@NsConstr"
        "Bar",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "bar_c",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "attributes.h:17:10",
                fieldName = NamePair {
                  nameC = Name "c",
                  nameHsIdent = Identifier
                    "bar_c"},
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
              commentOrigin = Just "c",
              commentLocation = Just
                "attributes.h:17:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["attributes.h"],
                  headerInclude = "attributes.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "bar_i",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "attributes.h:18:10",
                fieldName = NamePair {
                  nameC = Name "i",
                  nameHsIdent = Identifier
                    "bar_i"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 8,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "i",
              commentLocation = Just
                "attributes.h:18:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["attributes.h"],
                  headerInclude = "attributes.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "attributes.h:16:15",
            declId = NamePair {
              nameC = Name "bar",
              nameHsIdent = Identifier "Bar"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["attributes.h"],
                headerInclude = "attributes.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Bar"),
              structSizeof = 5,
              structAlignment = 1,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "attributes.h:17:10",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = Identifier
                        "bar_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "attributes.h:18:10",
                    fieldName = NamePair {
                      nameC = Name "i",
                      nameHsIdent = Identifier
                        "bar_i"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 8,
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
          commentOrigin = Just "bar",
          commentLocation = Just
            "attributes.h:16:15",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["attributes.h"],
              headerInclude = "attributes.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Bar",
          structConstr = Name
            "@NsConstr"
            "Bar",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "bar_c",
              fieldType = HsPrimType
                HsPrimCChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "attributes.h:17:10",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = Identifier
                        "bar_c"},
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
                  commentOrigin = Just "c",
                  commentLocation = Just
                    "attributes.h:17:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["attributes.h"],
                      headerInclude = "attributes.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "bar_i",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "attributes.h:18:10",
                    fieldName = NamePair {
                      nameC = Name "i",
                      nameHsIdent = Identifier
                        "bar_i"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 8,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "i",
                  commentLocation = Just
                    "attributes.h:18:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["attributes.h"],
                      headerInclude = "attributes.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "attributes.h:16:15",
                declId = NamePair {
                  nameC = Name "bar",
                  nameHsIdent = Identifier "Bar"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["attributes.h"],
                    headerInclude = "attributes.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Bar"),
                  structSizeof = 5,
                  structAlignment = 1,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "attributes.h:17:10",
                        fieldName = NamePair {
                          nameC = Name "c",
                          nameHsIdent = Identifier
                            "bar_c"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "attributes.h:18:10",
                        fieldName = NamePair {
                          nameC = Name "i",
                          nameHsIdent = Identifier
                            "bar_i"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 8,
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
              commentOrigin = Just "bar",
              commentLocation = Just
                "attributes.h:16:15",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["attributes.h"],
                  headerInclude = "attributes.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 5,
          storableAlignment = 1,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Bar",
                  structConstr = Name
                    "@NsConstr"
                    "Bar",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "bar_c",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "attributes.h:17:10",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = Identifier
                                "bar_c"},
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
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "attributes.h:17:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["attributes.h"],
                              headerInclude = "attributes.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "bar_i",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "attributes.h:18:10",
                            fieldName = NamePair {
                              nameC = Name "i",
                              nameHsIdent = Identifier
                                "bar_i"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 8,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "i",
                          commentLocation = Just
                            "attributes.h:18:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["attributes.h"],
                              headerInclude = "attributes.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "attributes.h:16:15",
                        declId = NamePair {
                          nameC = Name "bar",
                          nameHsIdent = Identifier "Bar"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["attributes.h"],
                            headerInclude = "attributes.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Bar"),
                          structSizeof = 5,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "attributes.h:17:10",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = Identifier
                                    "bar_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "attributes.h:18:10",
                                fieldName = NamePair {
                                  nameC = Name "i",
                                  nameHsIdent = Identifier
                                    "bar_i"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 8,
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
                      commentOrigin = Just "bar",
                      commentLocation = Just
                        "attributes.h:16:15",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["attributes.h"],
                          headerInclude = "attributes.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "bar_c")
                  (Idx 0),
                PeekCField
                  (HsStrLit "bar_i")
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
                    "Bar",
                  structConstr = Name
                    "@NsConstr"
                    "Bar",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "bar_c",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "attributes.h:17:10",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = Identifier
                                "bar_c"},
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
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "attributes.h:17:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["attributes.h"],
                              headerInclude = "attributes.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "bar_i",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "attributes.h:18:10",
                            fieldName = NamePair {
                              nameC = Name "i",
                              nameHsIdent = Identifier
                                "bar_i"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 8,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "i",
                          commentLocation = Just
                            "attributes.h:18:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["attributes.h"],
                              headerInclude = "attributes.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "attributes.h:16:15",
                        declId = NamePair {
                          nameC = Name "bar",
                          nameHsIdent = Identifier "Bar"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["attributes.h"],
                            headerInclude = "attributes.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Bar"),
                          structSizeof = 5,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "attributes.h:17:10",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = Identifier
                                    "bar_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "attributes.h:18:10",
                                fieldName = NamePair {
                                  nameC = Name "i",
                                  nameHsIdent = Identifier
                                    "bar_i"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 8,
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
                      commentOrigin = Just "bar",
                      commentLocation = Just
                        "attributes.h:16:15",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["attributes.h"],
                          headerInclude = "attributes.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "bar_c")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "bar_i")
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
        "Bar",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Bar",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Bar"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "bar_c",
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
            (Name "@NsTypeConstr" "Bar"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "bar_c",
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
            (Name "@NsTypeConstr" "Bar"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "bar_i",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCInt,
          hasCFieldInstanceFieldOffset =
          1},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Bar"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "bar_i",
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
        "Baz",
      structConstr = Name
        "@NsConstr"
        "Baz",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "baz_c",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "attributes.h:23:10",
                fieldName = NamePair {
                  nameC = Name "c",
                  nameHsIdent = Identifier
                    "baz_c"},
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
              commentOrigin = Just "c",
              commentLocation = Just
                "attributes.h:23:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["attributes.h"],
                  headerInclude = "attributes.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "baz_i",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "attributes.h:24:10",
                fieldName = NamePair {
                  nameC = Name "i",
                  nameHsIdent = Identifier
                    "baz_i"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 8,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "i",
              commentLocation = Just
                "attributes.h:24:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["attributes.h"],
                  headerInclude = "attributes.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "attributes.h:22:9",
            declId = NamePair {
              nameC = Name "baz",
              nameHsIdent = Identifier "Baz"},
            declOrigin = NameOriginGenerated
              (AnonId "attributes.h:22:9"),
            declAliases = [Name "baz"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["attributes.h"],
                headerInclude = "attributes.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Baz"),
              structSizeof = 5,
              structAlignment = 1,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "attributes.h:23:10",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = Identifier
                        "baz_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "attributes.h:24:10",
                    fieldName = NamePair {
                      nameC = Name "i",
                      nameHsIdent = Identifier
                        "baz_i"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 8,
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
            "attributes.h:22:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["attributes.h"],
              headerInclude = "attributes.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Baz",
          structConstr = Name
            "@NsConstr"
            "Baz",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "baz_c",
              fieldType = HsPrimType
                HsPrimCChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "attributes.h:23:10",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = Identifier
                        "baz_c"},
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
                  commentOrigin = Just "c",
                  commentLocation = Just
                    "attributes.h:23:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["attributes.h"],
                      headerInclude = "attributes.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "baz_i",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "attributes.h:24:10",
                    fieldName = NamePair {
                      nameC = Name "i",
                      nameHsIdent = Identifier
                        "baz_i"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 8,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "i",
                  commentLocation = Just
                    "attributes.h:24:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["attributes.h"],
                      headerInclude = "attributes.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "attributes.h:22:9",
                declId = NamePair {
                  nameC = Name "baz",
                  nameHsIdent = Identifier "Baz"},
                declOrigin = NameOriginGenerated
                  (AnonId "attributes.h:22:9"),
                declAliases = [Name "baz"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["attributes.h"],
                    headerInclude = "attributes.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Baz"),
                  structSizeof = 5,
                  structAlignment = 1,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "attributes.h:23:10",
                        fieldName = NamePair {
                          nameC = Name "c",
                          nameHsIdent = Identifier
                            "baz_c"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "attributes.h:24:10",
                        fieldName = NamePair {
                          nameC = Name "i",
                          nameHsIdent = Identifier
                            "baz_i"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 8,
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
                "attributes.h:22:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["attributes.h"],
                  headerInclude = "attributes.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 5,
          storableAlignment = 1,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Baz",
                  structConstr = Name
                    "@NsConstr"
                    "Baz",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "baz_c",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "attributes.h:23:10",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = Identifier
                                "baz_c"},
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
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "attributes.h:23:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["attributes.h"],
                              headerInclude = "attributes.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "baz_i",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "attributes.h:24:10",
                            fieldName = NamePair {
                              nameC = Name "i",
                              nameHsIdent = Identifier
                                "baz_i"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 8,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "i",
                          commentLocation = Just
                            "attributes.h:24:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["attributes.h"],
                              headerInclude = "attributes.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "attributes.h:22:9",
                        declId = NamePair {
                          nameC = Name "baz",
                          nameHsIdent = Identifier "Baz"},
                        declOrigin = NameOriginGenerated
                          (AnonId "attributes.h:22:9"),
                        declAliases = [Name "baz"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["attributes.h"],
                            headerInclude = "attributes.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Baz"),
                          structSizeof = 5,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "attributes.h:23:10",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = Identifier
                                    "baz_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "attributes.h:24:10",
                                fieldName = NamePair {
                                  nameC = Name "i",
                                  nameHsIdent = Identifier
                                    "baz_i"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 8,
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
                        "attributes.h:22:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["attributes.h"],
                          headerInclude = "attributes.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "baz_c")
                  (Idx 0),
                PeekCField
                  (HsStrLit "baz_i")
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
                    "Baz",
                  structConstr = Name
                    "@NsConstr"
                    "Baz",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "baz_c",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "attributes.h:23:10",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = Identifier
                                "baz_c"},
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
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "attributes.h:23:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["attributes.h"],
                              headerInclude = "attributes.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "baz_i",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "attributes.h:24:10",
                            fieldName = NamePair {
                              nameC = Name "i",
                              nameHsIdent = Identifier
                                "baz_i"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 8,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "i",
                          commentLocation = Just
                            "attributes.h:24:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["attributes.h"],
                              headerInclude = "attributes.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "attributes.h:22:9",
                        declId = NamePair {
                          nameC = Name "baz",
                          nameHsIdent = Identifier "Baz"},
                        declOrigin = NameOriginGenerated
                          (AnonId "attributes.h:22:9"),
                        declAliases = [Name "baz"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["attributes.h"],
                            headerInclude = "attributes.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Baz"),
                          structSizeof = 5,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "attributes.h:23:10",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = Identifier
                                    "baz_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "attributes.h:24:10",
                                fieldName = NamePair {
                                  nameC = Name "i",
                                  nameHsIdent = Identifier
                                    "baz_i"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 8,
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
                        "attributes.h:22:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["attributes.h"],
                          headerInclude = "attributes.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "baz_c")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "baz_i")
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
        "Baz",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Baz",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Baz"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "baz_c",
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
            (Name "@NsTypeConstr" "Baz"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "baz_c",
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
            (Name "@NsTypeConstr" "Baz"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "baz_i",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCInt,
          hasCFieldInstanceFieldOffset =
          1},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Baz"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "baz_i",
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
        "Qux",
      structConstr = Name
        "@NsConstr"
        "Qux",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "qux_c",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "attributes.h:29:10",
                fieldName = NamePair {
                  nameC = Name "c",
                  nameHsIdent = Identifier
                    "qux_c"},
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
              commentOrigin = Just "c",
              commentLocation = Just
                "attributes.h:29:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["attributes.h"],
                  headerInclude = "attributes.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "qux_i",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "attributes.h:30:10",
                fieldName = NamePair {
                  nameC = Name "i",
                  nameHsIdent = Identifier
                    "qux_i"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 8,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "i",
              commentLocation = Just
                "attributes.h:30:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["attributes.h"],
                  headerInclude = "attributes.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "attributes.h:28:9",
            declId = NamePair {
              nameC = Name "qux",
              nameHsIdent = Identifier "Qux"},
            declOrigin = NameOriginGenerated
              (AnonId "attributes.h:28:9"),
            declAliases = [Name "qux"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["attributes.h"],
                headerInclude = "attributes.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Qux"),
              structSizeof = 5,
              structAlignment = 1,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "attributes.h:29:10",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = Identifier
                        "qux_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "attributes.h:30:10",
                    fieldName = NamePair {
                      nameC = Name "i",
                      nameHsIdent = Identifier
                        "qux_i"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 8,
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
            "attributes.h:28:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["attributes.h"],
              headerInclude = "attributes.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Qux",
          structConstr = Name
            "@NsConstr"
            "Qux",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "qux_c",
              fieldType = HsPrimType
                HsPrimCChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "attributes.h:29:10",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = Identifier
                        "qux_c"},
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
                  commentOrigin = Just "c",
                  commentLocation = Just
                    "attributes.h:29:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["attributes.h"],
                      headerInclude = "attributes.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "qux_i",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "attributes.h:30:10",
                    fieldName = NamePair {
                      nameC = Name "i",
                      nameHsIdent = Identifier
                        "qux_i"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 8,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "i",
                  commentLocation = Just
                    "attributes.h:30:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["attributes.h"],
                      headerInclude = "attributes.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "attributes.h:28:9",
                declId = NamePair {
                  nameC = Name "qux",
                  nameHsIdent = Identifier "Qux"},
                declOrigin = NameOriginGenerated
                  (AnonId "attributes.h:28:9"),
                declAliases = [Name "qux"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["attributes.h"],
                    headerInclude = "attributes.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Qux"),
                  structSizeof = 5,
                  structAlignment = 1,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "attributes.h:29:10",
                        fieldName = NamePair {
                          nameC = Name "c",
                          nameHsIdent = Identifier
                            "qux_c"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "attributes.h:30:10",
                        fieldName = NamePair {
                          nameC = Name "i",
                          nameHsIdent = Identifier
                            "qux_i"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 8,
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
                "attributes.h:28:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["attributes.h"],
                  headerInclude = "attributes.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 5,
          storableAlignment = 1,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Qux",
                  structConstr = Name
                    "@NsConstr"
                    "Qux",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "qux_c",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "attributes.h:29:10",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = Identifier
                                "qux_c"},
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
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "attributes.h:29:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["attributes.h"],
                              headerInclude = "attributes.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "qux_i",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "attributes.h:30:10",
                            fieldName = NamePair {
                              nameC = Name "i",
                              nameHsIdent = Identifier
                                "qux_i"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 8,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "i",
                          commentLocation = Just
                            "attributes.h:30:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["attributes.h"],
                              headerInclude = "attributes.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "attributes.h:28:9",
                        declId = NamePair {
                          nameC = Name "qux",
                          nameHsIdent = Identifier "Qux"},
                        declOrigin = NameOriginGenerated
                          (AnonId "attributes.h:28:9"),
                        declAliases = [Name "qux"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["attributes.h"],
                            headerInclude = "attributes.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Qux"),
                          structSizeof = 5,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "attributes.h:29:10",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = Identifier
                                    "qux_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "attributes.h:30:10",
                                fieldName = NamePair {
                                  nameC = Name "i",
                                  nameHsIdent = Identifier
                                    "qux_i"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 8,
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
                        "attributes.h:28:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["attributes.h"],
                          headerInclude = "attributes.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "qux_c")
                  (Idx 0),
                PeekCField
                  (HsStrLit "qux_i")
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
                    "Qux",
                  structConstr = Name
                    "@NsConstr"
                    "Qux",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "qux_c",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "attributes.h:29:10",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = Identifier
                                "qux_c"},
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
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "attributes.h:29:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["attributes.h"],
                              headerInclude = "attributes.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "qux_i",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "attributes.h:30:10",
                            fieldName = NamePair {
                              nameC = Name "i",
                              nameHsIdent = Identifier
                                "qux_i"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 8,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "i",
                          commentLocation = Just
                            "attributes.h:30:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["attributes.h"],
                              headerInclude = "attributes.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "attributes.h:28:9",
                        declId = NamePair {
                          nameC = Name "qux",
                          nameHsIdent = Identifier "Qux"},
                        declOrigin = NameOriginGenerated
                          (AnonId "attributes.h:28:9"),
                        declAliases = [Name "qux"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["attributes.h"],
                            headerInclude = "attributes.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Qux"),
                          structSizeof = 5,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "attributes.h:29:10",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = Identifier
                                    "qux_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "attributes.h:30:10",
                                fieldName = NamePair {
                                  nameC = Name "i",
                                  nameHsIdent = Identifier
                                    "qux_i"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 8,
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
                        "attributes.h:28:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["attributes.h"],
                          headerInclude = "attributes.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "qux_c")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "qux_i")
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
        "Qux",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Qux",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Qux"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "qux_c",
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
            (Name "@NsTypeConstr" "Qux"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "qux_c",
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
            (Name "@NsTypeConstr" "Qux"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "qux_i",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCInt,
          hasCFieldInstanceFieldOffset =
          1},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Qux"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "qux_i",
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
        "FILE",
      structConstr = Name
        "@NsConstr"
        "FILE",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "fILE__r",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "attributes.h:35:9",
                fieldName = NamePair {
                  nameC = Name "_r",
                  nameHsIdent = Identifier
                    "fILE__r"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "_r",
              commentLocation = Just
                "attributes.h:35:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["attributes.h"],
                  headerInclude = "attributes.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "fILE__w",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "attributes.h:36:9",
                fieldName = NamePair {
                  nameC = Name "_w",
                  nameHsIdent = Identifier
                    "fILE__w"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "_w",
              commentLocation = Just
                "attributes.h:36:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["attributes.h"],
                  headerInclude = "attributes.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "fILE__close",
          fieldType = HsFunPtr
            (HsFun
              (HsPtr (HsPrimType HsPrimVoid))
              (HsIO (HsPrimType HsPrimCInt))),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "attributes.h:37:22",
                fieldName = NamePair {
                  nameC = Name "_close",
                  nameHsIdent = Identifier
                    "fILE__close"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeFun
                  [TypePointer TypeVoid]
                  (TypePrim
                    (PrimIntegral PrimInt Signed))),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "_close",
              commentLocation = Just
                "attributes.h:37:22",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["attributes.h"],
                  headerInclude = "attributes.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "attributes.h:34:16",
            declId = NamePair {
              nameC = Name "FILE",
              nameHsIdent = Identifier
                "FILE"},
            declOrigin =
            NameOriginRenamedFrom
              (Name "__sFILE"),
            declAliases = [Name "FILE"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["attributes.h"],
                headerInclude = "attributes.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "FILE"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "attributes.h:35:9",
                    fieldName = NamePair {
                      nameC = Name "_r",
                      nameHsIdent = Identifier
                        "fILE__r"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "attributes.h:36:9",
                    fieldName = NamePair {
                      nameC = Name "_w",
                      nameHsIdent = Identifier
                        "fILE__w"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "attributes.h:37:22",
                    fieldName = NamePair {
                      nameC = Name "_close",
                      nameHsIdent = Identifier
                        "fILE__close"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeFun
                      [TypePointer TypeVoid]
                      (TypePrim
                        (PrimIntegral PrimInt Signed))),
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
          commentOrigin = Just "FILE",
          commentLocation = Just
            "attributes.h:34:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["attributes.h"],
              headerInclude = "attributes.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "FILE",
          structConstr = Name
            "@NsConstr"
            "FILE",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "fILE__r",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "attributes.h:35:9",
                    fieldName = NamePair {
                      nameC = Name "_r",
                      nameHsIdent = Identifier
                        "fILE__r"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "_r",
                  commentLocation = Just
                    "attributes.h:35:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["attributes.h"],
                      headerInclude = "attributes.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "fILE__w",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "attributes.h:36:9",
                    fieldName = NamePair {
                      nameC = Name "_w",
                      nameHsIdent = Identifier
                        "fILE__w"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "_w",
                  commentLocation = Just
                    "attributes.h:36:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["attributes.h"],
                      headerInclude = "attributes.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "fILE__close",
              fieldType = HsFunPtr
                (HsFun
                  (HsPtr (HsPrimType HsPrimVoid))
                  (HsIO (HsPrimType HsPrimCInt))),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "attributes.h:37:22",
                    fieldName = NamePair {
                      nameC = Name "_close",
                      nameHsIdent = Identifier
                        "fILE__close"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeFun
                      [TypePointer TypeVoid]
                      (TypePrim
                        (PrimIntegral PrimInt Signed))),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "_close",
                  commentLocation = Just
                    "attributes.h:37:22",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["attributes.h"],
                      headerInclude = "attributes.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "attributes.h:34:16",
                declId = NamePair {
                  nameC = Name "FILE",
                  nameHsIdent = Identifier
                    "FILE"},
                declOrigin =
                NameOriginRenamedFrom
                  (Name "__sFILE"),
                declAliases = [Name "FILE"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["attributes.h"],
                    headerInclude = "attributes.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "FILE"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "attributes.h:35:9",
                        fieldName = NamePair {
                          nameC = Name "_r",
                          nameHsIdent = Identifier
                            "fILE__r"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "attributes.h:36:9",
                        fieldName = NamePair {
                          nameC = Name "_w",
                          nameHsIdent = Identifier
                            "fILE__w"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "attributes.h:37:22",
                        fieldName = NamePair {
                          nameC = Name "_close",
                          nameHsIdent = Identifier
                            "fILE__close"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeFun
                          [TypePointer TypeVoid]
                          (TypePrim
                            (PrimIntegral PrimInt Signed))),
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
              commentOrigin = Just "FILE",
              commentLocation = Just
                "attributes.h:34:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["attributes.h"],
                  headerInclude = "attributes.h"},
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
                    "FILE",
                  structConstr = Name
                    "@NsConstr"
                    "FILE",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "fILE__r",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "attributes.h:35:9",
                            fieldName = NamePair {
                              nameC = Name "_r",
                              nameHsIdent = Identifier
                                "fILE__r"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "_r",
                          commentLocation = Just
                            "attributes.h:35:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["attributes.h"],
                              headerInclude = "attributes.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "fILE__w",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "attributes.h:36:9",
                            fieldName = NamePair {
                              nameC = Name "_w",
                              nameHsIdent = Identifier
                                "fILE__w"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "_w",
                          commentLocation = Just
                            "attributes.h:36:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["attributes.h"],
                              headerInclude = "attributes.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "fILE__close",
                      fieldType = HsFunPtr
                        (HsFun
                          (HsPtr (HsPrimType HsPrimVoid))
                          (HsIO (HsPrimType HsPrimCInt))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "attributes.h:37:22",
                            fieldName = NamePair {
                              nameC = Name "_close",
                              nameHsIdent = Identifier
                                "fILE__close"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeFun
                              [TypePointer TypeVoid]
                              (TypePrim
                                (PrimIntegral PrimInt Signed))),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "_close",
                          commentLocation = Just
                            "attributes.h:37:22",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["attributes.h"],
                              headerInclude = "attributes.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "attributes.h:34:16",
                        declId = NamePair {
                          nameC = Name "FILE",
                          nameHsIdent = Identifier
                            "FILE"},
                        declOrigin =
                        NameOriginRenamedFrom
                          (Name "__sFILE"),
                        declAliases = [Name "FILE"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["attributes.h"],
                            headerInclude = "attributes.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "FILE"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "attributes.h:35:9",
                                fieldName = NamePair {
                                  nameC = Name "_r",
                                  nameHsIdent = Identifier
                                    "fILE__r"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "attributes.h:36:9",
                                fieldName = NamePair {
                                  nameC = Name "_w",
                                  nameHsIdent = Identifier
                                    "fILE__w"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "attributes.h:37:22",
                                fieldName = NamePair {
                                  nameC = Name "_close",
                                  nameHsIdent = Identifier
                                    "fILE__close"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeFun
                                  [TypePointer TypeVoid]
                                  (TypePrim
                                    (PrimIntegral PrimInt Signed))),
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
                      commentOrigin = Just "FILE",
                      commentLocation = Just
                        "attributes.h:34:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["attributes.h"],
                          headerInclude = "attributes.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "fILE__r")
                  (Idx 0),
                PeekCField
                  (HsStrLit "fILE__w")
                  (Idx 0),
                PeekCField
                  (HsStrLit "fILE__close")
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
                    "FILE",
                  structConstr = Name
                    "@NsConstr"
                    "FILE",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "fILE__r",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "attributes.h:35:9",
                            fieldName = NamePair {
                              nameC = Name "_r",
                              nameHsIdent = Identifier
                                "fILE__r"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "_r",
                          commentLocation = Just
                            "attributes.h:35:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["attributes.h"],
                              headerInclude = "attributes.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "fILE__w",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "attributes.h:36:9",
                            fieldName = NamePair {
                              nameC = Name "_w",
                              nameHsIdent = Identifier
                                "fILE__w"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "_w",
                          commentLocation = Just
                            "attributes.h:36:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["attributes.h"],
                              headerInclude = "attributes.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "fILE__close",
                      fieldType = HsFunPtr
                        (HsFun
                          (HsPtr (HsPrimType HsPrimVoid))
                          (HsIO (HsPrimType HsPrimCInt))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "attributes.h:37:22",
                            fieldName = NamePair {
                              nameC = Name "_close",
                              nameHsIdent = Identifier
                                "fILE__close"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeFun
                              [TypePointer TypeVoid]
                              (TypePrim
                                (PrimIntegral PrimInt Signed))),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "_close",
                          commentLocation = Just
                            "attributes.h:37:22",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["attributes.h"],
                              headerInclude = "attributes.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "attributes.h:34:16",
                        declId = NamePair {
                          nameC = Name "FILE",
                          nameHsIdent = Identifier
                            "FILE"},
                        declOrigin =
                        NameOriginRenamedFrom
                          (Name "__sFILE"),
                        declAliases = [Name "FILE"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["attributes.h"],
                            headerInclude = "attributes.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "FILE"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "attributes.h:35:9",
                                fieldName = NamePair {
                                  nameC = Name "_r",
                                  nameHsIdent = Identifier
                                    "fILE__r"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "attributes.h:36:9",
                                fieldName = NamePair {
                                  nameC = Name "_w",
                                  nameHsIdent = Identifier
                                    "fILE__w"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "attributes.h:37:22",
                                fieldName = NamePair {
                                  nameC = Name "_close",
                                  nameHsIdent = Identifier
                                    "fILE__close"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeFun
                                  [TypePointer TypeVoid]
                                  (TypePrim
                                    (PrimIntegral PrimInt Signed))),
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
                      commentOrigin = Just "FILE",
                      commentLocation = Just
                        "attributes.h:34:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["attributes.h"],
                          headerInclude = "attributes.h"},
                      commentChildren = []}}
                (Add 3)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "fILE__r")
                      (Idx 4)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "fILE__w")
                      (Idx 4)
                      (Idx 1),
                    PokeCField
                      (HsStrLit "fILE__close")
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
        "FILE",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "FILE",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "FILE"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "fILE__r",
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
            (Name "@NsTypeConstr" "FILE"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "fILE__r",
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
            (Name "@NsTypeConstr" "FILE"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "fILE__w",
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
            (Name "@NsTypeConstr" "FILE"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "fILE__w",
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
            (Name "@NsTypeConstr" "FILE"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "fILE__close",
          hasCFieldInstanceCFieldType =
          HsFunPtr
            (HsFun
              (HsPtr (HsPrimType HsPrimVoid))
              (HsIO (HsPrimType HsPrimCInt))),
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
            (Name "@NsTypeConstr" "FILE"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "fILE__close",
          hasFieldInstanceFieldType =
          HsFunPtr
            (HsFun
              (HsPtr (HsPrimType HsPrimVoid))
              (HsIO (HsPrimType HsPrimCInt))),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing}]
