[
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Another_typedef_struct_t",
      structConstr = Name
        "@NsConstr"
        "Another_typedef_struct_t",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "another_typedef_struct_t_foo",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "distilled_lib_1.h:9:22",
                fieldName = NamePair {
                  nameC = Name "foo",
                  nameHsIdent = Identifier
                    "another_typedef_struct_t_foo"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "foo",
              commentLocation = Just
                "distilled_lib_1.h:9:22",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["distilled_lib_1.h"],
                  headerInclude =
                  "distilled_lib_1.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "another_typedef_struct_t_bar",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "distilled_lib_1.h:9:32",
                fieldName = NamePair {
                  nameC = Name "bar",
                  nameHsIdent = Identifier
                    "another_typedef_struct_t_bar"},
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
              commentOrigin = Just "bar",
              commentLocation = Just
                "distilled_lib_1.h:9:32",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["distilled_lib_1.h"],
                  headerInclude =
                  "distilled_lib_1.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "distilled_lib_1.h:9:9",
            declId = NamePair {
              nameC = Name
                "another_typedef_struct_t",
              nameHsIdent = Identifier
                "Another_typedef_struct_t"},
            declOrigin = NameOriginGenerated
              (AnonId
                "distilled_lib_1.h:9:9"),
            declAliases = [
              Name
                "another_typedef_struct_t"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["distilled_lib_1.h"],
                headerInclude =
                "distilled_lib_1.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name
                  "@NsConstr"
                  "Another_typedef_struct_t"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:9:22",
                    fieldName = NamePair {
                      nameC = Name "foo",
                      nameHsIdent = Identifier
                        "another_typedef_struct_t_foo"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:9:32",
                    fieldName = NamePair {
                      nameC = Name "bar",
                      nameHsIdent = Identifier
                        "another_typedef_struct_t_bar"},
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
              cTypeSpecModule = Nothing,
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
            "distilled_lib_1.h:9:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Another_typedef_struct_t",
          structConstr = Name
            "@NsConstr"
            "Another_typedef_struct_t",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "another_typedef_struct_t_foo",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:9:22",
                    fieldName = NamePair {
                      nameC = Name "foo",
                      nameHsIdent = Identifier
                        "another_typedef_struct_t_foo"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "foo",
                  commentLocation = Just
                    "distilled_lib_1.h:9:22",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["distilled_lib_1.h"],
                      headerInclude =
                      "distilled_lib_1.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "another_typedef_struct_t_bar",
              fieldType = HsPrimType
                HsPrimCChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:9:32",
                    fieldName = NamePair {
                      nameC = Name "bar",
                      nameHsIdent = Identifier
                        "another_typedef_struct_t_bar"},
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
                  commentOrigin = Just "bar",
                  commentLocation = Just
                    "distilled_lib_1.h:9:32",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["distilled_lib_1.h"],
                      headerInclude =
                      "distilled_lib_1.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "distilled_lib_1.h:9:9",
                declId = NamePair {
                  nameC = Name
                    "another_typedef_struct_t",
                  nameHsIdent = Identifier
                    "Another_typedef_struct_t"},
                declOrigin = NameOriginGenerated
                  (AnonId
                    "distilled_lib_1.h:9:9"),
                declAliases = [
                  Name
                    "another_typedef_struct_t"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["distilled_lib_1.h"],
                    headerInclude =
                    "distilled_lib_1.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name
                      "@NsConstr"
                      "Another_typedef_struct_t"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "distilled_lib_1.h:9:22",
                        fieldName = NamePair {
                          nameC = Name "foo",
                          nameHsIdent = Identifier
                            "another_typedef_struct_t_foo"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "distilled_lib_1.h:9:32",
                        fieldName = NamePair {
                          nameC = Name "bar",
                          nameHsIdent = Identifier
                            "another_typedef_struct_t_bar"},
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
                  cTypeSpecModule = Nothing,
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
                "distilled_lib_1.h:9:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["distilled_lib_1.h"],
                  headerInclude =
                  "distilled_lib_1.h"},
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
                    "Another_typedef_struct_t",
                  structConstr = Name
                    "@NsConstr"
                    "Another_typedef_struct_t",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "another_typedef_struct_t_foo",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:9:22",
                            fieldName = NamePair {
                              nameC = Name "foo",
                              nameHsIdent = Identifier
                                "another_typedef_struct_t_foo"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "foo",
                          commentLocation = Just
                            "distilled_lib_1.h:9:22",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "another_typedef_struct_t_bar",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:9:32",
                            fieldName = NamePair {
                              nameC = Name "bar",
                              nameHsIdent = Identifier
                                "another_typedef_struct_t_bar"},
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
                          commentOrigin = Just "bar",
                          commentLocation = Just
                            "distilled_lib_1.h:9:32",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "distilled_lib_1.h:9:9",
                        declId = NamePair {
                          nameC = Name
                            "another_typedef_struct_t",
                          nameHsIdent = Identifier
                            "Another_typedef_struct_t"},
                        declOrigin = NameOriginGenerated
                          (AnonId
                            "distilled_lib_1.h:9:9"),
                        declAliases = [
                          Name
                            "another_typedef_struct_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["distilled_lib_1.h"],
                            headerInclude =
                            "distilled_lib_1.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Another_typedef_struct_t"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:9:22",
                                fieldName = NamePair {
                                  nameC = Name "foo",
                                  nameHsIdent = Identifier
                                    "another_typedef_struct_t_foo"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:9:32",
                                fieldName = NamePair {
                                  nameC = Name "bar",
                                  nameHsIdent = Identifier
                                    "another_typedef_struct_t_bar"},
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
                          cTypeSpecModule = Nothing,
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
                        "distilled_lib_1.h:9:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["distilled_lib_1.h"],
                          headerInclude =
                          "distilled_lib_1.h"},
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
                  structName = Name
                    "@NsTypeConstr"
                    "Another_typedef_struct_t",
                  structConstr = Name
                    "@NsConstr"
                    "Another_typedef_struct_t",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "another_typedef_struct_t_foo",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:9:22",
                            fieldName = NamePair {
                              nameC = Name "foo",
                              nameHsIdent = Identifier
                                "another_typedef_struct_t_foo"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "foo",
                          commentLocation = Just
                            "distilled_lib_1.h:9:22",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "another_typedef_struct_t_bar",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:9:32",
                            fieldName = NamePair {
                              nameC = Name "bar",
                              nameHsIdent = Identifier
                                "another_typedef_struct_t_bar"},
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
                          commentOrigin = Just "bar",
                          commentLocation = Just
                            "distilled_lib_1.h:9:32",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "distilled_lib_1.h:9:9",
                        declId = NamePair {
                          nameC = Name
                            "another_typedef_struct_t",
                          nameHsIdent = Identifier
                            "Another_typedef_struct_t"},
                        declOrigin = NameOriginGenerated
                          (AnonId
                            "distilled_lib_1.h:9:9"),
                        declAliases = [
                          Name
                            "another_typedef_struct_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["distilled_lib_1.h"],
                            headerInclude =
                            "distilled_lib_1.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Another_typedef_struct_t"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:9:22",
                                fieldName = NamePair {
                                  nameC = Name "foo",
                                  nameHsIdent = Identifier
                                    "another_typedef_struct_t_foo"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:9:32",
                                fieldName = NamePair {
                                  nameC = Name "bar",
                                  nameHsIdent = Identifier
                                    "another_typedef_struct_t_bar"},
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
                          cTypeSpecModule = Nothing,
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
                        "distilled_lib_1.h:9:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["distilled_lib_1.h"],
                          headerInclude =
                          "distilled_lib_1.h"},
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
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Another_typedef_struct_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Another_typedef_struct_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Another_typedef_enum_e",
      newtypeConstr = Name
        "@NsConstr"
        "Another_typedef_enum_e",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Another_typedef_enum_e",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "distilled_lib_1.h:10:9",
          declId = NamePair {
            nameC = Name
              "another_typedef_enum_e",
            nameHsIdent = Identifier
              "Another_typedef_enum_e"},
          declOrigin = NameOriginGenerated
            (AnonId
              "distilled_lib_1.h:10:9"),
          declAliases = [
            Name "another_typedef_enum_e"],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          declComment = Nothing},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Another_typedef_enum_e",
              newtypeField = Name
                "@NsVar"
                "un_Another_typedef_enum_e"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "distilled_lib_1.h:10:16",
                  fieldName = NamePair {
                    nameC = Name "FOO",
                    nameHsIdent = Identifier "FOO"},
                  fieldComment = Nothing},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "distilled_lib_1.h:10:21",
                  fieldName = NamePair {
                    nameC = Name "BAR",
                    nameHsIdent = Identifier "BAR"},
                  fieldComment = Nothing},
                enumConstantValue = 1}]},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Read, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Just
            "distilled_lib_1.h:10:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Another_typedef_enum_e",
          structConstr = Name
            "@NsConstr"
            "Another_typedef_enum_e",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_Another_typedef_enum_e",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
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
                    "Another_typedef_enum_e",
                  structConstr = Name
                    "@NsConstr"
                    "Another_typedef_enum_e",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "un_Another_typedef_enum_e",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = GeneratedField,
                      fieldComment = Nothing}],
                  structOrigin = Nothing,
                  structInstances = Set.fromList
                    [Eq, Ord, Read, Show, Storable],
                  structComment = Nothing})
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
                    "Another_typedef_enum_e",
                  structConstr = Name
                    "@NsConstr"
                    "Another_typedef_enum_e",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "un_Another_typedef_enum_e",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = GeneratedField,
                      fieldComment = Nothing}],
                  structOrigin = Nothing,
                  structInstances = Set.fromList
                    [Eq, Ord, Read, Show, Storable],
                  structComment = Nothing}
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
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Another_typedef_enum_e",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Another_typedef_enum_e",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Another_typedef_enum_e",
          structConstr = Name
            "@NsConstr"
            "Another_typedef_enum_e",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_Another_typedef_enum_e",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (HsPrimType HsPrimCUInt)
        (Map.fromList
          [
            _×_ 0 (NE.fromList ["FOO"]),
            _×_ 1 (NE.fromList ["BAR"])])
        True,
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceSequentialCEnum
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Another_typedef_enum_e",
          structConstr = Name
            "@NsConstr"
            "Another_typedef_enum_e",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_Another_typedef_enum_e",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (Name "@NsConstr" "FOO")
        (Name "@NsConstr" "BAR"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Another_typedef_enum_e",
          structConstr = Name
            "@NsConstr"
            "Another_typedef_enum_e",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_Another_typedef_enum_e",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumRead
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Another_typedef_enum_e",
          structConstr = Name
            "@NsConstr"
            "Another_typedef_enum_e",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_Another_typedef_enum_e",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing},
      defineInstanceComment =
      Nothing},
  DeclPatSyn
    PatSyn {
      patSynName = Name
        "@NsConstr"
        "FOO",
      patSynType = Name
        "@NsTypeConstr"
        "Another_typedef_enum_e",
      patSynConstr = Name
        "@NsConstr"
        "Another_typedef_enum_e",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "distilled_lib_1.h:10:16",
            fieldName = NamePair {
              nameC = Name "FOO",
              nameHsIdent = Identifier "FOO"},
            fieldComment = Nothing},
          enumConstantValue = 0},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "FOO",
          commentLocation = Just
            "distilled_lib_1.h:10:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          commentChildren = []}},
  DeclPatSyn
    PatSyn {
      patSynName = Name
        "@NsConstr"
        "BAR",
      patSynType = Name
        "@NsTypeConstr"
        "Another_typedef_enum_e",
      patSynConstr = Name
        "@NsConstr"
        "Another_typedef_enum_e",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "distilled_lib_1.h:10:21",
            fieldName = NamePair {
              nameC = Name "BAR",
              nameHsIdent = Identifier "BAR"},
            fieldComment = Nothing},
          enumConstantValue = 1},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "BAR",
          commentLocation = Just
            "distilled_lib_1.h:10:21",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          commentChildren = []}},
  DeclVar
    VarDecl {
      varDeclName = Name "@NsVar" "a",
      varDeclType = ForallTy {
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = TyConAppTy
            (ATyCon
              (GenerativeTyCon
                (DataTyCon IntLikeTyCon)))
            [
              TyConAppTy
                (ATyCon
                  (GenerativeTyCon
                    (DataTyCon
                      (IntLikeTyCon
                        (CIntegralType
                          (IntLike (Int Signed)))))))
                []]}},
      varDeclBody = VarDeclIntegral
        5
        HsPrimCInt,
      varDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "A",
          commentLocation = Just
            "distilled_lib_1.h:11:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          commentChildren = []}},
  DeclVar
    VarDecl {
      varDeclName = Name "@NsVar" "b",
      varDeclType = ForallTy {
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = TyConAppTy
            (ATyCon
              (GenerativeTyCon
                (DataTyCon IntLikeTyCon)))
            [
              TyConAppTy
                (ATyCon
                  (GenerativeTyCon
                    (DataTyCon
                      (IntLikeTyCon
                        (CIntegralType
                          (IntLike (Int Signed)))))))
                []]}},
      varDeclBody = VarDeclIntegral
        3
        HsPrimCInt,
      varDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "B",
          commentLocation = Just
            "distilled_lib_1.h:12:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          commentChildren = []}},
  DeclVar
    VarDecl {
      varDeclName = Name
        "@NsVar"
        "sOME_DEFINED_CONSTANT",
      varDeclType = ForallTy {
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = TyConAppTy
            (ATyCon
              (GenerativeTyCon
                (DataTyCon IntLikeTyCon)))
            [
              TyConAppTy
                (ATyCon
                  (GenerativeTyCon
                    (DataTyCon
                      (IntLikeTyCon
                        (CIntegralType
                          (IntLike (Int Signed)))))))
                []]}},
      varDeclBody = VarDeclIntegral
        4
        HsPrimCInt,
      varDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "SOME_DEFINED_CONSTANT",
          commentLocation = Just
            "distilled_lib_1.h:13:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          commentChildren = []}},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "A_type_t",
      newtypeConstr = Name
        "@NsConstr"
        "A_type_t",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_A_type_t",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "distilled_lib_1.h:14:13",
          declId = NamePair {
            nameC = Name "a_type_t",
            nameHsIdent = Identifier
              "A_type_t"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "A_type_t",
              newtypeField = Name
                "@NsVar"
                "un_A_type_t"},
            typedefType = TypePrim
              (PrimIntegral PrimInt Signed)},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
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
          commentOrigin = Just "a_type_t",
          commentLocation = Just
            "distilled_lib_1.h:14:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "A_type_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "A_type_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "A_type_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "A_type_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "A_type_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "A_type_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "A_type_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "A_type_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "A_type_t",
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
        "A_type_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "A_type_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "A_type_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "A_type_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Var_t",
      newtypeConstr = Name
        "@NsConstr"
        "Var_t",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Var_t",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "distilled_lib_1.h:15:13",
          declId = NamePair {
            nameC = Name "var_t",
            nameHsIdent = Identifier
              "Var_t"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Var_t",
              newtypeField = Name
                "@NsVar"
                "un_Var_t"},
            typedefType = TypePrim
              (PrimIntegral PrimInt Signed)},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
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
          commentOrigin = Just "var_t",
          commentLocation = Just
            "distilled_lib_1.h:15:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Var_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Var_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Var_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Var_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Var_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Var_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Var_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Var_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Var_t",
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
        "Var_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Var_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Var_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Var_t",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "A_typedef_struct_t",
      structConstr = Name
        "@NsConstr"
        "A_typedef_struct_t",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "a_typedef_struct_t_field_0",
          fieldType = HsPrimType
            HsPrimCBool,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "distilled_lib_1.h:37:31",
                fieldName = NamePair {
                  nameC = Name "field_0",
                  nameHsIdent = Identifier
                    "a_typedef_struct_t_field_0"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                PrimBool,
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "field_0",
              commentLocation = Just
                "distilled_lib_1.h:37:31",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["distilled_lib_1.h"],
                  headerInclude =
                  "distilled_lib_1.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "a_typedef_struct_t_field_1",
          fieldType = HsExtBinding
            ExtRef {
              extRefModule = ModuleName
                "HsBindgen.Runtime.Prelude",
              extRefIdentifier = Identifier
                "Word8"}
            CTypeSpec {
              cTypeSpecModule = Just
                (ModuleName
                  "HsBindgen.Runtime.Prelude"),
              cTypeSpecIdentifier = Just
                (Identifier "Word8"),
              cTypeSpecInstances =
              Map.fromList
                [
                  _×_
                    Bits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bounded
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Enum
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Eq
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    FiniteBits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Integral
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ix
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Num
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ord
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Read
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    ReadRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Real
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    StaticSize
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]},
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "distilled_lib_1.h:38:31",
                fieldName = NamePair {
                  nameC = Name "field_1",
                  nameHsIdent = Identifier
                    "a_typedef_struct_t_field_1"},
                fieldComment = Nothing},
              structFieldType = TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "uint8_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtRef {
                    extRefModule = ModuleName
                      "HsBindgen.Runtime.Prelude",
                    extRefIdentifier = Identifier
                      "Word8"},
                  extHsSpec = CTypeSpec {
                    cTypeSpecModule = Just
                      (ModuleName
                        "HsBindgen.Runtime.Prelude"),
                    cTypeSpecIdentifier = Just
                      (Identifier "Word8"),
                    cTypeSpecInstances =
                    Map.fromList
                      [
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}},
              structFieldOffset = 8,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "field_1",
              commentLocation = Just
                "distilled_lib_1.h:38:31",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["distilled_lib_1.h"],
                  headerInclude =
                  "distilled_lib_1.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "a_typedef_struct_t_field_2",
          fieldType = HsExtBinding
            ExtRef {
              extRefModule = ModuleName
                "HsBindgen.Runtime.Prelude",
              extRefIdentifier = Identifier
                "Word16"}
            CTypeSpec {
              cTypeSpecModule = Just
                (ModuleName
                  "HsBindgen.Runtime.Prelude"),
              cTypeSpecIdentifier = Just
                (Identifier "Word16"),
              cTypeSpecInstances =
              Map.fromList
                [
                  _×_
                    Bits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bounded
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Enum
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Eq
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    FiniteBits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Integral
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ix
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Num
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ord
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Read
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    ReadRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Real
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    StaticSize
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]},
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "distilled_lib_1.h:39:31",
                fieldName = NamePair {
                  nameC = Name "field_2",
                  nameHsIdent = Identifier
                    "a_typedef_struct_t_field_2"},
                fieldComment = Nothing},
              structFieldType = TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "uint16_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtRef {
                    extRefModule = ModuleName
                      "HsBindgen.Runtime.Prelude",
                    extRefIdentifier = Identifier
                      "Word16"},
                  extHsSpec = CTypeSpec {
                    cTypeSpecModule = Just
                      (ModuleName
                        "HsBindgen.Runtime.Prelude"),
                    cTypeSpecIdentifier = Just
                      (Identifier "Word16"),
                    cTypeSpecInstances =
                    Map.fromList
                      [
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}},
              structFieldOffset = 16,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "field_2",
              commentLocation = Just
                "distilled_lib_1.h:39:31",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["distilled_lib_1.h"],
                  headerInclude =
                  "distilled_lib_1.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "a_typedef_struct_t_field_3",
          fieldType = HsExtBinding
            ExtRef {
              extRefModule = ModuleName
                "HsBindgen.Runtime.Prelude",
              extRefIdentifier = Identifier
                "Word32"}
            CTypeSpec {
              cTypeSpecModule = Just
                (ModuleName
                  "HsBindgen.Runtime.Prelude"),
              cTypeSpecIdentifier = Just
                (Identifier "Word32"),
              cTypeSpecInstances =
              Map.fromList
                [
                  _×_
                    Bits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bounded
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Enum
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Eq
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    FiniteBits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Integral
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ix
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Num
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ord
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Read
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    ReadRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Real
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    StaticSize
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]},
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "distilled_lib_1.h:40:31",
                fieldName = NamePair {
                  nameC = Name "field_3",
                  nameHsIdent = Identifier
                    "a_typedef_struct_t_field_3"},
                fieldComment = Nothing},
              structFieldType = TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "uint32_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtRef {
                    extRefModule = ModuleName
                      "HsBindgen.Runtime.Prelude",
                    extRefIdentifier = Identifier
                      "Word32"},
                  extHsSpec = CTypeSpec {
                    cTypeSpecModule = Just
                      (ModuleName
                        "HsBindgen.Runtime.Prelude"),
                    cTypeSpecIdentifier = Just
                      (Identifier "Word32"),
                    cTypeSpecInstances =
                    Map.fromList
                      [
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}},
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "field_3",
              commentLocation = Just
                "distilled_lib_1.h:40:31",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["distilled_lib_1.h"],
                  headerInclude =
                  "distilled_lib_1.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "a_typedef_struct_t_field_4",
          fieldType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Another_typedef_struct_t"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "distilled_lib_1.h:41:31",
                fieldName = NamePair {
                  nameC = Name "field_4",
                  nameHsIdent = Identifier
                    "a_typedef_struct_t_field_4"},
                fieldComment = Nothing},
              structFieldType = TypeTypedef
                (TypedefSquashed
                  (Name
                    "another_typedef_struct_t")
                  (TypeStruct
                    NamePair {
                      nameC = Name
                        "another_typedef_struct_t",
                      nameHsIdent = Identifier
                        "Another_typedef_struct_t"}
                    (NameOriginGenerated
                      (AnonId
                        "distilled_lib_1.h:9:9")))),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "field_4",
              commentLocation = Just
                "distilled_lib_1.h:41:31",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["distilled_lib_1.h"],
                  headerInclude =
                  "distilled_lib_1.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "a_typedef_struct_t_field_5",
          fieldType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Another_typedef_struct_t")),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "distilled_lib_1.h:42:31",
                fieldName = NamePair {
                  nameC = Name "field_5",
                  nameHsIdent = Identifier
                    "a_typedef_struct_t_field_5"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeTypedef
                  (TypedefSquashed
                    (Name
                      "another_typedef_struct_t")
                    (TypeStruct
                      NamePair {
                        nameC = Name
                          "another_typedef_struct_t",
                        nameHsIdent = Identifier
                          "Another_typedef_struct_t"}
                      (NameOriginGenerated
                        (AnonId
                          "distilled_lib_1.h:9:9"))))),
              structFieldOffset = 128,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "field_5",
              commentLocation = Just
                "distilled_lib_1.h:42:31",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["distilled_lib_1.h"],
                  headerInclude =
                  "distilled_lib_1.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "a_typedef_struct_t_field_6",
          fieldType = HsPtr
            (HsPrimType HsPrimVoid),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "distilled_lib_1.h:43:31",
                fieldName = NamePair {
                  nameC = Name "field_6",
                  nameHsIdent = Identifier
                    "a_typedef_struct_t_field_6"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                TypeVoid,
              structFieldOffset = 192,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "field_6",
              commentLocation = Just
                "distilled_lib_1.h:43:31",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["distilled_lib_1.h"],
                  headerInclude =
                  "distilled_lib_1.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "a_typedef_struct_t_field_7",
          fieldType = HsConstArray
            7
            (HsExtBinding
              ExtRef {
                extRefModule = ModuleName
                  "HsBindgen.Runtime.Prelude",
                extRefIdentifier = Identifier
                  "Word32"}
              CTypeSpec {
                cTypeSpecModule = Just
                  (ModuleName
                    "HsBindgen.Runtime.Prelude"),
                cTypeSpecIdentifier = Just
                  (Identifier "Word32"),
                cTypeSpecInstances =
                Map.fromList
                  [
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]}),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "distilled_lib_1.h:44:31",
                fieldName = NamePair {
                  nameC = Name "field_7",
                  nameHsIdent = Identifier
                    "a_typedef_struct_t_field_7"},
                fieldComment = Nothing},
              structFieldType = TypeConstArray
                7
                (TypeExtBinding
                  ResolvedExtBinding {
                    extCName = QualName {
                      qualNameName = Name "uint32_t",
                      qualNameKind =
                      NameKindOrdinary},
                    extHsRef = ExtRef {
                      extRefModule = ModuleName
                        "HsBindgen.Runtime.Prelude",
                      extRefIdentifier = Identifier
                        "Word32"},
                    extHsSpec = CTypeSpec {
                      cTypeSpecModule = Just
                        (ModuleName
                          "HsBindgen.Runtime.Prelude"),
                      cTypeSpecIdentifier = Just
                        (Identifier "Word32"),
                      cTypeSpecInstances =
                      Map.fromList
                        [
                          _×_
                            Bits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bounded
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Enum
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Eq
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            FiniteBits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Integral
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ix
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Num
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ord
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Read
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            ReadRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Real
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Show
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            StaticSize
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Storable
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            WriteRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = [
                                  ]})]}}),
              structFieldOffset = 256,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "field_7",
              commentLocation = Just
                "distilled_lib_1.h:44:31",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["distilled_lib_1.h"],
                  headerInclude =
                  "distilled_lib_1.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "a_typedef_struct_t_field_8",
          fieldType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Another_typedef_enum_e"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "distilled_lib_1.h:45:31",
                fieldName = NamePair {
                  nameC = Name "field_8",
                  nameHsIdent = Identifier
                    "a_typedef_struct_t_field_8"},
                fieldComment = Nothing},
              structFieldType = TypeTypedef
                (TypedefSquashed
                  (Name "another_typedef_enum_e")
                  (TypeEnum
                    NamePair {
                      nameC = Name
                        "another_typedef_enum_e",
                      nameHsIdent = Identifier
                        "Another_typedef_enum_e"}
                    (NameOriginGenerated
                      (AnonId
                        "distilled_lib_1.h:10:9")))),
              structFieldOffset = 480,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "field_8",
              commentLocation = Just
                "distilled_lib_1.h:45:31",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["distilled_lib_1.h"],
                  headerInclude =
                  "distilled_lib_1.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "a_typedef_struct_t_field_9",
          fieldType = HsConstArray
            4
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Another_typedef_enum_e")),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "distilled_lib_1.h:46:31",
                fieldName = NamePair {
                  nameC = Name "field_9",
                  nameHsIdent = Identifier
                    "a_typedef_struct_t_field_9"},
                fieldComment = Nothing},
              structFieldType = TypeConstArray
                4
                (TypeTypedef
                  (TypedefSquashed
                    (Name "another_typedef_enum_e")
                    (TypeEnum
                      NamePair {
                        nameC = Name
                          "another_typedef_enum_e",
                        nameHsIdent = Identifier
                          "Another_typedef_enum_e"}
                      (NameOriginGenerated
                        (AnonId
                          "distilled_lib_1.h:10:9"))))),
              structFieldOffset = 512,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "field_9",
              commentLocation = Just
                "distilled_lib_1.h:46:31",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["distilled_lib_1.h"],
                  headerInclude =
                  "distilled_lib_1.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "a_typedef_struct_t_field_10",
          fieldType = HsConstArray
            5
            (HsConstArray
              3
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Another_typedef_enum_e"))),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "distilled_lib_1.h:47:31",
                fieldName = NamePair {
                  nameC = Name "field_10",
                  nameHsIdent = Identifier
                    "a_typedef_struct_t_field_10"},
                fieldComment = Nothing},
              structFieldType = TypeConstArray
                5
                (TypeConstArray
                  3
                  (TypeTypedef
                    (TypedefSquashed
                      (Name "another_typedef_enum_e")
                      (TypeEnum
                        NamePair {
                          nameC = Name
                            "another_typedef_enum_e",
                          nameHsIdent = Identifier
                            "Another_typedef_enum_e"}
                        (NameOriginGenerated
                          (AnonId
                            "distilled_lib_1.h:10:9")))))),
              structFieldOffset = 640,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "field_10",
              commentLocation = Just
                "distilled_lib_1.h:47:31",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["distilled_lib_1.h"],
                  headerInclude =
                  "distilled_lib_1.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "distilled_lib_1.h:35:16",
            declId = NamePair {
              nameC = Name
                "a_typedef_struct_t",
              nameHsIdent = Identifier
                "A_typedef_struct_t"},
            declOrigin =
            NameOriginRenamedFrom
              (Name "a_typedef_struct"),
            declAliases = [
              Name "a_typedef_struct_t"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["distilled_lib_1.h"],
                headerInclude =
                "distilled_lib_1.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name
                  "@NsConstr"
                  "A_typedef_struct_t"),
              structSizeof = 140,
              structAlignment = 1,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:37:31",
                    fieldName = NamePair {
                      nameC = Name "field_0",
                      nameHsIdent = Identifier
                        "a_typedef_struct_t_field_0"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:38:31",
                    fieldName = NamePair {
                      nameC = Name "field_1",
                      nameHsIdent = Identifier
                        "a_typedef_struct_t_field_1"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint8_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtRef {
                        extRefModule = ModuleName
                          "HsBindgen.Runtime.Prelude",
                        extRefIdentifier = Identifier
                          "Word8"},
                      extHsSpec = CTypeSpec {
                        cTypeSpecModule = Just
                          (ModuleName
                            "HsBindgen.Runtime.Prelude"),
                        cTypeSpecIdentifier = Just
                          (Identifier "Word8"),
                        cTypeSpecInstances =
                        Map.fromList
                          [
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 8,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:39:31",
                    fieldName = NamePair {
                      nameC = Name "field_2",
                      nameHsIdent = Identifier
                        "a_typedef_struct_t_field_2"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint16_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtRef {
                        extRefModule = ModuleName
                          "HsBindgen.Runtime.Prelude",
                        extRefIdentifier = Identifier
                          "Word16"},
                      extHsSpec = CTypeSpec {
                        cTypeSpecModule = Just
                          (ModuleName
                            "HsBindgen.Runtime.Prelude"),
                        cTypeSpecIdentifier = Just
                          (Identifier "Word16"),
                        cTypeSpecInstances =
                        Map.fromList
                          [
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 16,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:40:31",
                    fieldName = NamePair {
                      nameC = Name "field_3",
                      nameHsIdent = Identifier
                        "a_typedef_struct_t_field_3"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint32_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtRef {
                        extRefModule = ModuleName
                          "HsBindgen.Runtime.Prelude",
                        extRefIdentifier = Identifier
                          "Word32"},
                      extHsSpec = CTypeSpec {
                        cTypeSpecModule = Just
                          (ModuleName
                            "HsBindgen.Runtime.Prelude"),
                        cTypeSpecIdentifier = Just
                          (Identifier "Word32"),
                        cTypeSpecInstances =
                        Map.fromList
                          [
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:41:31",
                    fieldName = NamePair {
                      nameC = Name "field_4",
                      nameHsIdent = Identifier
                        "a_typedef_struct_t_field_4"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name
                        "another_typedef_struct_t")
                      (TypeStruct
                        NamePair {
                          nameC = Name
                            "another_typedef_struct_t",
                          nameHsIdent = Identifier
                            "Another_typedef_struct_t"}
                        (NameOriginGenerated
                          (AnonId
                            "distilled_lib_1.h:9:9")))),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:42:31",
                    fieldName = NamePair {
                      nameC = Name "field_5",
                      nameHsIdent = Identifier
                        "a_typedef_struct_t_field_5"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeTypedef
                      (TypedefSquashed
                        (Name
                          "another_typedef_struct_t")
                        (TypeStruct
                          NamePair {
                            nameC = Name
                              "another_typedef_struct_t",
                            nameHsIdent = Identifier
                              "Another_typedef_struct_t"}
                          (NameOriginGenerated
                            (AnonId
                              "distilled_lib_1.h:9:9"))))),
                  structFieldOffset = 128,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:43:31",
                    fieldName = NamePair {
                      nameC = Name "field_6",
                      nameHsIdent = Identifier
                        "a_typedef_struct_t_field_6"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    TypeVoid,
                  structFieldOffset = 192,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:44:31",
                    fieldName = NamePair {
                      nameC = Name "field_7",
                      nameHsIdent = Identifier
                        "a_typedef_struct_t_field_7"},
                    fieldComment = Nothing},
                  structFieldType = TypeConstArray
                    7
                    (TypeExtBinding
                      ResolvedExtBinding {
                        extCName = QualName {
                          qualNameName = Name "uint32_t",
                          qualNameKind =
                          NameKindOrdinary},
                        extHsRef = ExtRef {
                          extRefModule = ModuleName
                            "HsBindgen.Runtime.Prelude",
                          extRefIdentifier = Identifier
                            "Word32"},
                        extHsSpec = CTypeSpec {
                          cTypeSpecModule = Just
                            (ModuleName
                              "HsBindgen.Runtime.Prelude"),
                          cTypeSpecIdentifier = Just
                            (Identifier "Word32"),
                          cTypeSpecInstances =
                          Map.fromList
                            [
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]}}),
                  structFieldOffset = 256,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:45:31",
                    fieldName = NamePair {
                      nameC = Name "field_8",
                      nameHsIdent = Identifier
                        "a_typedef_struct_t_field_8"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name "another_typedef_enum_e")
                      (TypeEnum
                        NamePair {
                          nameC = Name
                            "another_typedef_enum_e",
                          nameHsIdent = Identifier
                            "Another_typedef_enum_e"}
                        (NameOriginGenerated
                          (AnonId
                            "distilled_lib_1.h:10:9")))),
                  structFieldOffset = 480,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:46:31",
                    fieldName = NamePair {
                      nameC = Name "field_9",
                      nameHsIdent = Identifier
                        "a_typedef_struct_t_field_9"},
                    fieldComment = Nothing},
                  structFieldType = TypeConstArray
                    4
                    (TypeTypedef
                      (TypedefSquashed
                        (Name "another_typedef_enum_e")
                        (TypeEnum
                          NamePair {
                            nameC = Name
                              "another_typedef_enum_e",
                            nameHsIdent = Identifier
                              "Another_typedef_enum_e"}
                          (NameOriginGenerated
                            (AnonId
                              "distilled_lib_1.h:10:9"))))),
                  structFieldOffset = 512,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:47:31",
                    fieldName = NamePair {
                      nameC = Name "field_10",
                      nameHsIdent = Identifier
                        "a_typedef_struct_t_field_10"},
                    fieldComment = Nothing},
                  structFieldType = TypeConstArray
                    5
                    (TypeConstArray
                      3
                      (TypeTypedef
                        (TypedefSquashed
                          (Name "another_typedef_enum_e")
                          (TypeEnum
                            NamePair {
                              nameC = Name
                                "another_typedef_enum_e",
                              nameHsIdent = Identifier
                                "Another_typedef_enum_e"}
                            (NameOriginGenerated
                              (AnonId
                                "distilled_lib_1.h:10:9")))))),
                  structFieldOffset = 640,
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
          commentOrigin = Just
            "a_typedef_struct_t",
          commentLocation = Just
            "distilled_lib_1.h:35:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "A_typedef_struct_t",
          structConstr = Name
            "@NsConstr"
            "A_typedef_struct_t",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "a_typedef_struct_t_field_0",
              fieldType = HsPrimType
                HsPrimCBool,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:37:31",
                    fieldName = NamePair {
                      nameC = Name "field_0",
                      nameHsIdent = Identifier
                        "a_typedef_struct_t_field_0"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "field_0",
                  commentLocation = Just
                    "distilled_lib_1.h:37:31",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["distilled_lib_1.h"],
                      headerInclude =
                      "distilled_lib_1.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "a_typedef_struct_t_field_1",
              fieldType = HsExtBinding
                ExtRef {
                  extRefModule = ModuleName
                    "HsBindgen.Runtime.Prelude",
                  extRefIdentifier = Identifier
                    "Word8"}
                CTypeSpec {
                  cTypeSpecModule = Just
                    (ModuleName
                      "HsBindgen.Runtime.Prelude"),
                  cTypeSpecIdentifier = Just
                    (Identifier "Word8"),
                  cTypeSpecInstances =
                  Map.fromList
                    [
                      _×_
                        Bits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bounded
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Enum
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Eq
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        FiniteBits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Integral
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ix
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Num
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ord
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Read
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        ReadRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Real
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        StaticSize
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]},
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:38:31",
                    fieldName = NamePair {
                      nameC = Name "field_1",
                      nameHsIdent = Identifier
                        "a_typedef_struct_t_field_1"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint8_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtRef {
                        extRefModule = ModuleName
                          "HsBindgen.Runtime.Prelude",
                        extRefIdentifier = Identifier
                          "Word8"},
                      extHsSpec = CTypeSpec {
                        cTypeSpecModule = Just
                          (ModuleName
                            "HsBindgen.Runtime.Prelude"),
                        cTypeSpecIdentifier = Just
                          (Identifier "Word8"),
                        cTypeSpecInstances =
                        Map.fromList
                          [
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 8,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "field_1",
                  commentLocation = Just
                    "distilled_lib_1.h:38:31",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["distilled_lib_1.h"],
                      headerInclude =
                      "distilled_lib_1.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "a_typedef_struct_t_field_2",
              fieldType = HsExtBinding
                ExtRef {
                  extRefModule = ModuleName
                    "HsBindgen.Runtime.Prelude",
                  extRefIdentifier = Identifier
                    "Word16"}
                CTypeSpec {
                  cTypeSpecModule = Just
                    (ModuleName
                      "HsBindgen.Runtime.Prelude"),
                  cTypeSpecIdentifier = Just
                    (Identifier "Word16"),
                  cTypeSpecInstances =
                  Map.fromList
                    [
                      _×_
                        Bits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bounded
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Enum
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Eq
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        FiniteBits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Integral
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ix
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Num
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ord
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Read
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        ReadRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Real
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        StaticSize
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]},
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:39:31",
                    fieldName = NamePair {
                      nameC = Name "field_2",
                      nameHsIdent = Identifier
                        "a_typedef_struct_t_field_2"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint16_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtRef {
                        extRefModule = ModuleName
                          "HsBindgen.Runtime.Prelude",
                        extRefIdentifier = Identifier
                          "Word16"},
                      extHsSpec = CTypeSpec {
                        cTypeSpecModule = Just
                          (ModuleName
                            "HsBindgen.Runtime.Prelude"),
                        cTypeSpecIdentifier = Just
                          (Identifier "Word16"),
                        cTypeSpecInstances =
                        Map.fromList
                          [
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 16,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "field_2",
                  commentLocation = Just
                    "distilled_lib_1.h:39:31",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["distilled_lib_1.h"],
                      headerInclude =
                      "distilled_lib_1.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "a_typedef_struct_t_field_3",
              fieldType = HsExtBinding
                ExtRef {
                  extRefModule = ModuleName
                    "HsBindgen.Runtime.Prelude",
                  extRefIdentifier = Identifier
                    "Word32"}
                CTypeSpec {
                  cTypeSpecModule = Just
                    (ModuleName
                      "HsBindgen.Runtime.Prelude"),
                  cTypeSpecIdentifier = Just
                    (Identifier "Word32"),
                  cTypeSpecInstances =
                  Map.fromList
                    [
                      _×_
                        Bits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bounded
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Enum
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Eq
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        FiniteBits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Integral
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ix
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Num
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ord
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Read
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        ReadRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Real
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        StaticSize
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]},
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:40:31",
                    fieldName = NamePair {
                      nameC = Name "field_3",
                      nameHsIdent = Identifier
                        "a_typedef_struct_t_field_3"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint32_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtRef {
                        extRefModule = ModuleName
                          "HsBindgen.Runtime.Prelude",
                        extRefIdentifier = Identifier
                          "Word32"},
                      extHsSpec = CTypeSpec {
                        cTypeSpecModule = Just
                          (ModuleName
                            "HsBindgen.Runtime.Prelude"),
                        cTypeSpecIdentifier = Just
                          (Identifier "Word32"),
                        cTypeSpecInstances =
                        Map.fromList
                          [
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "field_3",
                  commentLocation = Just
                    "distilled_lib_1.h:40:31",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["distilled_lib_1.h"],
                      headerInclude =
                      "distilled_lib_1.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "a_typedef_struct_t_field_4",
              fieldType = HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Another_typedef_struct_t"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:41:31",
                    fieldName = NamePair {
                      nameC = Name "field_4",
                      nameHsIdent = Identifier
                        "a_typedef_struct_t_field_4"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name
                        "another_typedef_struct_t")
                      (TypeStruct
                        NamePair {
                          nameC = Name
                            "another_typedef_struct_t",
                          nameHsIdent = Identifier
                            "Another_typedef_struct_t"}
                        (NameOriginGenerated
                          (AnonId
                            "distilled_lib_1.h:9:9")))),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "field_4",
                  commentLocation = Just
                    "distilled_lib_1.h:41:31",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["distilled_lib_1.h"],
                      headerInclude =
                      "distilled_lib_1.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "a_typedef_struct_t_field_5",
              fieldType = HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Another_typedef_struct_t")),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:42:31",
                    fieldName = NamePair {
                      nameC = Name "field_5",
                      nameHsIdent = Identifier
                        "a_typedef_struct_t_field_5"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeTypedef
                      (TypedefSquashed
                        (Name
                          "another_typedef_struct_t")
                        (TypeStruct
                          NamePair {
                            nameC = Name
                              "another_typedef_struct_t",
                            nameHsIdent = Identifier
                              "Another_typedef_struct_t"}
                          (NameOriginGenerated
                            (AnonId
                              "distilled_lib_1.h:9:9"))))),
                  structFieldOffset = 128,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "field_5",
                  commentLocation = Just
                    "distilled_lib_1.h:42:31",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["distilled_lib_1.h"],
                      headerInclude =
                      "distilled_lib_1.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "a_typedef_struct_t_field_6",
              fieldType = HsPtr
                (HsPrimType HsPrimVoid),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:43:31",
                    fieldName = NamePair {
                      nameC = Name "field_6",
                      nameHsIdent = Identifier
                        "a_typedef_struct_t_field_6"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    TypeVoid,
                  structFieldOffset = 192,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "field_6",
                  commentLocation = Just
                    "distilled_lib_1.h:43:31",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["distilled_lib_1.h"],
                      headerInclude =
                      "distilled_lib_1.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "a_typedef_struct_t_field_7",
              fieldType = HsConstArray
                7
                (HsExtBinding
                  ExtRef {
                    extRefModule = ModuleName
                      "HsBindgen.Runtime.Prelude",
                    extRefIdentifier = Identifier
                      "Word32"}
                  CTypeSpec {
                    cTypeSpecModule = Just
                      (ModuleName
                        "HsBindgen.Runtime.Prelude"),
                    cTypeSpecIdentifier = Just
                      (Identifier "Word32"),
                    cTypeSpecInstances =
                    Map.fromList
                      [
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:44:31",
                    fieldName = NamePair {
                      nameC = Name "field_7",
                      nameHsIdent = Identifier
                        "a_typedef_struct_t_field_7"},
                    fieldComment = Nothing},
                  structFieldType = TypeConstArray
                    7
                    (TypeExtBinding
                      ResolvedExtBinding {
                        extCName = QualName {
                          qualNameName = Name "uint32_t",
                          qualNameKind =
                          NameKindOrdinary},
                        extHsRef = ExtRef {
                          extRefModule = ModuleName
                            "HsBindgen.Runtime.Prelude",
                          extRefIdentifier = Identifier
                            "Word32"},
                        extHsSpec = CTypeSpec {
                          cTypeSpecModule = Just
                            (ModuleName
                              "HsBindgen.Runtime.Prelude"),
                          cTypeSpecIdentifier = Just
                            (Identifier "Word32"),
                          cTypeSpecInstances =
                          Map.fromList
                            [
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]}}),
                  structFieldOffset = 256,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "field_7",
                  commentLocation = Just
                    "distilled_lib_1.h:44:31",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["distilled_lib_1.h"],
                      headerInclude =
                      "distilled_lib_1.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "a_typedef_struct_t_field_8",
              fieldType = HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Another_typedef_enum_e"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:45:31",
                    fieldName = NamePair {
                      nameC = Name "field_8",
                      nameHsIdent = Identifier
                        "a_typedef_struct_t_field_8"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name "another_typedef_enum_e")
                      (TypeEnum
                        NamePair {
                          nameC = Name
                            "another_typedef_enum_e",
                          nameHsIdent = Identifier
                            "Another_typedef_enum_e"}
                        (NameOriginGenerated
                          (AnonId
                            "distilled_lib_1.h:10:9")))),
                  structFieldOffset = 480,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "field_8",
                  commentLocation = Just
                    "distilled_lib_1.h:45:31",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["distilled_lib_1.h"],
                      headerInclude =
                      "distilled_lib_1.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "a_typedef_struct_t_field_9",
              fieldType = HsConstArray
                4
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Another_typedef_enum_e")),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:46:31",
                    fieldName = NamePair {
                      nameC = Name "field_9",
                      nameHsIdent = Identifier
                        "a_typedef_struct_t_field_9"},
                    fieldComment = Nothing},
                  structFieldType = TypeConstArray
                    4
                    (TypeTypedef
                      (TypedefSquashed
                        (Name "another_typedef_enum_e")
                        (TypeEnum
                          NamePair {
                            nameC = Name
                              "another_typedef_enum_e",
                            nameHsIdent = Identifier
                              "Another_typedef_enum_e"}
                          (NameOriginGenerated
                            (AnonId
                              "distilled_lib_1.h:10:9"))))),
                  structFieldOffset = 512,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "field_9",
                  commentLocation = Just
                    "distilled_lib_1.h:46:31",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["distilled_lib_1.h"],
                      headerInclude =
                      "distilled_lib_1.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "a_typedef_struct_t_field_10",
              fieldType = HsConstArray
                5
                (HsConstArray
                  3
                  (HsTypRef
                    (Name
                      "@NsTypeConstr"
                      "Another_typedef_enum_e"))),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "distilled_lib_1.h:47:31",
                    fieldName = NamePair {
                      nameC = Name "field_10",
                      nameHsIdent = Identifier
                        "a_typedef_struct_t_field_10"},
                    fieldComment = Nothing},
                  structFieldType = TypeConstArray
                    5
                    (TypeConstArray
                      3
                      (TypeTypedef
                        (TypedefSquashed
                          (Name "another_typedef_enum_e")
                          (TypeEnum
                            NamePair {
                              nameC = Name
                                "another_typedef_enum_e",
                              nameHsIdent = Identifier
                                "Another_typedef_enum_e"}
                            (NameOriginGenerated
                              (AnonId
                                "distilled_lib_1.h:10:9")))))),
                  structFieldOffset = 640,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "field_10",
                  commentLocation = Just
                    "distilled_lib_1.h:47:31",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["distilled_lib_1.h"],
                      headerInclude =
                      "distilled_lib_1.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "distilled_lib_1.h:35:16",
                declId = NamePair {
                  nameC = Name
                    "a_typedef_struct_t",
                  nameHsIdent = Identifier
                    "A_typedef_struct_t"},
                declOrigin =
                NameOriginRenamedFrom
                  (Name "a_typedef_struct"),
                declAliases = [
                  Name "a_typedef_struct_t"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["distilled_lib_1.h"],
                    headerInclude =
                    "distilled_lib_1.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name
                      "@NsConstr"
                      "A_typedef_struct_t"),
                  structSizeof = 140,
                  structAlignment = 1,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "distilled_lib_1.h:37:31",
                        fieldName = NamePair {
                          nameC = Name "field_0",
                          nameHsIdent = Identifier
                            "a_typedef_struct_t_field_0"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        PrimBool,
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "distilled_lib_1.h:38:31",
                        fieldName = NamePair {
                          nameC = Name "field_1",
                          nameHsIdent = Identifier
                            "a_typedef_struct_t_field_1"},
                        fieldComment = Nothing},
                      structFieldType = TypeExtBinding
                        ResolvedExtBinding {
                          extCName = QualName {
                            qualNameName = Name "uint8_t",
                            qualNameKind =
                            NameKindOrdinary},
                          extHsRef = ExtRef {
                            extRefModule = ModuleName
                              "HsBindgen.Runtime.Prelude",
                            extRefIdentifier = Identifier
                              "Word8"},
                          extHsSpec = CTypeSpec {
                            cTypeSpecModule = Just
                              (ModuleName
                                "HsBindgen.Runtime.Prelude"),
                            cTypeSpecIdentifier = Just
                              (Identifier "Word8"),
                            cTypeSpecInstances =
                            Map.fromList
                              [
                                _×_
                                  Bits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bounded
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Enum
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Eq
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  FiniteBits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Integral
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ix
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Num
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ord
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Read
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  ReadRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Real
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  StaticSize
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]}},
                      structFieldOffset = 8,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "distilled_lib_1.h:39:31",
                        fieldName = NamePair {
                          nameC = Name "field_2",
                          nameHsIdent = Identifier
                            "a_typedef_struct_t_field_2"},
                        fieldComment = Nothing},
                      structFieldType = TypeExtBinding
                        ResolvedExtBinding {
                          extCName = QualName {
                            qualNameName = Name "uint16_t",
                            qualNameKind =
                            NameKindOrdinary},
                          extHsRef = ExtRef {
                            extRefModule = ModuleName
                              "HsBindgen.Runtime.Prelude",
                            extRefIdentifier = Identifier
                              "Word16"},
                          extHsSpec = CTypeSpec {
                            cTypeSpecModule = Just
                              (ModuleName
                                "HsBindgen.Runtime.Prelude"),
                            cTypeSpecIdentifier = Just
                              (Identifier "Word16"),
                            cTypeSpecInstances =
                            Map.fromList
                              [
                                _×_
                                  Bits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bounded
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Enum
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Eq
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  FiniteBits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Integral
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ix
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Num
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ord
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Read
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  ReadRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Real
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  StaticSize
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]}},
                      structFieldOffset = 16,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "distilled_lib_1.h:40:31",
                        fieldName = NamePair {
                          nameC = Name "field_3",
                          nameHsIdent = Identifier
                            "a_typedef_struct_t_field_3"},
                        fieldComment = Nothing},
                      structFieldType = TypeExtBinding
                        ResolvedExtBinding {
                          extCName = QualName {
                            qualNameName = Name "uint32_t",
                            qualNameKind =
                            NameKindOrdinary},
                          extHsRef = ExtRef {
                            extRefModule = ModuleName
                              "HsBindgen.Runtime.Prelude",
                            extRefIdentifier = Identifier
                              "Word32"},
                          extHsSpec = CTypeSpec {
                            cTypeSpecModule = Just
                              (ModuleName
                                "HsBindgen.Runtime.Prelude"),
                            cTypeSpecIdentifier = Just
                              (Identifier "Word32"),
                            cTypeSpecInstances =
                            Map.fromList
                              [
                                _×_
                                  Bits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bounded
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Enum
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Eq
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  FiniteBits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Integral
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ix
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Num
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ord
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Read
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  ReadRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Real
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  StaticSize
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]}},
                      structFieldOffset = 32,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "distilled_lib_1.h:41:31",
                        fieldName = NamePair {
                          nameC = Name "field_4",
                          nameHsIdent = Identifier
                            "a_typedef_struct_t_field_4"},
                        fieldComment = Nothing},
                      structFieldType = TypeTypedef
                        (TypedefSquashed
                          (Name
                            "another_typedef_struct_t")
                          (TypeStruct
                            NamePair {
                              nameC = Name
                                "another_typedef_struct_t",
                              nameHsIdent = Identifier
                                "Another_typedef_struct_t"}
                            (NameOriginGenerated
                              (AnonId
                                "distilled_lib_1.h:9:9")))),
                      structFieldOffset = 64,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "distilled_lib_1.h:42:31",
                        fieldName = NamePair {
                          nameC = Name "field_5",
                          nameHsIdent = Identifier
                            "a_typedef_struct_t_field_5"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeTypedef
                          (TypedefSquashed
                            (Name
                              "another_typedef_struct_t")
                            (TypeStruct
                              NamePair {
                                nameC = Name
                                  "another_typedef_struct_t",
                                nameHsIdent = Identifier
                                  "Another_typedef_struct_t"}
                              (NameOriginGenerated
                                (AnonId
                                  "distilled_lib_1.h:9:9"))))),
                      structFieldOffset = 128,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "distilled_lib_1.h:43:31",
                        fieldName = NamePair {
                          nameC = Name "field_6",
                          nameHsIdent = Identifier
                            "a_typedef_struct_t_field_6"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        TypeVoid,
                      structFieldOffset = 192,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "distilled_lib_1.h:44:31",
                        fieldName = NamePair {
                          nameC = Name "field_7",
                          nameHsIdent = Identifier
                            "a_typedef_struct_t_field_7"},
                        fieldComment = Nothing},
                      structFieldType = TypeConstArray
                        7
                        (TypeExtBinding
                          ResolvedExtBinding {
                            extCName = QualName {
                              qualNameName = Name "uint32_t",
                              qualNameKind =
                              NameKindOrdinary},
                            extHsRef = ExtRef {
                              extRefModule = ModuleName
                                "HsBindgen.Runtime.Prelude",
                              extRefIdentifier = Identifier
                                "Word32"},
                            extHsSpec = CTypeSpec {
                              cTypeSpecModule = Just
                                (ModuleName
                                  "HsBindgen.Runtime.Prelude"),
                              cTypeSpecIdentifier = Just
                                (Identifier "Word32"),
                              cTypeSpecInstances =
                              Map.fromList
                                [
                                  _×_
                                    Bits
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Bounded
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Enum
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Eq
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    FiniteBits
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Integral
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Ix
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Num
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Ord
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Read
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    ReadRaw
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Real
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Show
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    StaticSize
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Storable
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    WriteRaw
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = [
                                          ]})]}}),
                      structFieldOffset = 256,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "distilled_lib_1.h:45:31",
                        fieldName = NamePair {
                          nameC = Name "field_8",
                          nameHsIdent = Identifier
                            "a_typedef_struct_t_field_8"},
                        fieldComment = Nothing},
                      structFieldType = TypeTypedef
                        (TypedefSquashed
                          (Name "another_typedef_enum_e")
                          (TypeEnum
                            NamePair {
                              nameC = Name
                                "another_typedef_enum_e",
                              nameHsIdent = Identifier
                                "Another_typedef_enum_e"}
                            (NameOriginGenerated
                              (AnonId
                                "distilled_lib_1.h:10:9")))),
                      structFieldOffset = 480,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "distilled_lib_1.h:46:31",
                        fieldName = NamePair {
                          nameC = Name "field_9",
                          nameHsIdent = Identifier
                            "a_typedef_struct_t_field_9"},
                        fieldComment = Nothing},
                      structFieldType = TypeConstArray
                        4
                        (TypeTypedef
                          (TypedefSquashed
                            (Name "another_typedef_enum_e")
                            (TypeEnum
                              NamePair {
                                nameC = Name
                                  "another_typedef_enum_e",
                                nameHsIdent = Identifier
                                  "Another_typedef_enum_e"}
                              (NameOriginGenerated
                                (AnonId
                                  "distilled_lib_1.h:10:9"))))),
                      structFieldOffset = 512,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "distilled_lib_1.h:47:31",
                        fieldName = NamePair {
                          nameC = Name "field_10",
                          nameHsIdent = Identifier
                            "a_typedef_struct_t_field_10"},
                        fieldComment = Nothing},
                      structFieldType = TypeConstArray
                        5
                        (TypeConstArray
                          3
                          (TypeTypedef
                            (TypedefSquashed
                              (Name "another_typedef_enum_e")
                              (TypeEnum
                                NamePair {
                                  nameC = Name
                                    "another_typedef_enum_e",
                                  nameHsIdent = Identifier
                                    "Another_typedef_enum_e"}
                                (NameOriginGenerated
                                  (AnonId
                                    "distilled_lib_1.h:10:9")))))),
                      structFieldOffset = 640,
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
              commentOrigin = Just
                "a_typedef_struct_t",
              commentLocation = Just
                "distilled_lib_1.h:35:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["distilled_lib_1.h"],
                  headerInclude =
                  "distilled_lib_1.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 140,
          storableAlignment = 1,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "A_typedef_struct_t",
                  structConstr = Name
                    "@NsConstr"
                    "A_typedef_struct_t",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_typedef_struct_t_field_0",
                      fieldType = HsPrimType
                        HsPrimCBool,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:37:31",
                            fieldName = NamePair {
                              nameC = Name "field_0",
                              nameHsIdent = Identifier
                                "a_typedef_struct_t_field_0"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            PrimBool,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field_0",
                          commentLocation = Just
                            "distilled_lib_1.h:37:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_typedef_struct_t_field_1",
                      fieldType = HsExtBinding
                        ExtRef {
                          extRefModule = ModuleName
                            "HsBindgen.Runtime.Prelude",
                          extRefIdentifier = Identifier
                            "Word8"}
                        CTypeSpec {
                          cTypeSpecModule = Just
                            (ModuleName
                              "HsBindgen.Runtime.Prelude"),
                          cTypeSpecIdentifier = Just
                            (Identifier "Word8"),
                          cTypeSpecInstances =
                          Map.fromList
                            [
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:38:31",
                            fieldName = NamePair {
                              nameC = Name "field_1",
                              nameHsIdent = Identifier
                                "a_typedef_struct_t_field_1"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint8_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtRef {
                                extRefModule = ModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extRefIdentifier = Identifier
                                  "Word8"},
                              extHsSpec = CTypeSpec {
                                cTypeSpecModule = Just
                                  (ModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                cTypeSpecIdentifier = Just
                                  (Identifier "Word8"),
                                cTypeSpecInstances =
                                Map.fromList
                                  [
                                    _×_
                                      Bits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bounded
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Enum
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Eq
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      FiniteBits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Integral
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ix
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Num
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ord
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Read
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      ReadRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Real
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      StaticSize
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}},
                          structFieldOffset = 8,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field_1",
                          commentLocation = Just
                            "distilled_lib_1.h:38:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_typedef_struct_t_field_2",
                      fieldType = HsExtBinding
                        ExtRef {
                          extRefModule = ModuleName
                            "HsBindgen.Runtime.Prelude",
                          extRefIdentifier = Identifier
                            "Word16"}
                        CTypeSpec {
                          cTypeSpecModule = Just
                            (ModuleName
                              "HsBindgen.Runtime.Prelude"),
                          cTypeSpecIdentifier = Just
                            (Identifier "Word16"),
                          cTypeSpecInstances =
                          Map.fromList
                            [
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:39:31",
                            fieldName = NamePair {
                              nameC = Name "field_2",
                              nameHsIdent = Identifier
                                "a_typedef_struct_t_field_2"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint16_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtRef {
                                extRefModule = ModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extRefIdentifier = Identifier
                                  "Word16"},
                              extHsSpec = CTypeSpec {
                                cTypeSpecModule = Just
                                  (ModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                cTypeSpecIdentifier = Just
                                  (Identifier "Word16"),
                                cTypeSpecInstances =
                                Map.fromList
                                  [
                                    _×_
                                      Bits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bounded
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Enum
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Eq
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      FiniteBits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Integral
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ix
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Num
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ord
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Read
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      ReadRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Real
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      StaticSize
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}},
                          structFieldOffset = 16,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field_2",
                          commentLocation = Just
                            "distilled_lib_1.h:39:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_typedef_struct_t_field_3",
                      fieldType = HsExtBinding
                        ExtRef {
                          extRefModule = ModuleName
                            "HsBindgen.Runtime.Prelude",
                          extRefIdentifier = Identifier
                            "Word32"}
                        CTypeSpec {
                          cTypeSpecModule = Just
                            (ModuleName
                              "HsBindgen.Runtime.Prelude"),
                          cTypeSpecIdentifier = Just
                            (Identifier "Word32"),
                          cTypeSpecInstances =
                          Map.fromList
                            [
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:40:31",
                            fieldName = NamePair {
                              nameC = Name "field_3",
                              nameHsIdent = Identifier
                                "a_typedef_struct_t_field_3"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint32_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtRef {
                                extRefModule = ModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extRefIdentifier = Identifier
                                  "Word32"},
                              extHsSpec = CTypeSpec {
                                cTypeSpecModule = Just
                                  (ModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                cTypeSpecIdentifier = Just
                                  (Identifier "Word32"),
                                cTypeSpecInstances =
                                Map.fromList
                                  [
                                    _×_
                                      Bits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bounded
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Enum
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Eq
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      FiniteBits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Integral
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ix
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Num
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ord
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Read
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      ReadRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Real
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      StaticSize
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}},
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field_3",
                          commentLocation = Just
                            "distilled_lib_1.h:40:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_typedef_struct_t_field_4",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Another_typedef_struct_t"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:41:31",
                            fieldName = NamePair {
                              nameC = Name "field_4",
                              nameHsIdent = Identifier
                                "a_typedef_struct_t_field_4"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefSquashed
                              (Name
                                "another_typedef_struct_t")
                              (TypeStruct
                                NamePair {
                                  nameC = Name
                                    "another_typedef_struct_t",
                                  nameHsIdent = Identifier
                                    "Another_typedef_struct_t"}
                                (NameOriginGenerated
                                  (AnonId
                                    "distilled_lib_1.h:9:9")))),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field_4",
                          commentLocation = Just
                            "distilled_lib_1.h:41:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_typedef_struct_t_field_5",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name
                            "@NsTypeConstr"
                            "Another_typedef_struct_t")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:42:31",
                            fieldName = NamePair {
                              nameC = Name "field_5",
                              nameHsIdent = Identifier
                                "a_typedef_struct_t_field_5"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeTypedef
                              (TypedefSquashed
                                (Name
                                  "another_typedef_struct_t")
                                (TypeStruct
                                  NamePair {
                                    nameC = Name
                                      "another_typedef_struct_t",
                                    nameHsIdent = Identifier
                                      "Another_typedef_struct_t"}
                                  (NameOriginGenerated
                                    (AnonId
                                      "distilled_lib_1.h:9:9"))))),
                          structFieldOffset = 128,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field_5",
                          commentLocation = Just
                            "distilled_lib_1.h:42:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_typedef_struct_t_field_6",
                      fieldType = HsPtr
                        (HsPrimType HsPrimVoid),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:43:31",
                            fieldName = NamePair {
                              nameC = Name "field_6",
                              nameHsIdent = Identifier
                                "a_typedef_struct_t_field_6"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            TypeVoid,
                          structFieldOffset = 192,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field_6",
                          commentLocation = Just
                            "distilled_lib_1.h:43:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_typedef_struct_t_field_7",
                      fieldType = HsConstArray
                        7
                        (HsExtBinding
                          ExtRef {
                            extRefModule = ModuleName
                              "HsBindgen.Runtime.Prelude",
                            extRefIdentifier = Identifier
                              "Word32"}
                          CTypeSpec {
                            cTypeSpecModule = Just
                              (ModuleName
                                "HsBindgen.Runtime.Prelude"),
                            cTypeSpecIdentifier = Just
                              (Identifier "Word32"),
                            cTypeSpecInstances =
                            Map.fromList
                              [
                                _×_
                                  Bits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bounded
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Enum
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Eq
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  FiniteBits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Integral
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ix
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Num
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ord
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Read
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  ReadRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Real
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  StaticSize
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]}),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:44:31",
                            fieldName = NamePair {
                              nameC = Name "field_7",
                              nameHsIdent = Identifier
                                "a_typedef_struct_t_field_7"},
                            fieldComment = Nothing},
                          structFieldType = TypeConstArray
                            7
                            (TypeExtBinding
                              ResolvedExtBinding {
                                extCName = QualName {
                                  qualNameName = Name "uint32_t",
                                  qualNameKind =
                                  NameKindOrdinary},
                                extHsRef = ExtRef {
                                  extRefModule = ModuleName
                                    "HsBindgen.Runtime.Prelude",
                                  extRefIdentifier = Identifier
                                    "Word32"},
                                extHsSpec = CTypeSpec {
                                  cTypeSpecModule = Just
                                    (ModuleName
                                      "HsBindgen.Runtime.Prelude"),
                                  cTypeSpecIdentifier = Just
                                    (Identifier "Word32"),
                                  cTypeSpecInstances =
                                  Map.fromList
                                    [
                                      _×_
                                        Bits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Bounded
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Enum
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Eq
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        FiniteBits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Integral
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ix
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Num
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ord
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Read
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        ReadRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Real
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Show
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        StaticSize
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Storable
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        WriteRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = [
                                              ]})]}}),
                          structFieldOffset = 256,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field_7",
                          commentLocation = Just
                            "distilled_lib_1.h:44:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_typedef_struct_t_field_8",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Another_typedef_enum_e"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:45:31",
                            fieldName = NamePair {
                              nameC = Name "field_8",
                              nameHsIdent = Identifier
                                "a_typedef_struct_t_field_8"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefSquashed
                              (Name "another_typedef_enum_e")
                              (TypeEnum
                                NamePair {
                                  nameC = Name
                                    "another_typedef_enum_e",
                                  nameHsIdent = Identifier
                                    "Another_typedef_enum_e"}
                                (NameOriginGenerated
                                  (AnonId
                                    "distilled_lib_1.h:10:9")))),
                          structFieldOffset = 480,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field_8",
                          commentLocation = Just
                            "distilled_lib_1.h:45:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_typedef_struct_t_field_9",
                      fieldType = HsConstArray
                        4
                        (HsTypRef
                          (Name
                            "@NsTypeConstr"
                            "Another_typedef_enum_e")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:46:31",
                            fieldName = NamePair {
                              nameC = Name "field_9",
                              nameHsIdent = Identifier
                                "a_typedef_struct_t_field_9"},
                            fieldComment = Nothing},
                          structFieldType = TypeConstArray
                            4
                            (TypeTypedef
                              (TypedefSquashed
                                (Name "another_typedef_enum_e")
                                (TypeEnum
                                  NamePair {
                                    nameC = Name
                                      "another_typedef_enum_e",
                                    nameHsIdent = Identifier
                                      "Another_typedef_enum_e"}
                                  (NameOriginGenerated
                                    (AnonId
                                      "distilled_lib_1.h:10:9"))))),
                          structFieldOffset = 512,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field_9",
                          commentLocation = Just
                            "distilled_lib_1.h:46:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_typedef_struct_t_field_10",
                      fieldType = HsConstArray
                        5
                        (HsConstArray
                          3
                          (HsTypRef
                            (Name
                              "@NsTypeConstr"
                              "Another_typedef_enum_e"))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:47:31",
                            fieldName = NamePair {
                              nameC = Name "field_10",
                              nameHsIdent = Identifier
                                "a_typedef_struct_t_field_10"},
                            fieldComment = Nothing},
                          structFieldType = TypeConstArray
                            5
                            (TypeConstArray
                              3
                              (TypeTypedef
                                (TypedefSquashed
                                  (Name "another_typedef_enum_e")
                                  (TypeEnum
                                    NamePair {
                                      nameC = Name
                                        "another_typedef_enum_e",
                                      nameHsIdent = Identifier
                                        "Another_typedef_enum_e"}
                                    (NameOriginGenerated
                                      (AnonId
                                        "distilled_lib_1.h:10:9")))))),
                          structFieldOffset = 640,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field_10",
                          commentLocation = Just
                            "distilled_lib_1.h:47:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "distilled_lib_1.h:35:16",
                        declId = NamePair {
                          nameC = Name
                            "a_typedef_struct_t",
                          nameHsIdent = Identifier
                            "A_typedef_struct_t"},
                        declOrigin =
                        NameOriginRenamedFrom
                          (Name "a_typedef_struct"),
                        declAliases = [
                          Name "a_typedef_struct_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["distilled_lib_1.h"],
                            headerInclude =
                            "distilled_lib_1.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "A_typedef_struct_t"),
                          structSizeof = 140,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:37:31",
                                fieldName = NamePair {
                                  nameC = Name "field_0",
                                  nameHsIdent = Identifier
                                    "a_typedef_struct_t_field_0"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                PrimBool,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:38:31",
                                fieldName = NamePair {
                                  nameC = Name "field_1",
                                  nameHsIdent = Identifier
                                    "a_typedef_struct_t_field_1"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint8_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtRef {
                                    extRefModule = ModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extRefIdentifier = Identifier
                                      "Word8"},
                                  extHsSpec = CTypeSpec {
                                    cTypeSpecModule = Just
                                      (ModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    cTypeSpecIdentifier = Just
                                      (Identifier "Word8"),
                                    cTypeSpecInstances =
                                    Map.fromList
                                      [
                                        _×_
                                          Bits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bounded
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Enum
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Eq
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          FiniteBits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Integral
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ix
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Num
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ord
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Read
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          ReadRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Real
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          StaticSize
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 8,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:39:31",
                                fieldName = NamePair {
                                  nameC = Name "field_2",
                                  nameHsIdent = Identifier
                                    "a_typedef_struct_t_field_2"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint16_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtRef {
                                    extRefModule = ModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extRefIdentifier = Identifier
                                      "Word16"},
                                  extHsSpec = CTypeSpec {
                                    cTypeSpecModule = Just
                                      (ModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    cTypeSpecIdentifier = Just
                                      (Identifier "Word16"),
                                    cTypeSpecInstances =
                                    Map.fromList
                                      [
                                        _×_
                                          Bits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bounded
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Enum
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Eq
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          FiniteBits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Integral
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ix
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Num
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ord
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Read
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          ReadRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Real
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          StaticSize
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 16,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:40:31",
                                fieldName = NamePair {
                                  nameC = Name "field_3",
                                  nameHsIdent = Identifier
                                    "a_typedef_struct_t_field_3"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint32_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtRef {
                                    extRefModule = ModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extRefIdentifier = Identifier
                                      "Word32"},
                                  extHsSpec = CTypeSpec {
                                    cTypeSpecModule = Just
                                      (ModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    cTypeSpecIdentifier = Just
                                      (Identifier "Word32"),
                                    cTypeSpecInstances =
                                    Map.fromList
                                      [
                                        _×_
                                          Bits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bounded
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Enum
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Eq
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          FiniteBits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Integral
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ix
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Num
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ord
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Read
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          ReadRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Real
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          StaticSize
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 32,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:41:31",
                                fieldName = NamePair {
                                  nameC = Name "field_4",
                                  nameHsIdent = Identifier
                                    "a_typedef_struct_t_field_4"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefSquashed
                                  (Name
                                    "another_typedef_struct_t")
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name
                                        "another_typedef_struct_t",
                                      nameHsIdent = Identifier
                                        "Another_typedef_struct_t"}
                                    (NameOriginGenerated
                                      (AnonId
                                        "distilled_lib_1.h:9:9")))),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:42:31",
                                fieldName = NamePair {
                                  nameC = Name "field_5",
                                  nameHsIdent = Identifier
                                    "a_typedef_struct_t_field_5"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeTypedef
                                  (TypedefSquashed
                                    (Name
                                      "another_typedef_struct_t")
                                    (TypeStruct
                                      NamePair {
                                        nameC = Name
                                          "another_typedef_struct_t",
                                        nameHsIdent = Identifier
                                          "Another_typedef_struct_t"}
                                      (NameOriginGenerated
                                        (AnonId
                                          "distilled_lib_1.h:9:9"))))),
                              structFieldOffset = 128,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:43:31",
                                fieldName = NamePair {
                                  nameC = Name "field_6",
                                  nameHsIdent = Identifier
                                    "a_typedef_struct_t_field_6"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                TypeVoid,
                              structFieldOffset = 192,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:44:31",
                                fieldName = NamePair {
                                  nameC = Name "field_7",
                                  nameHsIdent = Identifier
                                    "a_typedef_struct_t_field_7"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                7
                                (TypeExtBinding
                                  ResolvedExtBinding {
                                    extCName = QualName {
                                      qualNameName = Name "uint32_t",
                                      qualNameKind =
                                      NameKindOrdinary},
                                    extHsRef = ExtRef {
                                      extRefModule = ModuleName
                                        "HsBindgen.Runtime.Prelude",
                                      extRefIdentifier = Identifier
                                        "Word32"},
                                    extHsSpec = CTypeSpec {
                                      cTypeSpecModule = Just
                                        (ModuleName
                                          "HsBindgen.Runtime.Prelude"),
                                      cTypeSpecIdentifier = Just
                                        (Identifier "Word32"),
                                      cTypeSpecInstances =
                                      Map.fromList
                                        [
                                          _×_
                                            Bits
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Bounded
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Enum
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Eq
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            FiniteBits
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Integral
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Ix
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Num
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Ord
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Read
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            ReadRaw
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Real
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Show
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            StaticSize
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Storable
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            WriteRaw
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = [
                                                  ]})]}}),
                              structFieldOffset = 256,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:45:31",
                                fieldName = NamePair {
                                  nameC = Name "field_8",
                                  nameHsIdent = Identifier
                                    "a_typedef_struct_t_field_8"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefSquashed
                                  (Name "another_typedef_enum_e")
                                  (TypeEnum
                                    NamePair {
                                      nameC = Name
                                        "another_typedef_enum_e",
                                      nameHsIdent = Identifier
                                        "Another_typedef_enum_e"}
                                    (NameOriginGenerated
                                      (AnonId
                                        "distilled_lib_1.h:10:9")))),
                              structFieldOffset = 480,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:46:31",
                                fieldName = NamePair {
                                  nameC = Name "field_9",
                                  nameHsIdent = Identifier
                                    "a_typedef_struct_t_field_9"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                4
                                (TypeTypedef
                                  (TypedefSquashed
                                    (Name "another_typedef_enum_e")
                                    (TypeEnum
                                      NamePair {
                                        nameC = Name
                                          "another_typedef_enum_e",
                                        nameHsIdent = Identifier
                                          "Another_typedef_enum_e"}
                                      (NameOriginGenerated
                                        (AnonId
                                          "distilled_lib_1.h:10:9"))))),
                              structFieldOffset = 512,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:47:31",
                                fieldName = NamePair {
                                  nameC = Name "field_10",
                                  nameHsIdent = Identifier
                                    "a_typedef_struct_t_field_10"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                5
                                (TypeConstArray
                                  3
                                  (TypeTypedef
                                    (TypedefSquashed
                                      (Name "another_typedef_enum_e")
                                      (TypeEnum
                                        NamePair {
                                          nameC = Name
                                            "another_typedef_enum_e",
                                          nameHsIdent = Identifier
                                            "Another_typedef_enum_e"}
                                        (NameOriginGenerated
                                          (AnonId
                                            "distilled_lib_1.h:10:9")))))),
                              structFieldOffset = 640,
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
                      commentOrigin = Just
                        "a_typedef_struct_t",
                      commentLocation = Just
                        "distilled_lib_1.h:35:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["distilled_lib_1.h"],
                          headerInclude =
                          "distilled_lib_1.h"},
                      commentChildren = []}})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 1,
                PeekByteOff (Idx 0) 2,
                PeekByteOff (Idx 0) 4,
                PeekByteOff (Idx 0) 8,
                PeekByteOff (Idx 0) 16,
                PeekByteOff (Idx 0) 24,
                PeekByteOff (Idx 0) 32,
                PeekByteOff (Idx 0) 60,
                PeekByteOff (Idx 0) 64,
                PeekByteOff (Idx 0) 80]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "A_typedef_struct_t",
                  structConstr = Name
                    "@NsConstr"
                    "A_typedef_struct_t",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_typedef_struct_t_field_0",
                      fieldType = HsPrimType
                        HsPrimCBool,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:37:31",
                            fieldName = NamePair {
                              nameC = Name "field_0",
                              nameHsIdent = Identifier
                                "a_typedef_struct_t_field_0"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            PrimBool,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field_0",
                          commentLocation = Just
                            "distilled_lib_1.h:37:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_typedef_struct_t_field_1",
                      fieldType = HsExtBinding
                        ExtRef {
                          extRefModule = ModuleName
                            "HsBindgen.Runtime.Prelude",
                          extRefIdentifier = Identifier
                            "Word8"}
                        CTypeSpec {
                          cTypeSpecModule = Just
                            (ModuleName
                              "HsBindgen.Runtime.Prelude"),
                          cTypeSpecIdentifier = Just
                            (Identifier "Word8"),
                          cTypeSpecInstances =
                          Map.fromList
                            [
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:38:31",
                            fieldName = NamePair {
                              nameC = Name "field_1",
                              nameHsIdent = Identifier
                                "a_typedef_struct_t_field_1"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint8_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtRef {
                                extRefModule = ModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extRefIdentifier = Identifier
                                  "Word8"},
                              extHsSpec = CTypeSpec {
                                cTypeSpecModule = Just
                                  (ModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                cTypeSpecIdentifier = Just
                                  (Identifier "Word8"),
                                cTypeSpecInstances =
                                Map.fromList
                                  [
                                    _×_
                                      Bits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bounded
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Enum
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Eq
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      FiniteBits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Integral
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ix
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Num
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ord
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Read
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      ReadRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Real
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      StaticSize
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}},
                          structFieldOffset = 8,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field_1",
                          commentLocation = Just
                            "distilled_lib_1.h:38:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_typedef_struct_t_field_2",
                      fieldType = HsExtBinding
                        ExtRef {
                          extRefModule = ModuleName
                            "HsBindgen.Runtime.Prelude",
                          extRefIdentifier = Identifier
                            "Word16"}
                        CTypeSpec {
                          cTypeSpecModule = Just
                            (ModuleName
                              "HsBindgen.Runtime.Prelude"),
                          cTypeSpecIdentifier = Just
                            (Identifier "Word16"),
                          cTypeSpecInstances =
                          Map.fromList
                            [
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:39:31",
                            fieldName = NamePair {
                              nameC = Name "field_2",
                              nameHsIdent = Identifier
                                "a_typedef_struct_t_field_2"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint16_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtRef {
                                extRefModule = ModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extRefIdentifier = Identifier
                                  "Word16"},
                              extHsSpec = CTypeSpec {
                                cTypeSpecModule = Just
                                  (ModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                cTypeSpecIdentifier = Just
                                  (Identifier "Word16"),
                                cTypeSpecInstances =
                                Map.fromList
                                  [
                                    _×_
                                      Bits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bounded
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Enum
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Eq
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      FiniteBits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Integral
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ix
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Num
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ord
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Read
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      ReadRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Real
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      StaticSize
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}},
                          structFieldOffset = 16,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field_2",
                          commentLocation = Just
                            "distilled_lib_1.h:39:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_typedef_struct_t_field_3",
                      fieldType = HsExtBinding
                        ExtRef {
                          extRefModule = ModuleName
                            "HsBindgen.Runtime.Prelude",
                          extRefIdentifier = Identifier
                            "Word32"}
                        CTypeSpec {
                          cTypeSpecModule = Just
                            (ModuleName
                              "HsBindgen.Runtime.Prelude"),
                          cTypeSpecIdentifier = Just
                            (Identifier "Word32"),
                          cTypeSpecInstances =
                          Map.fromList
                            [
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:40:31",
                            fieldName = NamePair {
                              nameC = Name "field_3",
                              nameHsIdent = Identifier
                                "a_typedef_struct_t_field_3"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint32_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtRef {
                                extRefModule = ModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extRefIdentifier = Identifier
                                  "Word32"},
                              extHsSpec = CTypeSpec {
                                cTypeSpecModule = Just
                                  (ModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                cTypeSpecIdentifier = Just
                                  (Identifier "Word32"),
                                cTypeSpecInstances =
                                Map.fromList
                                  [
                                    _×_
                                      Bits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bounded
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Enum
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Eq
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      FiniteBits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Integral
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ix
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Num
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ord
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Read
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      ReadRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Real
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      StaticSize
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}},
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field_3",
                          commentLocation = Just
                            "distilled_lib_1.h:40:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_typedef_struct_t_field_4",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Another_typedef_struct_t"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:41:31",
                            fieldName = NamePair {
                              nameC = Name "field_4",
                              nameHsIdent = Identifier
                                "a_typedef_struct_t_field_4"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefSquashed
                              (Name
                                "another_typedef_struct_t")
                              (TypeStruct
                                NamePair {
                                  nameC = Name
                                    "another_typedef_struct_t",
                                  nameHsIdent = Identifier
                                    "Another_typedef_struct_t"}
                                (NameOriginGenerated
                                  (AnonId
                                    "distilled_lib_1.h:9:9")))),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field_4",
                          commentLocation = Just
                            "distilled_lib_1.h:41:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_typedef_struct_t_field_5",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name
                            "@NsTypeConstr"
                            "Another_typedef_struct_t")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:42:31",
                            fieldName = NamePair {
                              nameC = Name "field_5",
                              nameHsIdent = Identifier
                                "a_typedef_struct_t_field_5"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeTypedef
                              (TypedefSquashed
                                (Name
                                  "another_typedef_struct_t")
                                (TypeStruct
                                  NamePair {
                                    nameC = Name
                                      "another_typedef_struct_t",
                                    nameHsIdent = Identifier
                                      "Another_typedef_struct_t"}
                                  (NameOriginGenerated
                                    (AnonId
                                      "distilled_lib_1.h:9:9"))))),
                          structFieldOffset = 128,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field_5",
                          commentLocation = Just
                            "distilled_lib_1.h:42:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_typedef_struct_t_field_6",
                      fieldType = HsPtr
                        (HsPrimType HsPrimVoid),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:43:31",
                            fieldName = NamePair {
                              nameC = Name "field_6",
                              nameHsIdent = Identifier
                                "a_typedef_struct_t_field_6"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            TypeVoid,
                          structFieldOffset = 192,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field_6",
                          commentLocation = Just
                            "distilled_lib_1.h:43:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_typedef_struct_t_field_7",
                      fieldType = HsConstArray
                        7
                        (HsExtBinding
                          ExtRef {
                            extRefModule = ModuleName
                              "HsBindgen.Runtime.Prelude",
                            extRefIdentifier = Identifier
                              "Word32"}
                          CTypeSpec {
                            cTypeSpecModule = Just
                              (ModuleName
                                "HsBindgen.Runtime.Prelude"),
                            cTypeSpecIdentifier = Just
                              (Identifier "Word32"),
                            cTypeSpecInstances =
                            Map.fromList
                              [
                                _×_
                                  Bits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bounded
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Enum
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Eq
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  FiniteBits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Integral
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ix
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Num
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ord
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Read
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  ReadRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Real
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  StaticSize
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]}),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:44:31",
                            fieldName = NamePair {
                              nameC = Name "field_7",
                              nameHsIdent = Identifier
                                "a_typedef_struct_t_field_7"},
                            fieldComment = Nothing},
                          structFieldType = TypeConstArray
                            7
                            (TypeExtBinding
                              ResolvedExtBinding {
                                extCName = QualName {
                                  qualNameName = Name "uint32_t",
                                  qualNameKind =
                                  NameKindOrdinary},
                                extHsRef = ExtRef {
                                  extRefModule = ModuleName
                                    "HsBindgen.Runtime.Prelude",
                                  extRefIdentifier = Identifier
                                    "Word32"},
                                extHsSpec = CTypeSpec {
                                  cTypeSpecModule = Just
                                    (ModuleName
                                      "HsBindgen.Runtime.Prelude"),
                                  cTypeSpecIdentifier = Just
                                    (Identifier "Word32"),
                                  cTypeSpecInstances =
                                  Map.fromList
                                    [
                                      _×_
                                        Bits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Bounded
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Enum
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Eq
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        FiniteBits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Integral
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ix
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Num
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ord
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Read
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        ReadRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Real
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Show
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        StaticSize
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Storable
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        WriteRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = [
                                              ]})]}}),
                          structFieldOffset = 256,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field_7",
                          commentLocation = Just
                            "distilled_lib_1.h:44:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_typedef_struct_t_field_8",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Another_typedef_enum_e"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:45:31",
                            fieldName = NamePair {
                              nameC = Name "field_8",
                              nameHsIdent = Identifier
                                "a_typedef_struct_t_field_8"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefSquashed
                              (Name "another_typedef_enum_e")
                              (TypeEnum
                                NamePair {
                                  nameC = Name
                                    "another_typedef_enum_e",
                                  nameHsIdent = Identifier
                                    "Another_typedef_enum_e"}
                                (NameOriginGenerated
                                  (AnonId
                                    "distilled_lib_1.h:10:9")))),
                          structFieldOffset = 480,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field_8",
                          commentLocation = Just
                            "distilled_lib_1.h:45:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_typedef_struct_t_field_9",
                      fieldType = HsConstArray
                        4
                        (HsTypRef
                          (Name
                            "@NsTypeConstr"
                            "Another_typedef_enum_e")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:46:31",
                            fieldName = NamePair {
                              nameC = Name "field_9",
                              nameHsIdent = Identifier
                                "a_typedef_struct_t_field_9"},
                            fieldComment = Nothing},
                          structFieldType = TypeConstArray
                            4
                            (TypeTypedef
                              (TypedefSquashed
                                (Name "another_typedef_enum_e")
                                (TypeEnum
                                  NamePair {
                                    nameC = Name
                                      "another_typedef_enum_e",
                                    nameHsIdent = Identifier
                                      "Another_typedef_enum_e"}
                                  (NameOriginGenerated
                                    (AnonId
                                      "distilled_lib_1.h:10:9"))))),
                          structFieldOffset = 512,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field_9",
                          commentLocation = Just
                            "distilled_lib_1.h:46:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_typedef_struct_t_field_10",
                      fieldType = HsConstArray
                        5
                        (HsConstArray
                          3
                          (HsTypRef
                            (Name
                              "@NsTypeConstr"
                              "Another_typedef_enum_e"))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "distilled_lib_1.h:47:31",
                            fieldName = NamePair {
                              nameC = Name "field_10",
                              nameHsIdent = Identifier
                                "a_typedef_struct_t_field_10"},
                            fieldComment = Nothing},
                          structFieldType = TypeConstArray
                            5
                            (TypeConstArray
                              3
                              (TypeTypedef
                                (TypedefSquashed
                                  (Name "another_typedef_enum_e")
                                  (TypeEnum
                                    NamePair {
                                      nameC = Name
                                        "another_typedef_enum_e",
                                      nameHsIdent = Identifier
                                        "Another_typedef_enum_e"}
                                    (NameOriginGenerated
                                      (AnonId
                                        "distilled_lib_1.h:10:9")))))),
                          structFieldOffset = 640,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field_10",
                          commentLocation = Just
                            "distilled_lib_1.h:47:31",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["distilled_lib_1.h"],
                              headerInclude =
                              "distilled_lib_1.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "distilled_lib_1.h:35:16",
                        declId = NamePair {
                          nameC = Name
                            "a_typedef_struct_t",
                          nameHsIdent = Identifier
                            "A_typedef_struct_t"},
                        declOrigin =
                        NameOriginRenamedFrom
                          (Name "a_typedef_struct"),
                        declAliases = [
                          Name "a_typedef_struct_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["distilled_lib_1.h"],
                            headerInclude =
                            "distilled_lib_1.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "A_typedef_struct_t"),
                          structSizeof = 140,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:37:31",
                                fieldName = NamePair {
                                  nameC = Name "field_0",
                                  nameHsIdent = Identifier
                                    "a_typedef_struct_t_field_0"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                PrimBool,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:38:31",
                                fieldName = NamePair {
                                  nameC = Name "field_1",
                                  nameHsIdent = Identifier
                                    "a_typedef_struct_t_field_1"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint8_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtRef {
                                    extRefModule = ModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extRefIdentifier = Identifier
                                      "Word8"},
                                  extHsSpec = CTypeSpec {
                                    cTypeSpecModule = Just
                                      (ModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    cTypeSpecIdentifier = Just
                                      (Identifier "Word8"),
                                    cTypeSpecInstances =
                                    Map.fromList
                                      [
                                        _×_
                                          Bits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bounded
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Enum
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Eq
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          FiniteBits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Integral
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ix
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Num
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ord
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Read
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          ReadRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Real
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          StaticSize
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 8,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:39:31",
                                fieldName = NamePair {
                                  nameC = Name "field_2",
                                  nameHsIdent = Identifier
                                    "a_typedef_struct_t_field_2"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint16_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtRef {
                                    extRefModule = ModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extRefIdentifier = Identifier
                                      "Word16"},
                                  extHsSpec = CTypeSpec {
                                    cTypeSpecModule = Just
                                      (ModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    cTypeSpecIdentifier = Just
                                      (Identifier "Word16"),
                                    cTypeSpecInstances =
                                    Map.fromList
                                      [
                                        _×_
                                          Bits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bounded
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Enum
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Eq
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          FiniteBits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Integral
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ix
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Num
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ord
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Read
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          ReadRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Real
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          StaticSize
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 16,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:40:31",
                                fieldName = NamePair {
                                  nameC = Name "field_3",
                                  nameHsIdent = Identifier
                                    "a_typedef_struct_t_field_3"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint32_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtRef {
                                    extRefModule = ModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extRefIdentifier = Identifier
                                      "Word32"},
                                  extHsSpec = CTypeSpec {
                                    cTypeSpecModule = Just
                                      (ModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    cTypeSpecIdentifier = Just
                                      (Identifier "Word32"),
                                    cTypeSpecInstances =
                                    Map.fromList
                                      [
                                        _×_
                                          Bits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bounded
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Enum
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Eq
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          FiniteBits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Integral
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ix
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Num
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ord
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Read
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          ReadRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Real
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          StaticSize
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 32,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:41:31",
                                fieldName = NamePair {
                                  nameC = Name "field_4",
                                  nameHsIdent = Identifier
                                    "a_typedef_struct_t_field_4"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefSquashed
                                  (Name
                                    "another_typedef_struct_t")
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name
                                        "another_typedef_struct_t",
                                      nameHsIdent = Identifier
                                        "Another_typedef_struct_t"}
                                    (NameOriginGenerated
                                      (AnonId
                                        "distilled_lib_1.h:9:9")))),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:42:31",
                                fieldName = NamePair {
                                  nameC = Name "field_5",
                                  nameHsIdent = Identifier
                                    "a_typedef_struct_t_field_5"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeTypedef
                                  (TypedefSquashed
                                    (Name
                                      "another_typedef_struct_t")
                                    (TypeStruct
                                      NamePair {
                                        nameC = Name
                                          "another_typedef_struct_t",
                                        nameHsIdent = Identifier
                                          "Another_typedef_struct_t"}
                                      (NameOriginGenerated
                                        (AnonId
                                          "distilled_lib_1.h:9:9"))))),
                              structFieldOffset = 128,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:43:31",
                                fieldName = NamePair {
                                  nameC = Name "field_6",
                                  nameHsIdent = Identifier
                                    "a_typedef_struct_t_field_6"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                TypeVoid,
                              structFieldOffset = 192,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:44:31",
                                fieldName = NamePair {
                                  nameC = Name "field_7",
                                  nameHsIdent = Identifier
                                    "a_typedef_struct_t_field_7"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                7
                                (TypeExtBinding
                                  ResolvedExtBinding {
                                    extCName = QualName {
                                      qualNameName = Name "uint32_t",
                                      qualNameKind =
                                      NameKindOrdinary},
                                    extHsRef = ExtRef {
                                      extRefModule = ModuleName
                                        "HsBindgen.Runtime.Prelude",
                                      extRefIdentifier = Identifier
                                        "Word32"},
                                    extHsSpec = CTypeSpec {
                                      cTypeSpecModule = Just
                                        (ModuleName
                                          "HsBindgen.Runtime.Prelude"),
                                      cTypeSpecIdentifier = Just
                                        (Identifier "Word32"),
                                      cTypeSpecInstances =
                                      Map.fromList
                                        [
                                          _×_
                                            Bits
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Bounded
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Enum
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Eq
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            FiniteBits
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Integral
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Ix
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Num
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Ord
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Read
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            ReadRaw
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Real
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Show
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            StaticSize
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            Storable
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = []}),
                                          _×_
                                            WriteRaw
                                            (Require
                                              InstanceSpec {
                                                instanceSpecStrategy = Nothing,
                                                instanceSpecConstraints = [
                                                  ]})]}}),
                              structFieldOffset = 256,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:45:31",
                                fieldName = NamePair {
                                  nameC = Name "field_8",
                                  nameHsIdent = Identifier
                                    "a_typedef_struct_t_field_8"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefSquashed
                                  (Name "another_typedef_enum_e")
                                  (TypeEnum
                                    NamePair {
                                      nameC = Name
                                        "another_typedef_enum_e",
                                      nameHsIdent = Identifier
                                        "Another_typedef_enum_e"}
                                    (NameOriginGenerated
                                      (AnonId
                                        "distilled_lib_1.h:10:9")))),
                              structFieldOffset = 480,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:46:31",
                                fieldName = NamePair {
                                  nameC = Name "field_9",
                                  nameHsIdent = Identifier
                                    "a_typedef_struct_t_field_9"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                4
                                (TypeTypedef
                                  (TypedefSquashed
                                    (Name "another_typedef_enum_e")
                                    (TypeEnum
                                      NamePair {
                                        nameC = Name
                                          "another_typedef_enum_e",
                                        nameHsIdent = Identifier
                                          "Another_typedef_enum_e"}
                                      (NameOriginGenerated
                                        (AnonId
                                          "distilled_lib_1.h:10:9"))))),
                              structFieldOffset = 512,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "distilled_lib_1.h:47:31",
                                fieldName = NamePair {
                                  nameC = Name "field_10",
                                  nameHsIdent = Identifier
                                    "a_typedef_struct_t_field_10"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                5
                                (TypeConstArray
                                  3
                                  (TypeTypedef
                                    (TypedefSquashed
                                      (Name "another_typedef_enum_e")
                                      (TypeEnum
                                        NamePair {
                                          nameC = Name
                                            "another_typedef_enum_e",
                                          nameHsIdent = Identifier
                                            "Another_typedef_enum_e"}
                                        (NameOriginGenerated
                                          (AnonId
                                            "distilled_lib_1.h:10:9")))))),
                              structFieldOffset = 640,
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
                      commentOrigin = Just
                        "a_typedef_struct_t",
                      commentLocation = Just
                        "distilled_lib_1.h:35:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["distilled_lib_1.h"],
                          headerInclude =
                          "distilled_lib_1.h"},
                      commentChildren = []}}
                (Add 11)
                (Seq
                  [
                    PokeByteOff (Idx 12) 0 (Idx 0),
                    PokeByteOff (Idx 12) 1 (Idx 1),
                    PokeByteOff (Idx 12) 2 (Idx 2),
                    PokeByteOff (Idx 12) 4 (Idx 3),
                    PokeByteOff (Idx 12) 8 (Idx 4),
                    PokeByteOff (Idx 12) 16 (Idx 5),
                    PokeByteOff (Idx 12) 24 (Idx 6),
                    PokeByteOff (Idx 12) 32 (Idx 7),
                    PokeByteOff (Idx 12) 60 (Idx 8),
                    PokeByteOff (Idx 12) 64 (Idx 9),
                    PokeByteOff
                      (Idx 12)
                      80
                      (Idx 10)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "A_typedef_struct_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "A_typedef_struct_t",
      deriveInstanceComment =
      Nothing},
  DeclVar
    VarDecl {
      varDeclName = Name
        "@NsVar"
        "a_DEFINE_0",
      varDeclType = ForallTy {
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = TyConAppTy
            (ATyCon
              (GenerativeTyCon
                (DataTyCon IntLikeTyCon)))
            [
              TyConAppTy
                (ATyCon
                  (GenerativeTyCon
                    (DataTyCon
                      (IntLikeTyCon
                        (CIntegralType
                          (IntLike (Int Signed)))))))
                []]}},
      varDeclBody = VarDeclIntegral
        0
        HsPrimCInt,
      varDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "A_DEFINE_0",
          commentLocation = Just
            "distilled_lib_1.h:53:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          commentChildren = []}},
  DeclVar
    VarDecl {
      varDeclName = Name
        "@NsVar"
        "a_DEFINE_1",
      varDeclType = ForallTy {
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = TyConAppTy
            (ATyCon
              (GenerativeTyCon
                (DataTyCon IntLikeTyCon)))
            [
              TyConAppTy
                (ATyCon
                  (GenerativeTyCon
                    (DataTyCon
                      (IntLikeTyCon
                        (CIntegralType
                          (IntLike (Int Unsigned)))))))
                []]}},
      varDeclBody = VarDeclIntegral
        20560
        HsPrimCUInt,
      varDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "A_DEFINE_1",
          commentLocation = Just
            "distilled_lib_1.h:54:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          commentChildren = []}},
  DeclVar
    VarDecl {
      varDeclName = Name
        "@NsVar"
        "a_DEFINE_2",
      varDeclType = ForallTy {
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = TyConAppTy
            (ATyCon
              (GenerativeTyCon
                (DataTyCon IntLikeTyCon)))
            [
              TyConAppTy
                (ATyCon
                  (GenerativeTyCon
                    (DataTyCon
                      (IntLikeTyCon
                        (CIntegralType
                          (IntLike (Int Signed)))))))
                []]}},
      varDeclBody = VarDeclIntegral
        2
        HsPrimCInt,
      varDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "A_DEFINE_2",
          commentLocation = Just
            "distilled_lib_1.h:55:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          commentChildren = []}},
  DeclVar
    VarDecl {
      varDeclName = Name
        "@NsVar"
        "tWO_ARGS",
      varDeclType = ForallTy {
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = TyConAppTy
            (ATyCon
              (GenerativeTyCon
                (DataTyCon (TupleTyCon 2))))
            [
              TyConAppTy
                (ATyCon
                  (GenerativeTyCon
                    (DataTyCon IntLikeTyCon)))
                [
                  TyConAppTy
                    (ATyCon
                      (GenerativeTyCon
                        (DataTyCon
                          (IntLikeTyCon
                            (CIntegralType
                              (IntLike (Int Signed)))))))
                    []],
              TyConAppTy
                (ATyCon
                  (GenerativeTyCon
                    (DataTyCon IntLikeTyCon)))
                [
                  TyConAppTy
                    (ATyCon
                      (GenerativeTyCon
                        (DataTyCon
                          (IntLikeTyCon
                            (CIntegralType
                              (IntLike (Int Signed)))))))
                    []]]}},
      varDeclBody = VarDeclApp
        (InfixAppHead MTuple)
        [
          VarDeclIntegral
            13398
            HsPrimCInt,
          VarDeclIntegral
            30874
            HsPrimCInt],
      varDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "TWO_ARGS",
          commentLocation = Just
            "distilled_lib_1.h:56:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          commentChildren = []}},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "A_typedef_enum_e",
      newtypeConstr = Name
        "@NsConstr"
        "A_typedef_enum_e",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_A_typedef_enum_e",
        fieldType = HsPrimType
          HsPrimCUChar,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "distilled_lib_1.h:61:9",
          declId = NamePair {
            nameC = Name "a_typedef_enum_e",
            nameHsIdent = Identifier
              "A_typedef_enum_e"},
          declOrigin = NameOriginGenerated
            (AnonId
              "distilled_lib_1.h:61:9"),
          declAliases = [
            Name "a_typedef_enum_e"],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          declComment = Nothing},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "A_typedef_enum_e",
              newtypeField = Name
                "@NsVar"
                "un_A_typedef_enum_e"},
            enumType = TypePrim
              (PrimChar
                (PrimSignExplicit Unsigned)),
            enumSizeof = 1,
            enumAlignment = 1,
            enumConstants = [
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "distilled_lib_1.h:63:3",
                  fieldName = NamePair {
                    nameC = Name "ENUM_CASE_0",
                    nameHsIdent = Identifier
                      "ENUM_CASE_0"},
                  fieldComment = Nothing},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "distilled_lib_1.h:64:3",
                  fieldName = NamePair {
                    nameC = Name "ENUM_CASE_1",
                    nameHsIdent = Identifier
                      "ENUM_CASE_1"},
                  fieldComment = Nothing},
                enumConstantValue = 1},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "distilled_lib_1.h:65:3",
                  fieldName = NamePair {
                    nameC = Name "ENUM_CASE_2",
                    nameHsIdent = Identifier
                      "ENUM_CASE_2"},
                  fieldComment = Nothing},
                enumConstantValue = 2},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "distilled_lib_1.h:66:3",
                  fieldName = NamePair {
                    nameC = Name "ENUM_CASE_3",
                    nameHsIdent = Identifier
                      "ENUM_CASE_3"},
                  fieldComment = Nothing},
                enumConstantValue = 3}]},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Read, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Just
            "distilled_lib_1.h:61:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "A_typedef_enum_e",
          structConstr = Name
            "@NsConstr"
            "A_typedef_enum_e",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_A_typedef_enum_e",
              fieldType = HsPrimType
                HsPrimCUChar,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
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
                    "A_typedef_enum_e",
                  structConstr = Name
                    "@NsConstr"
                    "A_typedef_enum_e",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "un_A_typedef_enum_e",
                      fieldType = HsPrimType
                        HsPrimCUChar,
                      fieldOrigin = GeneratedField,
                      fieldComment = Nothing}],
                  structOrigin = Nothing,
                  structInstances = Set.fromList
                    [Eq, Ord, Read, Show, Storable],
                  structComment = Nothing})
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
                    "A_typedef_enum_e",
                  structConstr = Name
                    "@NsConstr"
                    "A_typedef_enum_e",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "un_A_typedef_enum_e",
                      fieldType = HsPrimType
                        HsPrimCUChar,
                      fieldOrigin = GeneratedField,
                      fieldComment = Nothing}],
                  structOrigin = Nothing,
                  structInstances = Set.fromList
                    [Eq, Ord, Read, Show, Storable],
                  structComment = Nothing}
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
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "A_typedef_enum_e",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "A_typedef_enum_e",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
        Struct {
          structName = Name
            "@NsTypeConstr"
            "A_typedef_enum_e",
          structConstr = Name
            "@NsConstr"
            "A_typedef_enum_e",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_A_typedef_enum_e",
              fieldType = HsPrimType
                HsPrimCUChar,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (HsPrimType HsPrimCUChar)
        (Map.fromList
          [
            _×_
              0
              (NE.fromList ["ENUM_CASE_0"]),
            _×_
              1
              (NE.fromList ["ENUM_CASE_1"]),
            _×_
              2
              (NE.fromList ["ENUM_CASE_2"]),
            _×_
              3
              (NE.fromList ["ENUM_CASE_3"])])
        True,
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceSequentialCEnum
        Struct {
          structName = Name
            "@NsTypeConstr"
            "A_typedef_enum_e",
          structConstr = Name
            "@NsConstr"
            "A_typedef_enum_e",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_A_typedef_enum_e",
              fieldType = HsPrimType
                HsPrimCUChar,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (Name "@NsConstr" "ENUM_CASE_0")
        (Name
          "@NsConstr"
          "ENUM_CASE_3"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
        Struct {
          structName = Name
            "@NsTypeConstr"
            "A_typedef_enum_e",
          structConstr = Name
            "@NsConstr"
            "A_typedef_enum_e",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_A_typedef_enum_e",
              fieldType = HsPrimType
                HsPrimCUChar,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumRead
        Struct {
          structName = Name
            "@NsTypeConstr"
            "A_typedef_enum_e",
          structConstr = Name
            "@NsConstr"
            "A_typedef_enum_e",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_A_typedef_enum_e",
              fieldType = HsPrimType
                HsPrimCUChar,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing},
      defineInstanceComment =
      Nothing},
  DeclPatSyn
    PatSyn {
      patSynName = Name
        "@NsConstr"
        "ENUM_CASE_0",
      patSynType = Name
        "@NsTypeConstr"
        "A_typedef_enum_e",
      patSynConstr = Name
        "@NsConstr"
        "A_typedef_enum_e",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "distilled_lib_1.h:63:3",
            fieldName = NamePair {
              nameC = Name "ENUM_CASE_0",
              nameHsIdent = Identifier
                "ENUM_CASE_0"},
            fieldComment = Nothing},
          enumConstantValue = 0},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ENUM_CASE_0",
          commentLocation = Just
            "distilled_lib_1.h:63:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          commentChildren = []}},
  DeclPatSyn
    PatSyn {
      patSynName = Name
        "@NsConstr"
        "ENUM_CASE_1",
      patSynType = Name
        "@NsTypeConstr"
        "A_typedef_enum_e",
      patSynConstr = Name
        "@NsConstr"
        "A_typedef_enum_e",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "distilled_lib_1.h:64:3",
            fieldName = NamePair {
              nameC = Name "ENUM_CASE_1",
              nameHsIdent = Identifier
                "ENUM_CASE_1"},
            fieldComment = Nothing},
          enumConstantValue = 1},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ENUM_CASE_1",
          commentLocation = Just
            "distilled_lib_1.h:64:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          commentChildren = []}},
  DeclPatSyn
    PatSyn {
      patSynName = Name
        "@NsConstr"
        "ENUM_CASE_2",
      patSynType = Name
        "@NsTypeConstr"
        "A_typedef_enum_e",
      patSynConstr = Name
        "@NsConstr"
        "A_typedef_enum_e",
      patSynValue = 2,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "distilled_lib_1.h:65:3",
            fieldName = NamePair {
              nameC = Name "ENUM_CASE_2",
              nameHsIdent = Identifier
                "ENUM_CASE_2"},
            fieldComment = Nothing},
          enumConstantValue = 2},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ENUM_CASE_2",
          commentLocation = Just
            "distilled_lib_1.h:65:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          commentChildren = []}},
  DeclPatSyn
    PatSyn {
      patSynName = Name
        "@NsConstr"
        "ENUM_CASE_3",
      patSynType = Name
        "@NsTypeConstr"
        "A_typedef_enum_e",
      patSynConstr = Name
        "@NsConstr"
        "A_typedef_enum_e",
      patSynValue = 3,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "distilled_lib_1.h:66:3",
            fieldName = NamePair {
              nameC = Name "ENUM_CASE_3",
              nameHsIdent = Identifier
                "ENUM_CASE_3"},
            fieldComment = Nothing},
          enumConstantValue = 3},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ENUM_CASE_3",
          commentLocation = Just
            "distilled_lib_1.h:66:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          commentChildren = []}},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Callback_t_Deref",
      newtypeConstr = Name
        "@NsConstr"
        "Callback_t_Deref",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Callback_t_Deref",
        fieldType = HsFun
          (HsPtr (HsPrimType HsPrimVoid))
          (HsFun
            (HsExtBinding
              ExtRef {
                extRefModule = ModuleName
                  "HsBindgen.Runtime.Prelude",
                extRefIdentifier = Identifier
                  "Word32"}
              CTypeSpec {
                cTypeSpecModule = Just
                  (ModuleName
                    "HsBindgen.Runtime.Prelude"),
                cTypeSpecIdentifier = Just
                  (Identifier "Word32"),
                cTypeSpecInstances =
                Map.fromList
                  [
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]})
            (HsIO
              (HsExtBinding
                ExtRef {
                  extRefModule = ModuleName
                    "HsBindgen.Runtime.Prelude",
                  extRefIdentifier = Identifier
                    "Word32"}
                CTypeSpec {
                  cTypeSpecModule = Just
                    (ModuleName
                      "HsBindgen.Runtime.Prelude"),
                  cTypeSpecIdentifier = Just
                    (Identifier "Word32"),
                  cTypeSpecInstances =
                  Map.fromList
                    [
                      _×_
                        Bits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bounded
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Enum
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Eq
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        FiniteBits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Integral
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ix
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Num
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ord
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Read
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        ReadRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Real
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        StaticSize
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]}))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "distilled_lib_1.h:77:19",
          declId = NamePair {
            nameC = Name "callback_t_Deref",
            nameHsIdent = Identifier
              "Callback_t_Deref"},
          declOrigin = NameOriginGenerated
            (AnonId
              "distilled_lib_1.h:77:19"),
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          declComment = Just
            (Comment
              [
                Paragraph
                  [
                    TextContent
                      "Auxiliary type used by ",
                    InlineRefCommand
                      (ById
                        NamePair {
                          nameC = Name "callback_t",
                          nameHsIdent = Identifier
                            "Callback_t"})]])},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Callback_t_Deref",
              newtypeField = Name
                "@NsVar"
                "un_Callback_t_Deref"},
            typedefType = TypeFun
              [
                TypePointer TypeVoid,
                TypeExtBinding
                  ResolvedExtBinding {
                    extCName = QualName {
                      qualNameName = Name "uint32_t",
                      qualNameKind =
                      NameKindOrdinary},
                    extHsRef = ExtRef {
                      extRefModule = ModuleName
                        "HsBindgen.Runtime.Prelude",
                      extRefIdentifier = Identifier
                        "Word32"},
                    extHsSpec = CTypeSpec {
                      cTypeSpecModule = Just
                        (ModuleName
                          "HsBindgen.Runtime.Prelude"),
                      cTypeSpecIdentifier = Just
                        (Identifier "Word32"),
                      cTypeSpecInstances =
                      Map.fromList
                        [
                          _×_
                            Bits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bounded
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Enum
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Eq
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            FiniteBits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Integral
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ix
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Num
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ord
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Read
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            ReadRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Real
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Show
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            StaticSize
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Storable
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            WriteRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = [
                                  ]})]}}]
              (TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "uint32_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtRef {
                    extRefModule = ModuleName
                      "HsBindgen.Runtime.Prelude",
                    extRefIdentifier = Identifier
                      "Word32"},
                  extHsSpec = CTypeSpec {
                    cTypeSpecModule = Just
                      (ModuleName
                        "HsBindgen.Runtime.Prelude"),
                    cTypeSpecIdentifier = Just
                      (Identifier "Word32"),
                    cTypeSpecInstances =
                    Map.fromList
                      [
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}})},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [],
      newtypeComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Auxiliary type used by",
              Identifier "Callback_t"],
          commentOrigin = Nothing,
          commentLocation = Just
            "distilled_lib_1.h:77:19",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "toCallback_t_Deref",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Callback_t_Deref"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Callback_t_Deref")))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = ToFunPtr
        (TypeFun
          [
            TypePointer TypeVoid,
            TypeExtBinding
              ResolvedExtBinding {
                extCName = QualName {
                  qualNameName = Name "uint32_t",
                  qualNameKind =
                  NameKindOrdinary},
                extHsRef = ExtRef {
                  extRefModule = ModuleName
                    "HsBindgen.Runtime.Prelude",
                  extRefIdentifier = Identifier
                    "Word32"},
                extHsSpec = CTypeSpec {
                  cTypeSpecModule = Just
                    (ModuleName
                      "HsBindgen.Runtime.Prelude"),
                  cTypeSpecIdentifier = Just
                    (Identifier "Word32"),
                  cTypeSpecInstances =
                  Map.fromList
                    [
                      _×_
                        Bits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bounded
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Enum
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Eq
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        FiniteBits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Integral
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ix
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Num
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ord
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Read
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        ReadRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Real
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        StaticSize
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]}}]
          (TypeExtBinding
            ResolvedExtBinding {
              extCName = QualName {
                qualNameName = Name "uint32_t",
                qualNameKind =
                NameKindOrdinary},
              extHsRef = ExtRef {
                extRefModule = ModuleName
                  "HsBindgen.Runtime.Prelude",
                extRefIdentifier = Identifier
                  "Word32"},
              extHsSpec = CTypeSpec {
                cTypeSpecModule = Just
                  (ModuleName
                    "HsBindgen.Runtime.Prelude"),
                cTypeSpecIdentifier = Just
                  (Identifier "Word32"),
                cTypeSpecInstances =
                Map.fromList
                  [
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]}})),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fromCallback_t_Deref",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Callback_t_Deref")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsTypRef
          (Name
            "@NsTypeConstr"
            "Callback_t_Deref")),
      foreignImportOrigName =
      "dynamic",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = FromFunPtr
        (TypeFun
          [
            TypePointer TypeVoid,
            TypeExtBinding
              ResolvedExtBinding {
                extCName = QualName {
                  qualNameName = Name "uint32_t",
                  qualNameKind =
                  NameKindOrdinary},
                extHsRef = ExtRef {
                  extRefModule = ModuleName
                    "HsBindgen.Runtime.Prelude",
                  extRefIdentifier = Identifier
                    "Word32"},
                extHsSpec = CTypeSpec {
                  cTypeSpecModule = Just
                    (ModuleName
                      "HsBindgen.Runtime.Prelude"),
                  cTypeSpecIdentifier = Just
                    (Identifier "Word32"),
                  cTypeSpecInstances =
                  Map.fromList
                    [
                      _×_
                        Bits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bounded
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Enum
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Eq
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        FiniteBits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Integral
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ix
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Num
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ord
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Read
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        ReadRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Real
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        StaticSize
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]}}]
          (TypeExtBinding
            ResolvedExtBinding {
              extCName = QualName {
                qualNameName = Name "uint32_t",
                qualNameKind =
                NameKindOrdinary},
              extHsRef = ExtRef {
                extRefModule = ModuleName
                  "HsBindgen.Runtime.Prelude",
                extRefIdentifier = Identifier
                  "Word32"},
              extHsSpec = CTypeSpec {
                cTypeSpecModule = Just
                  (ModuleName
                    "HsBindgen.Runtime.Prelude"),
                cTypeSpecIdentifier = Just
                  (Identifier "Word32"),
                cTypeSpecInstances =
                Map.fromList
                  [
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]}})),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceToFunPtr
        ToFunPtrInstance {
          toFunPtrInstanceType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Callback_t_Deref"),
          toFunPtrInstanceBody = Name
            "@NsVar"
            "toCallback_t_Deref"},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceFromFunPtr
        FromFunPtrInstance {
          fromFunPtrInstanceType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Callback_t_Deref"),
          fromFunPtrInstanceBody = Name
            "@NsVar"
            "fromCallback_t_Deref"},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Callback_t",
      newtypeConstr = Name
        "@NsConstr"
        "Callback_t",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Callback_t",
        fieldType = HsFunPtr
          (HsTypRef
            (Name
              "@NsTypeConstr"
              "Callback_t_Deref")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "distilled_lib_1.h:77:19",
          declId = NamePair {
            nameC = Name "callback_t",
            nameHsIdent = Identifier
              "Callback_t"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Callback_t",
              newtypeField = Name
                "@NsVar"
                "un_Callback_t"},
            typedefType = TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "callback_t_Deref",
                    nameHsIdent = Identifier
                      "Callback_t_Deref"}
                  (TypeFun
                    [
                      TypePointer TypeVoid,
                      TypeExtBinding
                        ResolvedExtBinding {
                          extCName = QualName {
                            qualNameName = Name "uint32_t",
                            qualNameKind =
                            NameKindOrdinary},
                          extHsRef = ExtRef {
                            extRefModule = ModuleName
                              "HsBindgen.Runtime.Prelude",
                            extRefIdentifier = Identifier
                              "Word32"},
                          extHsSpec = CTypeSpec {
                            cTypeSpecModule = Just
                              (ModuleName
                                "HsBindgen.Runtime.Prelude"),
                            cTypeSpecIdentifier = Just
                              (Identifier "Word32"),
                            cTypeSpecInstances =
                            Map.fromList
                              [
                                _×_
                                  Bits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bounded
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Enum
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Eq
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  FiniteBits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Integral
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ix
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Num
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ord
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Read
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  ReadRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Real
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  StaticSize
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]}}]
                    (TypeExtBinding
                      ResolvedExtBinding {
                        extCName = QualName {
                          qualNameName = Name "uint32_t",
                          qualNameKind =
                          NameKindOrdinary},
                        extHsRef = ExtRef {
                          extRefModule = ModuleName
                            "HsBindgen.Runtime.Prelude",
                          extRefIdentifier = Identifier
                            "Word32"},
                        extHsSpec = CTypeSpec {
                          cTypeSpecModule = Just
                            (ModuleName
                              "HsBindgen.Runtime.Prelude"),
                          cTypeSpecIdentifier = Just
                            (Identifier "Word32"),
                          cTypeSpecInstances =
                          Map.fromList
                            [
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]}}))))},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "callback_t",
          commentLocation = Just
            "distilled_lib_1.h:77:19",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Callback_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Callback_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Callback_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Callback_t",
      deriveInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "some_fun",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "i"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "A_type_t")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "i",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "j"),
          functionParameterType =
          HsExtBinding
            ExtRef {
              extRefModule = ModuleName
                "HsBindgen.Runtime.Prelude",
              extRefIdentifier = Identifier
                "Word32"}
            CTypeSpec {
              cTypeSpecModule = Just
                (ModuleName
                  "HsBindgen.Runtime.Prelude"),
              cTypeSpecIdentifier = Just
                (Identifier "Word32"),
              cTypeSpecInstances =
              Map.fromList
                [
                  _×_
                    Bits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bounded
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Enum
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Eq
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    FiniteBits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Integral
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ix
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Num
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ord
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Read
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    ReadRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Real
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    StaticSize
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]},
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "j",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "k"),
          functionParameterType = HsPtr
            (HsExtBinding
              ExtRef {
                extRefModule = ModuleName
                  "HsBindgen.Runtime.Prelude",
                extRefIdentifier = Identifier
                  "Word8"}
              CTypeSpec {
                cTypeSpecModule = Just
                  (ModuleName
                    "HsBindgen.Runtime.Prelude"),
                cTypeSpecIdentifier = Just
                  (Identifier "Word8"),
                cTypeSpecInstances =
                Map.fromList
                  [
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]}),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "k",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsExtBinding
            ExtRef {
              extRefModule = ModuleName
                "HsBindgen.Runtime.Prelude",
              extRefIdentifier = Identifier
                "Int32"}
            CTypeSpec {
              cTypeSpecModule = Just
                (ModuleName
                  "HsBindgen.Runtime.Prelude"),
              cTypeSpecIdentifier = Just
                (Identifier "Int32"),
              cTypeSpecInstances =
              Map.fromList
                [
                  _×_
                    Bits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bounded
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Enum
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Eq
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    FiniteBits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Integral
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ix
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Num
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ord
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Read
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    ReadRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Real
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    StaticSize
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]})),
      foreignImportOrigName =
      "hs_bindgen_test_distilled_lib_1_29c178c31334688f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "int32_t hs_bindgen_test_distilled_lib_1_29c178c31334688f (a_type_t *arg1, uint32_t arg2, uint8_t *arg3) { return some_fun(arg1, arg2, arg3); }",
          capiWrapperImport =
          "distilled_lib_1.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "i",
                  nameHsIdent = Identifier "i"})
              (TypePointer
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "a_type_t",
                      nameHsIdent = Identifier
                        "A_type_t"}
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "j",
                  nameHsIdent = Identifier "j"})
              (TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "uint32_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtRef {
                    extRefModule = ModuleName
                      "HsBindgen.Runtime.Prelude",
                    extRefIdentifier = Identifier
                      "Word32"},
                  extHsSpec = CTypeSpec {
                    cTypeSpecModule = Just
                      (ModuleName
                        "HsBindgen.Runtime.Prelude"),
                    cTypeSpecIdentifier = Just
                      (Identifier "Word32"),
                    cTypeSpecInstances =
                    Map.fromList
                      [
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}}),
            _×_
              (Just
                NamePair {
                  nameC = Name "k",
                  nameHsIdent = Identifier "k"})
              (TypeIncompleteArray
                (TypeExtBinding
                  ResolvedExtBinding {
                    extCName = QualName {
                      qualNameName = Name "uint8_t",
                      qualNameKind =
                      NameKindOrdinary},
                    extHsRef = ExtRef {
                      extRefModule = ModuleName
                        "HsBindgen.Runtime.Prelude",
                      extRefIdentifier = Identifier
                        "Word8"},
                    extHsSpec = CTypeSpec {
                      cTypeSpecModule = Just
                        (ModuleName
                          "HsBindgen.Runtime.Prelude"),
                      cTypeSpecIdentifier = Just
                        (Identifier "Word8"),
                      cTypeSpecInstances =
                      Map.fromList
                        [
                          _×_
                            Bits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bounded
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Enum
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Eq
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            FiniteBits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Integral
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ix
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Num
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ord
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Read
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            ReadRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Real
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Show
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            StaticSize
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Storable
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            WriteRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = [
                                  ]})]}}))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeExtBinding
            ResolvedExtBinding {
              extCName = QualName {
                qualNameName = Name "int32_t",
                qualNameKind =
                NameKindOrdinary},
              extHsRef = ExtRef {
                extRefModule = ModuleName
                  "HsBindgen.Runtime.Prelude",
                extRefIdentifier = Identifier
                  "Int32"},
              extHsSpec = CTypeSpec {
                cTypeSpecModule = Just
                  (ModuleName
                    "HsBindgen.Runtime.Prelude"),
                cTypeSpecIdentifier = Just
                  (Identifier "Int32"),
                cTypeSpecInstances =
                Map.fromList
                  [
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]}}},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "some_fun",
          commentLocation = Just
            "distilled_lib_1.h:72:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "some_fun",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "i"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "A_type_t")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "i",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "j"),
          functionParameterType =
          HsExtBinding
            ExtRef {
              extRefModule = ModuleName
                "HsBindgen.Runtime.Prelude",
              extRefIdentifier = Identifier
                "Word32"}
            CTypeSpec {
              cTypeSpecModule = Just
                (ModuleName
                  "HsBindgen.Runtime.Prelude"),
              cTypeSpecIdentifier = Just
                (Identifier "Word32"),
              cTypeSpecInstances =
              Map.fromList
                [
                  _×_
                    Bits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bounded
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Enum
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Eq
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    FiniteBits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Integral
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ix
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Num
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ord
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Read
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    ReadRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Real
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    StaticSize
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]},
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "j",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "k"),
          functionParameterType = HsPtr
            (HsExtBinding
              ExtRef {
                extRefModule = ModuleName
                  "HsBindgen.Runtime.Prelude",
                extRefIdentifier = Identifier
                  "Word8"}
              CTypeSpec {
                cTypeSpecModule = Just
                  (ModuleName
                    "HsBindgen.Runtime.Prelude"),
                cTypeSpecIdentifier = Just
                  (Identifier "Word8"),
                cTypeSpecInstances =
                Map.fromList
                  [
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]}),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "k",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsExtBinding
            ExtRef {
              extRefModule = ModuleName
                "HsBindgen.Runtime.Prelude",
              extRefIdentifier = Identifier
                "Int32"}
            CTypeSpec {
              cTypeSpecModule = Just
                (ModuleName
                  "HsBindgen.Runtime.Prelude"),
              cTypeSpecIdentifier = Just
                (Identifier "Int32"),
              cTypeSpecInstances =
              Map.fromList
                [
                  _×_
                    Bits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bounded
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Enum
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Eq
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    FiniteBits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Integral
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ix
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Num
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ord
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Read
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    ReadRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Real
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    StaticSize
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]})),
      foreignImportOrigName =
      "hs_bindgen_test_distilled_lib_1_efd27157959bd4b3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "int32_t hs_bindgen_test_distilled_lib_1_efd27157959bd4b3 (a_type_t *arg1, uint32_t arg2, uint8_t *arg3) { return some_fun(arg1, arg2, arg3); }",
          capiWrapperImport =
          "distilled_lib_1.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "i",
                  nameHsIdent = Identifier "i"})
              (TypePointer
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "a_type_t",
                      nameHsIdent = Identifier
                        "A_type_t"}
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "j",
                  nameHsIdent = Identifier "j"})
              (TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "uint32_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtRef {
                    extRefModule = ModuleName
                      "HsBindgen.Runtime.Prelude",
                    extRefIdentifier = Identifier
                      "Word32"},
                  extHsSpec = CTypeSpec {
                    cTypeSpecModule = Just
                      (ModuleName
                        "HsBindgen.Runtime.Prelude"),
                    cTypeSpecIdentifier = Just
                      (Identifier "Word32"),
                    cTypeSpecInstances =
                    Map.fromList
                      [
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}}),
            _×_
              (Just
                NamePair {
                  nameC = Name "k",
                  nameHsIdent = Identifier "k"})
              (TypeIncompleteArray
                (TypeExtBinding
                  ResolvedExtBinding {
                    extCName = QualName {
                      qualNameName = Name "uint8_t",
                      qualNameKind =
                      NameKindOrdinary},
                    extHsRef = ExtRef {
                      extRefModule = ModuleName
                        "HsBindgen.Runtime.Prelude",
                      extRefIdentifier = Identifier
                        "Word8"},
                    extHsSpec = CTypeSpec {
                      cTypeSpecModule = Just
                        (ModuleName
                          "HsBindgen.Runtime.Prelude"),
                      cTypeSpecIdentifier = Just
                        (Identifier "Word8"),
                      cTypeSpecInstances =
                      Map.fromList
                        [
                          _×_
                            Bits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bounded
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Enum
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Eq
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            FiniteBits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Integral
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ix
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Num
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ord
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Read
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            ReadRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Real
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Show
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            StaticSize
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Storable
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            WriteRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = [
                                  ]})]}}))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeExtBinding
            ResolvedExtBinding {
              extCName = QualName {
                qualNameName = Name "int32_t",
                qualNameKind =
                NameKindOrdinary},
              extHsRef = ExtRef {
                extRefModule = ModuleName
                  "HsBindgen.Runtime.Prelude",
                extRefIdentifier = Identifier
                  "Int32"},
              extHsSpec = CTypeSpec {
                cTypeSpecModule = Just
                  (ModuleName
                    "HsBindgen.Runtime.Prelude"),
                cTypeSpecIdentifier = Just
                  (Identifier "Int32"),
                cTypeSpecInstances =
                Map.fromList
                  [
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]}}},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "some_fun",
          commentLocation = Just
            "distilled_lib_1.h:72:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["distilled_lib_1.h"],
              headerInclude =
              "distilled_lib_1.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_distilled_lib_1_969c7d0305e0614c",
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
                    "A_type_t")))
              (HsFun
                (HsExtBinding
                  ExtRef {
                    extRefModule = ModuleName
                      "HsBindgen.Runtime.Prelude",
                    extRefIdentifier = Identifier
                      "Word32"}
                  CTypeSpec {
                    cTypeSpecModule = Just
                      (ModuleName
                        "HsBindgen.Runtime.Prelude"),
                    cTypeSpecIdentifier = Just
                      (Identifier "Word32"),
                    cTypeSpecInstances =
                    Map.fromList
                      [
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]})
                (HsFun
                  (HsIncompleteArray
                    (HsExtBinding
                      ExtRef {
                        extRefModule = ModuleName
                          "HsBindgen.Runtime.Prelude",
                        extRefIdentifier = Identifier
                          "Word8"}
                      CTypeSpec {
                        cTypeSpecModule = Just
                          (ModuleName
                            "HsBindgen.Runtime.Prelude"),
                        cTypeSpecIdentifier = Just
                          (Identifier "Word8"),
                        cTypeSpecInstances =
                        Map.fromList
                          [
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}))
                  (HsIO
                    (HsExtBinding
                      ExtRef {
                        extRefModule = ModuleName
                          "HsBindgen.Runtime.Prelude",
                        extRefIdentifier = Identifier
                          "Int32"}
                      CTypeSpec {
                        cTypeSpecModule = Just
                          (ModuleName
                            "HsBindgen.Runtime.Prelude"),
                        cTypeSpecIdentifier = Just
                          (Identifier "Int32"),
                        cTypeSpecInstances =
                        Map.fromList
                          [
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}))))))),
      foreignImportOrigName =
      "hs_bindgen_test_distilled_lib_1_969c7d0305e0614c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_some_fun_ptr */ __attribute__ ((const)) int32_t (*hs_bindgen_test_distilled_lib_1_969c7d0305e0614c (void)) (a_type_t *arg1, uint32_t arg2, uint8_t arg3[]) { return &some_fun; } ",
          capiWrapperImport =
          "distilled_lib_1.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "a_type_t",
                    nameHsIdent = Identifier
                      "A_type_t"}
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))),
            TypeExtBinding
              ResolvedExtBinding {
                extCName = QualName {
                  qualNameName = Name "uint32_t",
                  qualNameKind =
                  NameKindOrdinary},
                extHsRef = ExtRef {
                  extRefModule = ModuleName
                    "HsBindgen.Runtime.Prelude",
                  extRefIdentifier = Identifier
                    "Word32"},
                extHsSpec = CTypeSpec {
                  cTypeSpecModule = Just
                    (ModuleName
                      "HsBindgen.Runtime.Prelude"),
                  cTypeSpecIdentifier = Just
                    (Identifier "Word32"),
                  cTypeSpecInstances =
                  Map.fromList
                    [
                      _×_
                        Bits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bounded
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Enum
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Eq
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        FiniteBits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Integral
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ix
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Num
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ord
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Read
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        ReadRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Real
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        StaticSize
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]}},
            TypeIncompleteArray
              (TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "uint8_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtRef {
                    extRefModule = ModuleName
                      "HsBindgen.Runtime.Prelude",
                    extRefIdentifier = Identifier
                      "Word8"},
                  extHsSpec = CTypeSpec {
                    cTypeSpecModule = Just
                      (ModuleName
                        "HsBindgen.Runtime.Prelude"),
                    cTypeSpecIdentifier = Just
                      (Identifier "Word8"),
                    cTypeSpecInstances =
                    Map.fromList
                      [
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}})]
          (TypeExtBinding
            ResolvedExtBinding {
              extCName = QualName {
                qualNameName = Name "int32_t",
                qualNameKind =
                NameKindOrdinary},
              extHsRef = ExtRef {
                extRefModule = ModuleName
                  "HsBindgen.Runtime.Prelude",
                extRefIdentifier = Identifier
                  "Int32"},
              extHsSpec = CTypeSpec {
                cTypeSpecModule = Just
                  (ModuleName
                    "HsBindgen.Runtime.Prelude"),
                cTypeSpecIdentifier = Just
                  (Identifier "Int32"),
                cTypeSpecInstances =
                Map.fromList
                  [
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]}})),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_distilled_lib_1_b9e65c51f976c6f6",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Var_t")))),
      foreignImportOrigName =
      "hs_bindgen_test_distilled_lib_1_b9e65c51f976c6f6",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_v_ptr */ __attribute__ ((const)) var_t *hs_bindgen_test_distilled_lib_1_b9e65c51f976c6f6 (void) { return &v; } ",
          capiWrapperImport =
          "distilled_lib_1.h"},
      foreignImportOrigin = Global
        (TypeTypedef
          (TypedefRegular
            NamePair {
              nameC = Name "var_t",
              nameHsIdent = Identifier
                "Var_t"}
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
