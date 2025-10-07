[
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Fun_ptr_Deref",
      newtypeConstr = Name
        "@NsConstr"
        "Fun_ptr_Deref",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Fun_ptr_Deref",
        fieldType = HsFun
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Forward_declaration")))
          (HsIO (HsPrimType HsPrimUnit)),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "circular_dependency_fun.h:3:16",
          declId = NamePair {
            nameC = Name "fun_ptr_Deref",
            nameHsIdent = Identifier
              "Fun_ptr_Deref"},
          declOrigin = NameOriginGenerated
            (AnonId
              "circular_dependency_fun.h:3:16"),
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["circular_dependency_fun.h"],
              headerInclude =
              "circular_dependency_fun.h"},
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
                          nameC = Name "fun_ptr",
                          nameHsIdent = Identifier
                            "Fun_ptr"})]])},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Fun_ptr_Deref",
              newtypeField = Name
                "@NsVar"
                "un_Fun_ptr_Deref"},
            typedefType = TypeFun
              [
                TypePointer
                  (TypeStruct
                    NamePair {
                      nameC = Name
                        "forward_declaration",
                      nameHsIdent = Identifier
                        "Forward_declaration"}
                    NameOriginInSource)]
              TypeVoid},
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
              Identifier "Fun_ptr"],
          commentOrigin = Nothing,
          commentLocation = Just
            "circular_dependency_fun.h:3:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["circular_dependency_fun.h"],
              headerInclude =
              "circular_dependency_fun.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "toFun_ptr_Deref",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Fun_ptr_Deref"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Fun_ptr_Deref")))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = ToFunPtr
        (TypeFun
          [
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name
                    "forward_declaration",
                  nameHsIdent = Identifier
                    "Forward_declaration"}
                NameOriginInSource)]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fromFun_ptr_Deref",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Fun_ptr_Deref")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsTypRef
          (Name
            "@NsTypeConstr"
            "Fun_ptr_Deref")),
      foreignImportOrigName =
      "dynamic",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = FromFunPtr
        (TypeFun
          [
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name
                    "forward_declaration",
                  nameHsIdent = Identifier
                    "Forward_declaration"}
                NameOriginInSource)]
          TypeVoid),
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
              "Fun_ptr_Deref"),
          toFunPtrInstanceBody = Name
            "@NsVar"
            "toFun_ptr_Deref"},
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
              "Fun_ptr_Deref"),
          fromFunPtrInstanceBody = Name
            "@NsVar"
            "fromFun_ptr_Deref"},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Fun_ptr",
      newtypeConstr = Name
        "@NsConstr"
        "Fun_ptr",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Fun_ptr",
        fieldType = HsFunPtr
          (HsTypRef
            (Name
              "@NsTypeConstr"
              "Fun_ptr_Deref")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "circular_dependency_fun.h:3:16",
          declId = NamePair {
            nameC = Name "fun_ptr",
            nameHsIdent = Identifier
              "Fun_ptr"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["circular_dependency_fun.h"],
              headerInclude =
              "circular_dependency_fun.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Fun_ptr",
              newtypeField = Name
                "@NsVar"
                "un_Fun_ptr"},
            typedefType = TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "fun_ptr_Deref",
                    nameHsIdent = Identifier
                      "Fun_ptr_Deref"}))},
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
          commentOrigin = Just "fun_ptr",
          commentLocation = Just
            "circular_dependency_fun.h:3:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["circular_dependency_fun.h"],
              headerInclude =
              "circular_dependency_fun.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Fun_ptr",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Fun_ptr",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Fun_ptr",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Fun_ptr",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Forward_declaration",
      structConstr = Name
        "@NsConstr"
        "Forward_declaration",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "forward_declaration_f",
          fieldType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Fun_ptr"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "circular_dependency_fun.h:6:11",
                fieldName = NamePair {
                  nameC = Name "f",
                  nameHsIdent = Identifier
                    "forward_declaration_f"},
                fieldComment = Nothing},
              structFieldType = TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "fun_ptr",
                    nameHsIdent = Identifier
                      "Fun_ptr"}),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "f",
              commentLocation = Just
                "circular_dependency_fun.h:6:11",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["circular_dependency_fun.h"],
                  headerInclude =
                  "circular_dependency_fun.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "circular_dependency_fun.h:5:8",
            declId = NamePair {
              nameC = Name
                "forward_declaration",
              nameHsIdent = Identifier
                "Forward_declaration"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["circular_dependency_fun.h"],
                headerInclude =
                "circular_dependency_fun.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name
                  "@NsConstr"
                  "Forward_declaration"),
              structSizeof = 8,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "circular_dependency_fun.h:6:11",
                    fieldName = NamePair {
                      nameC = Name "f",
                      nameHsIdent = Identifier
                        "forward_declaration_f"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "fun_ptr",
                        nameHsIdent = Identifier
                          "Fun_ptr"}),
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
          commentOrigin = Just
            "forward_declaration",
          commentLocation = Just
            "circular_dependency_fun.h:5:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["circular_dependency_fun.h"],
              headerInclude =
              "circular_dependency_fun.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Forward_declaration",
          structConstr = Name
            "@NsConstr"
            "Forward_declaration",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "forward_declaration_f",
              fieldType = HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Fun_ptr"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "circular_dependency_fun.h:6:11",
                    fieldName = NamePair {
                      nameC = Name "f",
                      nameHsIdent = Identifier
                        "forward_declaration_f"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "fun_ptr",
                        nameHsIdent = Identifier
                          "Fun_ptr"}),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "f",
                  commentLocation = Just
                    "circular_dependency_fun.h:6:11",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["circular_dependency_fun.h"],
                      headerInclude =
                      "circular_dependency_fun.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "circular_dependency_fun.h:5:8",
                declId = NamePair {
                  nameC = Name
                    "forward_declaration",
                  nameHsIdent = Identifier
                    "Forward_declaration"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["circular_dependency_fun.h"],
                    headerInclude =
                    "circular_dependency_fun.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name
                      "@NsConstr"
                      "Forward_declaration"),
                  structSizeof = 8,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "circular_dependency_fun.h:6:11",
                        fieldName = NamePair {
                          nameC = Name "f",
                          nameHsIdent = Identifier
                            "forward_declaration_f"},
                        fieldComment = Nothing},
                      structFieldType = TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "fun_ptr",
                            nameHsIdent = Identifier
                              "Fun_ptr"}),
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
              commentOrigin = Just
                "forward_declaration",
              commentLocation = Just
                "circular_dependency_fun.h:5:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["circular_dependency_fun.h"],
                  headerInclude =
                  "circular_dependency_fun.h"},
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
                    "Forward_declaration",
                  structConstr = Name
                    "@NsConstr"
                    "Forward_declaration",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "forward_declaration_f",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Fun_ptr"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "circular_dependency_fun.h:6:11",
                            fieldName = NamePair {
                              nameC = Name "f",
                              nameHsIdent = Identifier
                                "forward_declaration_f"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefRegular
                              NamePair {
                                nameC = Name "fun_ptr",
                                nameHsIdent = Identifier
                                  "Fun_ptr"}),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "f",
                          commentLocation = Just
                            "circular_dependency_fun.h:6:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["circular_dependency_fun.h"],
                              headerInclude =
                              "circular_dependency_fun.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "circular_dependency_fun.h:5:8",
                        declId = NamePair {
                          nameC = Name
                            "forward_declaration",
                          nameHsIdent = Identifier
                            "Forward_declaration"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["circular_dependency_fun.h"],
                            headerInclude =
                            "circular_dependency_fun.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Forward_declaration"),
                          structSizeof = 8,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "circular_dependency_fun.h:6:11",
                                fieldName = NamePair {
                                  nameC = Name "f",
                                  nameHsIdent = Identifier
                                    "forward_declaration_f"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefRegular
                                  NamePair {
                                    nameC = Name "fun_ptr",
                                    nameHsIdent = Identifier
                                      "Fun_ptr"}),
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
                      commentOrigin = Just
                        "forward_declaration",
                      commentLocation = Just
                        "circular_dependency_fun.h:5:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["circular_dependency_fun.h"],
                          headerInclude =
                          "circular_dependency_fun.h"},
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
                    "Forward_declaration",
                  structConstr = Name
                    "@NsConstr"
                    "Forward_declaration",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "forward_declaration_f",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Fun_ptr"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "circular_dependency_fun.h:6:11",
                            fieldName = NamePair {
                              nameC = Name "f",
                              nameHsIdent = Identifier
                                "forward_declaration_f"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefRegular
                              NamePair {
                                nameC = Name "fun_ptr",
                                nameHsIdent = Identifier
                                  "Fun_ptr"}),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "f",
                          commentLocation = Just
                            "circular_dependency_fun.h:6:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["circular_dependency_fun.h"],
                              headerInclude =
                              "circular_dependency_fun.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "circular_dependency_fun.h:5:8",
                        declId = NamePair {
                          nameC = Name
                            "forward_declaration",
                          nameHsIdent = Identifier
                            "Forward_declaration"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["circular_dependency_fun.h"],
                            headerInclude =
                            "circular_dependency_fun.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Forward_declaration"),
                          structSizeof = 8,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "circular_dependency_fun.h:6:11",
                                fieldName = NamePair {
                                  nameC = Name "f",
                                  nameHsIdent = Identifier
                                    "forward_declaration_f"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefRegular
                                  NamePair {
                                    nameC = Name "fun_ptr",
                                    nameHsIdent = Identifier
                                      "Fun_ptr"}),
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
                      commentOrigin = Just
                        "forward_declaration",
                      commentLocation = Just
                        "circular_dependency_fun.h:5:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["circular_dependency_fun.h"],
                          headerInclude =
                          "circular_dependency_fun.h"},
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
        "Forward_declaration",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Forward_declaration",
      deriveInstanceComment =
      Nothing}]
