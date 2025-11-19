[
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Int2int",
      newtypeConstr = Name
        "@NsConstr"
        "Int2int",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Int2int",
        fieldType = HsFun
          (HsPrimType HsPrimCInt)
          (HsIO (HsPrimType HsPrimCInt)),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "function_pointers.h:19:13",
          declId = NamePair {
            nameC = Name "int2int",
            nameHsIdent = Identifier
              "Int2int"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/function_pointers.h"],
              headerInclude =
              "manual/function_pointers.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Int2int",
              newtypeField = Name
                "@NsVar"
                "un_Int2int"},
            typedefType = TypeFun
              [
                TypePrim
                  (PrimIntegral PrimInt Signed)]
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "int2int",
          commentLocation = Just
            "function_pointers.h:19:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/function_pointers.h"],
              headerInclude =
              "manual/function_pointers.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "toInt2int",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Int2int"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Int2int")))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = ToFunPtr
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed)]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fromInt2int",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Int2int")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsTypRef
          (Name
            "@NsTypeConstr"
            "Int2int")),
      foreignImportOrigName =
      "dynamic",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = FromFunPtr
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed)]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
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
              "Int2int"),
          toFunPtrInstanceBody = Name
            "@NsVar"
            "toInt2int"},
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
              "Int2int"),
          fromFunPtrInstanceBody = Name
            "@NsVar"
            "fromInt2int"},
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
              "Int2int"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_Int2int",
          hasFieldInstanceFieldType =
          HsFun
            (HsPrimType HsPrimCInt)
            (HsIO (HsPrimType HsPrimCInt)),
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
              "Int2int"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_Int2int",
          hasCFieldInstanceCFieldType =
          HsFun
            (HsPrimType HsPrimCInt)
            (HsIO (HsPrimType HsPrimCInt)),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Apply1Struct",
      structConstr = Name
        "@NsConstr"
        "Apply1Struct",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "apply1Struct_apply1_nopointer_struct_field",
          fieldType = HsFunPtr
            (HsFun
              (HsFunPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Int2int")))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimCInt)))),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "function_pointers.h:38:16",
                fieldName = NamePair {
                  nameC = Name
                    "apply1_nopointer_struct_field",
                  nameHsIdent = Identifier
                    "apply1Struct_apply1_nopointer_struct_field"},
                fieldComment = Nothing},
              structFieldType = TypeQualified
                TypeQualifierConst
                (TypePointer
                  (TypeFun
                    [
                      TypePointer
                        (TypeTypedef
                          (TypedefRegular
                            NamePair {
                              nameC = Name "int2int",
                              nameHsIdent = Identifier
                                "Int2int"}
                            (TypeFun
                              [
                                TypePrim
                                  (PrimIntegral PrimInt Signed)]
                              (TypePrim
                                (PrimIntegral
                                  PrimInt
                                  Signed))))),
                      TypePrim
                        (PrimIntegral PrimInt Signed)]
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "apply1_nopointer_struct_field",
              commentLocation = Just
                "function_pointers.h:38:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/function_pointers.h"],
                  headerInclude =
                  "manual/function_pointers.h"},
              commentChildren = []}}],
      structOrigin =
      Just
        Decl {
          declInfo =
          DeclInfo {
            declLoc =
            "function_pointers.h:37:8",
            declId = NamePair {
              nameC = Name "Apply1Struct",
              nameHsIdent = Identifier
                "Apply1Struct"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["manual/function_pointers.h"],
                headerInclude =
                "manual/function_pointers.h"},
            declComment =
            Just
              (Comment
                [
                  Paragraph
                    [
                      TextContent
                        "A struct field pointing to a function like apply1_nopointer()."]])},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name
                  "@NsConstr"
                  "Apply1Struct"),
              structSizeof = 8,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "function_pointers.h:38:16",
                    fieldName = NamePair {
                      nameC = Name
                        "apply1_nopointer_struct_field",
                      nameHsIdent = Identifier
                        "apply1Struct_apply1_nopointer_struct_field"},
                    fieldComment = Nothing},
                  structFieldType = TypeQualified
                    TypeQualifierConst
                    (TypePointer
                      (TypeFun
                        [
                          TypePointer
                            (TypeTypedef
                              (TypedefRegular
                                NamePair {
                                  nameC = Name "int2int",
                                  nameHsIdent = Identifier
                                    "Int2int"}
                                (TypeFun
                                  [
                                    TypePrim
                                      (PrimIntegral PrimInt Signed)]
                                  (TypePrim
                                    (PrimIntegral
                                      PrimInt
                                      Signed))))),
                          TypePrim
                            (PrimIntegral PrimInt Signed)]
                        (TypePrim
                          (PrimIntegral
                            PrimInt
                            Signed)))),
                  structFieldOffset = 0,
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
          commentTitle =
          Just
            [
              TextContent
                "A struct field pointing to a function like apply1_nopointer()."],
          commentOrigin = Just
            "Apply1Struct",
          commentLocation = Just
            "function_pointers.h:37:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/function_pointers.h"],
              headerInclude =
              "manual/function_pointers.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Apply1Struct",
          structConstr = Name
            "@NsConstr"
            "Apply1Struct",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "apply1Struct_apply1_nopointer_struct_field",
              fieldType = HsFunPtr
                (HsFun
                  (HsFunPtr
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "Int2int")))
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsIO
                      (HsPrimType HsPrimCInt)))),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "function_pointers.h:38:16",
                    fieldName = NamePair {
                      nameC = Name
                        "apply1_nopointer_struct_field",
                      nameHsIdent = Identifier
                        "apply1Struct_apply1_nopointer_struct_field"},
                    fieldComment = Nothing},
                  structFieldType = TypeQualified
                    TypeQualifierConst
                    (TypePointer
                      (TypeFun
                        [
                          TypePointer
                            (TypeTypedef
                              (TypedefRegular
                                NamePair {
                                  nameC = Name "int2int",
                                  nameHsIdent = Identifier
                                    "Int2int"}
                                (TypeFun
                                  [
                                    TypePrim
                                      (PrimIntegral PrimInt Signed)]
                                  (TypePrim
                                    (PrimIntegral
                                      PrimInt
                                      Signed))))),
                          TypePrim
                            (PrimIntegral PrimInt Signed)]
                        (TypePrim
                          (PrimIntegral
                            PrimInt
                            Signed)))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just
                    "apply1_nopointer_struct_field",
                  commentLocation = Just
                    "function_pointers.h:38:16",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["manual/function_pointers.h"],
                      headerInclude =
                      "manual/function_pointers.h"},
                  commentChildren = []}}],
          structOrigin =
          Just
            Decl {
              declInfo =
              DeclInfo {
                declLoc =
                "function_pointers.h:37:8",
                declId = NamePair {
                  nameC = Name "Apply1Struct",
                  nameHsIdent = Identifier
                    "Apply1Struct"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["manual/function_pointers.h"],
                    headerInclude =
                    "manual/function_pointers.h"},
                declComment =
                Just
                  (Comment
                    [
                      Paragraph
                        [
                          TextContent
                            "A struct field pointing to a function like apply1_nopointer()."]])},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name
                      "@NsConstr"
                      "Apply1Struct"),
                  structSizeof = 8,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "function_pointers.h:38:16",
                        fieldName = NamePair {
                          nameC = Name
                            "apply1_nopointer_struct_field",
                          nameHsIdent = Identifier
                            "apply1Struct_apply1_nopointer_struct_field"},
                        fieldComment = Nothing},
                      structFieldType = TypeQualified
                        TypeQualifierConst
                        (TypePointer
                          (TypeFun
                            [
                              TypePointer
                                (TypeTypedef
                                  (TypedefRegular
                                    NamePair {
                                      nameC = Name "int2int",
                                      nameHsIdent = Identifier
                                        "Int2int"}
                                    (TypeFun
                                      [
                                        TypePrim
                                          (PrimIntegral PrimInt Signed)]
                                      (TypePrim
                                        (PrimIntegral
                                          PrimInt
                                          Signed))))),
                              TypePrim
                                (PrimIntegral PrimInt Signed)]
                            (TypePrim
                              (PrimIntegral
                                PrimInt
                                Signed)))),
                      structFieldOffset = 0,
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
              commentTitle =
              Just
                [
                  TextContent
                    "A struct field pointing to a function like apply1_nopointer()."],
              commentOrigin = Just
                "Apply1Struct",
              commentLocation = Just
                "function_pointers.h:37:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["manual/function_pointers.h"],
                  headerInclude =
                  "manual/function_pointers.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 8,
          storableAlignment = 8,
          storablePeek =
          Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Apply1Struct",
                  structConstr = Name
                    "@NsConstr"
                    "Apply1Struct",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "apply1Struct_apply1_nopointer_struct_field",
                      fieldType = HsFunPtr
                        (HsFun
                          (HsFunPtr
                            (HsTypRef
                              (Name
                                "@NsTypeConstr"
                                "Int2int")))
                          (HsFun
                            (HsPrimType HsPrimCInt)
                            (HsIO
                              (HsPrimType HsPrimCInt)))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "function_pointers.h:38:16",
                            fieldName = NamePair {
                              nameC = Name
                                "apply1_nopointer_struct_field",
                              nameHsIdent = Identifier
                                "apply1Struct_apply1_nopointer_struct_field"},
                            fieldComment = Nothing},
                          structFieldType = TypeQualified
                            TypeQualifierConst
                            (TypePointer
                              (TypeFun
                                [
                                  TypePointer
                                    (TypeTypedef
                                      (TypedefRegular
                                        NamePair {
                                          nameC = Name "int2int",
                                          nameHsIdent = Identifier
                                            "Int2int"}
                                        (TypeFun
                                          [
                                            TypePrim
                                              (PrimIntegral PrimInt Signed)]
                                          (TypePrim
                                            (PrimIntegral
                                              PrimInt
                                              Signed))))),
                                  TypePrim
                                    (PrimIntegral PrimInt Signed)]
                                (TypePrim
                                  (PrimIntegral
                                    PrimInt
                                    Signed)))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "apply1_nopointer_struct_field",
                          commentLocation = Just
                            "function_pointers.h:38:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/function_pointers.h"],
                              headerInclude =
                              "manual/function_pointers.h"},
                          commentChildren = []}}],
                  structOrigin =
                  Just
                    Decl {
                      declInfo =
                      DeclInfo {
                        declLoc =
                        "function_pointers.h:37:8",
                        declId = NamePair {
                          nameC = Name "Apply1Struct",
                          nameHsIdent = Identifier
                            "Apply1Struct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["manual/function_pointers.h"],
                            headerInclude =
                            "manual/function_pointers.h"},
                        declComment =
                        Just
                          (Comment
                            [
                              Paragraph
                                [
                                  TextContent
                                    "A struct field pointing to a function like apply1_nopointer()."]])},
                      declKind =
                      Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Apply1Struct"),
                          structSizeof = 8,
                          structAlignment = 8,
                          structFields =
                          [
                            StructField {
                              structFieldInfo =
                              FieldInfo {
                                fieldLoc =
                                "function_pointers.h:38:16",
                                fieldName =
                                NamePair {
                                  nameC = Name
                                    "apply1_nopointer_struct_field",
                                  nameHsIdent =
                                  Identifier
                                    "apply1Struct_apply1_nopointer_struct_field"},
                                fieldComment = Nothing},
                              structFieldType = TypeQualified
                                TypeQualifierConst
                                (TypePointer
                                  (TypeFun
                                    [
                                      TypePointer
                                        (TypeTypedef
                                          (TypedefRegular
                                            NamePair {
                                              nameC = Name "int2int",
                                              nameHsIdent = Identifier
                                                "Int2int"}
                                            (TypeFun
                                              [
                                                TypePrim
                                                  (PrimIntegral PrimInt Signed)]
                                              (TypePrim
                                                (PrimIntegral
                                                  PrimInt
                                                  Signed))))),
                                      TypePrim
                                        (PrimIntegral PrimInt Signed)]
                                    (TypePrim
                                      (PrimIntegral
                                        PrimInt
                                        Signed)))),
                              structFieldOffset = 0,
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
                      commentTitle =
                      Just
                        [
                          TextContent
                            "A struct field pointing to a function like apply1_nopointer()."],
                      commentOrigin = Just
                        "Apply1Struct",
                      commentLocation = Just
                        "function_pointers.h:37:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["manual/function_pointers.h"],
                          headerInclude =
                          "manual/function_pointers.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit
                    "apply1Struct_apply1_nopointer_struct_field")
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
                    "Apply1Struct",
                  structConstr = Name
                    "@NsConstr"
                    "Apply1Struct",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "apply1Struct_apply1_nopointer_struct_field",
                      fieldType = HsFunPtr
                        (HsFun
                          (HsFunPtr
                            (HsTypRef
                              (Name
                                "@NsTypeConstr"
                                "Int2int")))
                          (HsFun
                            (HsPrimType HsPrimCInt)
                            (HsIO
                              (HsPrimType HsPrimCInt)))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "function_pointers.h:38:16",
                            fieldName = NamePair {
                              nameC = Name
                                "apply1_nopointer_struct_field",
                              nameHsIdent = Identifier
                                "apply1Struct_apply1_nopointer_struct_field"},
                            fieldComment = Nothing},
                          structFieldType = TypeQualified
                            TypeQualifierConst
                            (TypePointer
                              (TypeFun
                                [
                                  TypePointer
                                    (TypeTypedef
                                      (TypedefRegular
                                        NamePair {
                                          nameC = Name "int2int",
                                          nameHsIdent = Identifier
                                            "Int2int"}
                                        (TypeFun
                                          [
                                            TypePrim
                                              (PrimIntegral PrimInt Signed)]
                                          (TypePrim
                                            (PrimIntegral
                                              PrimInt
                                              Signed))))),
                                  TypePrim
                                    (PrimIntegral PrimInt Signed)]
                                (TypePrim
                                  (PrimIntegral
                                    PrimInt
                                    Signed)))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "apply1_nopointer_struct_field",
                          commentLocation = Just
                            "function_pointers.h:38:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["manual/function_pointers.h"],
                              headerInclude =
                              "manual/function_pointers.h"},
                          commentChildren = []}}],
                  structOrigin =
                  Just
                    Decl {
                      declInfo =
                      DeclInfo {
                        declLoc =
                        "function_pointers.h:37:8",
                        declId = NamePair {
                          nameC = Name "Apply1Struct",
                          nameHsIdent = Identifier
                            "Apply1Struct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["manual/function_pointers.h"],
                            headerInclude =
                            "manual/function_pointers.h"},
                        declComment =
                        Just
                          (Comment
                            [
                              Paragraph
                                [
                                  TextContent
                                    "A struct field pointing to a function like apply1_nopointer()."]])},
                      declKind =
                      Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Apply1Struct"),
                          structSizeof = 8,
                          structAlignment = 8,
                          structFields =
                          [
                            StructField {
                              structFieldInfo =
                              FieldInfo {
                                fieldLoc =
                                "function_pointers.h:38:16",
                                fieldName =
                                NamePair {
                                  nameC = Name
                                    "apply1_nopointer_struct_field",
                                  nameHsIdent =
                                  Identifier
                                    "apply1Struct_apply1_nopointer_struct_field"},
                                fieldComment = Nothing},
                              structFieldType = TypeQualified
                                TypeQualifierConst
                                (TypePointer
                                  (TypeFun
                                    [
                                      TypePointer
                                        (TypeTypedef
                                          (TypedefRegular
                                            NamePair {
                                              nameC = Name "int2int",
                                              nameHsIdent = Identifier
                                                "Int2int"}
                                            (TypeFun
                                              [
                                                TypePrim
                                                  (PrimIntegral PrimInt Signed)]
                                              (TypePrim
                                                (PrimIntegral
                                                  PrimInt
                                                  Signed))))),
                                      TypePrim
                                        (PrimIntegral PrimInt Signed)]
                                    (TypePrim
                                      (PrimIntegral
                                        PrimInt
                                        Signed)))),
                              structFieldOffset = 0,
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
                      commentTitle =
                      Just
                        [
                          TextContent
                            "A struct field pointing to a function like apply1_nopointer()."],
                      commentOrigin = Just
                        "Apply1Struct",
                      commentLocation = Just
                        "function_pointers.h:37:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["manual/function_pointers.h"],
                          headerInclude =
                          "manual/function_pointers.h"},
                      commentChildren = []}}
                (Add 1)
                (Seq
                  [
                    PokeCField
                      (HsStrLit
                        "apply1Struct_apply1_nopointer_struct_field")
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
        "Apply1Struct",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Apply1Struct",
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
              "Apply1Struct"),
          hasCFieldInstanceFieldName =
          Name
            "@NsVar"
            "apply1Struct_apply1_nopointer_struct_field",
          hasCFieldInstanceCFieldType =
          HsFunPtr
            (HsFun
              (HsFunPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Int2int")))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimCInt)))),
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
              "Apply1Struct"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "apply1Struct_apply1_nopointer_struct_field",
          hasFieldInstanceFieldType =
          HsFunPtr
            (HsFun
              (HsFunPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Int2int")))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimCInt)))),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Apply1Union",
      newtypeConstr = Name
        "@NsConstr"
        "Apply1Union",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Apply1Union",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin =
      Decl {
        declInfo =
        DeclInfo {
          declLoc =
          "function_pointers.h:43:7",
          declId = NamePair {
            nameC = Name "Apply1Union",
            nameHsIdent = Identifier
              "Apply1Union"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/function_pointers.h"],
              headerInclude =
              "manual/function_pointers.h"},
          declComment =
          Just
            (Comment
              [
                Paragraph
                  [
                    TextContent
                      "A union field pointing to a function like apply1_nopointer()."]])},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Apply1Union",
              newtypeField = Name
                "@NsVar"
                "un_Apply1Union"},
            unionSizeof = 8,
            unionAlignment = 8,
            unionFields = [
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc =
                  "function_pointers.h:44:16",
                  fieldName = NamePair {
                    nameC = Name
                      "apply1_nopointer_union_field",
                    nameHsIdent = Identifier
                      "apply1Union_apply1_nopointer_union_field"},
                  fieldComment = Nothing},
                unionFieldType = TypeQualified
                  TypeQualifierConst
                  (TypePointer
                    (TypeFun
                      [
                        TypePointer
                          (TypeTypedef
                            (TypedefRegular
                              NamePair {
                                nameC = Name "int2int",
                                nameHsIdent = Identifier
                                  "Int2int"}
                              (TypeFun
                                [
                                  TypePrim
                                    (PrimIntegral PrimInt Signed)]
                                (TypePrim
                                  (PrimIntegral
                                    PrimInt
                                    Signed))))),
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed))))}]},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [Storable],
      newtypeComment =
      Just
        Comment {
          commentTitle =
          Just
            [
              TextContent
                "A union field pointing to a function like apply1_nopointer()."],
          commentOrigin = Just
            "Apply1Union",
          commentLocation = Just
            "function_pointers.h:43:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/function_pointers.h"],
              headerInclude =
              "manual/function_pointers.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveVia
        (HsSizedByteArray 8 8),
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Apply1Union",
      deriveInstanceComment =
      Nothing},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = Name
        "@NsVar"
        "get_apply1Union_apply1_nopointer_union_field",
      unionGetterType = HsFunPtr
        (HsFun
          (HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Int2int")))
          (HsFun
            (HsPrimType HsPrimCInt)
            (HsIO
              (HsPrimType HsPrimCInt)))),
      unionGetterConstr = Name
        "@NsTypeConstr"
        "Apply1Union",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "apply1_nopointer_union_field",
          commentLocation = Just
            "function_pointers.h:44:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/function_pointers.h"],
              headerInclude =
              "manual/function_pointers.h"},
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_apply1Union_apply1_nopointer_union_field"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = Name
        "@NsVar"
        "set_apply1Union_apply1_nopointer_union_field",
      unionSetterType = HsFunPtr
        (HsFun
          (HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Int2int")))
          (HsFun
            (HsPrimType HsPrimCInt)
            (HsIO
              (HsPrimType HsPrimCInt)))),
      unionSetterConstr = Name
        "@NsTypeConstr"
        "Apply1Union",
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
                  "get_apply1Union_apply1_nopointer_union_field"]]}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Apply1Union"),
          hasCFieldInstanceFieldName =
          Name
            "@NsVar"
            "apply1Union_apply1_nopointer_union_field",
          hasCFieldInstanceCFieldType =
          HsFunPtr
            (HsFun
              (HsFunPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Int2int")))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimCInt)))),
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
              "Apply1Union"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "apply1Union_apply1_nopointer_union_field",
          hasFieldInstanceFieldType =
          HsFunPtr
            (HsFun
              (HsFunPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Int2int")))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimCInt)))),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "square",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_55e5eb89e54abf83",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_manualfunction_pointers_55e5eb89e54abf83 (\n",
              "  signed int arg1\n",
              ")\n",
              "{\n",
              "  return square(arg1);\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "square",
          commentLocation = Just
            "function_pointers.h:5:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/function_pointers.h"],
              headerInclude =
              "manual/function_pointers.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "plus",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_680daf766a044980",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_manualfunction_pointers_680daf766a044980 (\n",
              "  signed int arg1,\n",
              "  signed int arg2\n",
              ")\n",
              "{\n",
              "  return plus(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              Nothing
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "plus",
          commentLocation = Just
            "function_pointers.h:7:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/function_pointers.h"],
              headerInclude =
              "manual/function_pointers.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "apply1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "f"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimCInt))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "f",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_abcb860034253564",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_manualfunction_pointers_abcb860034253564 (\n",
              "  signed int (*arg1) (\n",
              "  signed int arg1\n",
              "),\n",
              "  signed int arg2\n",
              ")\n",
              "{\n",
              "  return apply1(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "f",
                  nameHsIdent = Identifier "f"})
              (TypePointer
                (TypeFun
                  [
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "apply1",
          commentLocation = Just
            "function_pointers.h:9:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/function_pointers.h"],
              headerInclude =
              "manual/function_pointers.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "apply2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "f"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimCInt)))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "f",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "y"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_1ad13c166a710f40",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_manualfunction_pointers_1ad13c166a710f40 (\n",
              "  signed int (*arg1) (\n",
              "  signed int arg1,\n",
              "  signed int arg2\n",
              "),\n",
              "  signed int arg2,\n",
              "  signed int arg3\n",
              ")\n",
              "{\n",
              "  return apply2(arg1, arg2, arg3);\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "f",
                  nameHsIdent = Identifier "f"})
              (TypePointer
                (TypeFun
                  [
                    TypePrim
                      (PrimIntegral PrimInt Signed),
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier "y"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "apply2",
          commentLocation = Just
            "function_pointers.h:11:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/function_pointers.h"],
              headerInclude =
              "manual/function_pointers.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "apply1_pointer_arg",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Int2int")),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_a8ef4d9e6ce68f54",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_manualfunction_pointers_a8ef4d9e6ce68f54 (\n",
              "  int2int *arg1,\n",
              "  signed int arg2\n",
              ")\n",
              "{\n",
              "  return apply1_pointer_arg(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypePointer
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "int2int",
                      nameHsIdent = Identifier
                        "Int2int"}
                    (TypeFun
                      [
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed)))))),
            _×_
              Nothing
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment =
      Just
        Comment {
          commentTitle =
          Just
            [
              TextContent
                "Basically the same as apply1(), but here for illustratory purposes."],
          commentOrigin = Just
            "apply1_pointer_arg",
          commentLocation = Just
            "function_pointers.h:22:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/function_pointers.h"],
              headerInclude =
              "manual/function_pointers.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "apply1_nopointer_arg",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Int2int")),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_7dc4caa1f7f0caf0",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_manualfunction_pointers_7dc4caa1f7f0caf0 (\n",
              "  int2int *arg1,\n",
              "  signed int arg2\n",
              ")\n",
              "{\n",
              "  return apply1_nopointer_arg(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypePointer
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "int2int",
                      nameHsIdent = Identifier
                        "Int2int"}
                    (TypeFun
                      [
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed)))))),
            _×_
              Nothing
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment =
      Just
        Comment {
          commentTitle =
          Just
            [
              TextContent
                "A version of apply1_pointer_arg() that declares to take a argument of",
              TextContent
                "function type, rather than a pointer-to-function type."],
          commentOrigin = Just
            "apply1_nopointer_arg",
          commentLocation = Just
            "function_pointers.h:26:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/function_pointers.h"],
              headerInclude =
              "manual/function_pointers.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "apply1_nopointer_res",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsFunPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Int2int")))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_3612aa0d10e36d5b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int (*const hs_bindgen_test_manualfunction_pointers_3612aa0d10e36d5b (void)) (\n",
              "  int2int *arg1,\n",
              "  signed int arg2\n",
              ")\n",
              "{\n",
              "  return apply1_nopointer_res();\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeQualified
            TypeQualifierConst
            (TypePointer
              (TypeFun
                [
                  TypePointer
                    (TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "int2int",
                          nameHsIdent = Identifier
                            "Int2int"}
                        (TypeFun
                          [
                            TypePrim
                              (PrimIntegral PrimInt Signed)]
                          (TypePrim
                            (PrimIntegral
                              PrimInt
                              Signed))))),
                  TypePrim
                    (PrimIntegral PrimInt Signed)]
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed))))},
      foreignImportComment =
      Just
        Comment {
          commentTitle =
          Just
            [
              TextContent
                "A function returning a pointer to a function like apply1_nopointer()."],
          commentOrigin = Just
            "apply1_nopointer_res",
          commentLocation = Just
            "function_pointers.h:31:21",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/function_pointers.h"],
              headerInclude =
              "manual/function_pointers.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "square",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_cb3c687f16289bb3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_manualfunction_pointers_cb3c687f16289bb3 (\n",
              "  signed int arg1\n",
              ")\n",
              "{\n",
              "  return square(arg1);\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "square",
          commentLocation = Just
            "function_pointers.h:5:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/function_pointers.h"],
              headerInclude =
              "manual/function_pointers.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "plus",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_a9730564387164c0",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_manualfunction_pointers_a9730564387164c0 (\n",
              "  signed int arg1,\n",
              "  signed int arg2\n",
              ")\n",
              "{\n",
              "  return plus(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              Nothing
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "plus",
          commentLocation = Just
            "function_pointers.h:7:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/function_pointers.h"],
              headerInclude =
              "manual/function_pointers.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "apply1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "f"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimCInt))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "f",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_3fb9c4a14d502477",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_manualfunction_pointers_3fb9c4a14d502477 (\n",
              "  signed int (*arg1) (\n",
              "  signed int arg1\n",
              "),\n",
              "  signed int arg2\n",
              ")\n",
              "{\n",
              "  return apply1(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "f",
                  nameHsIdent = Identifier "f"})
              (TypePointer
                (TypeFun
                  [
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "apply1",
          commentLocation = Just
            "function_pointers.h:9:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/function_pointers.h"],
              headerInclude =
              "manual/function_pointers.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "apply2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "f"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimCInt)))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "f",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "y"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_75b5699fdabb6333",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_manualfunction_pointers_75b5699fdabb6333 (\n",
              "  signed int (*arg1) (\n",
              "  signed int arg1,\n",
              "  signed int arg2\n",
              "),\n",
              "  signed int arg2,\n",
              "  signed int arg3\n",
              ")\n",
              "{\n",
              "  return apply2(arg1, arg2, arg3);\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "f",
                  nameHsIdent = Identifier "f"})
              (TypePointer
                (TypeFun
                  [
                    TypePrim
                      (PrimIntegral PrimInt Signed),
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier "y"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "apply2",
          commentLocation = Just
            "function_pointers.h:11:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/function_pointers.h"],
              headerInclude =
              "manual/function_pointers.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "apply1_pointer_arg",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Int2int")),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_2d5144fc06502862",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_manualfunction_pointers_2d5144fc06502862 (\n",
              "  int2int *arg1,\n",
              "  signed int arg2\n",
              ")\n",
              "{\n",
              "  return apply1_pointer_arg(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypePointer
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "int2int",
                      nameHsIdent = Identifier
                        "Int2int"}
                    (TypeFun
                      [
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed)))))),
            _×_
              Nothing
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment =
      Just
        Comment {
          commentTitle =
          Just
            [
              TextContent
                "Basically the same as apply1(), but here for illustratory purposes."],
          commentOrigin = Just
            "apply1_pointer_arg",
          commentLocation = Just
            "function_pointers.h:22:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/function_pointers.h"],
              headerInclude =
              "manual/function_pointers.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "apply1_nopointer_arg",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Int2int")),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_b7597a0c4856ebb3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int hs_bindgen_test_manualfunction_pointers_b7597a0c4856ebb3 (\n",
              "  int2int *arg1,\n",
              "  signed int arg2\n",
              ")\n",
              "{\n",
              "  return apply1_nopointer_arg(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypePointer
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "int2int",
                      nameHsIdent = Identifier
                        "Int2int"}
                    (TypeFun
                      [
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed)))))),
            _×_
              Nothing
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment =
      Just
        Comment {
          commentTitle =
          Just
            [
              TextContent
                "A version of apply1_pointer_arg() that declares to take a argument of",
              TextContent
                "function type, rather than a pointer-to-function type."],
          commentOrigin = Just
            "apply1_nopointer_arg",
          commentLocation = Just
            "function_pointers.h:26:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/function_pointers.h"],
              headerInclude =
              "manual/function_pointers.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "apply1_nopointer_res",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsFunPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Int2int")))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_be3907895c70597f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int (*const hs_bindgen_test_manualfunction_pointers_be3907895c70597f (void)) (\n",
              "  int2int *arg1,\n",
              "  signed int arg2\n",
              ")\n",
              "{\n",
              "  return apply1_nopointer_res();\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeQualified
            TypeQualifierConst
            (TypePointer
              (TypeFun
                [
                  TypePointer
                    (TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "int2int",
                          nameHsIdent = Identifier
                            "Int2int"}
                        (TypeFun
                          [
                            TypePrim
                              (PrimIntegral PrimInt Signed)]
                          (TypePrim
                            (PrimIntegral
                              PrimInt
                              Signed))))),
                  TypePrim
                    (PrimIntegral PrimInt Signed)]
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed))))},
      foreignImportComment =
      Just
        Comment {
          commentTitle =
          Just
            [
              TextContent
                "A function returning a pointer to a function like apply1_nopointer()."],
          commentOrigin = Just
            "apply1_nopointer_res",
          commentLocation = Just
            "function_pointers.h:31:21",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["manual/function_pointers.h"],
              headerInclude =
              "manual/function_pointers.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_manualfunction_pointers_c41111f40a04cdc9",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_c41111f40a04cdc9",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_square_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*hs_bindgen_test_manualfunction_pointers_c41111f40a04cdc9 (void)) (\n",
              "  signed int arg1\n",
              ")\n",
              "{\n",
              "  return &square;\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed)]
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
        "hs_bindgen_test_manualfunction_pointers_bf838c747898dc42",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_bf838c747898dc42",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_plus_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*hs_bindgen_test_manualfunction_pointers_bf838c747898dc42 (void)) (\n",
              "  signed int arg1,\n",
              "  signed int arg2\n",
              ")\n",
              "{\n",
              "  return &plus;\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypePrim
              (PrimIntegral PrimInt Signed)]
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
        "hs_bindgen_test_manualfunction_pointers_4d1935e01bc37070",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsFunPtr
                (HsFun
                  (HsPrimType HsPrimCInt)
                  (HsIO (HsPrimType HsPrimCInt))))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_4d1935e01bc37070",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_apply1_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*hs_bindgen_test_manualfunction_pointers_4d1935e01bc37070 (void)) (\n",
              "  signed int (*arg1) (\n",
              "  signed int arg1\n",
              "),\n",
              "  signed int arg2\n",
              ")\n",
              "{\n",
              "  return &apply1;\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed)]
                (TypePrim
                  (PrimIntegral PrimInt Signed))),
            TypePrim
              (PrimIntegral PrimInt Signed)]
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
        "hs_bindgen_test_manualfunction_pointers_40cb8952bacd236a",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsFunPtr
                (HsFun
                  (HsPrimType HsPrimCInt)
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsIO
                      (HsPrimType HsPrimCInt)))))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsFun
                  (HsPrimType HsPrimCInt)
                  (HsIO
                    (HsPrimType HsPrimCInt))))))),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_40cb8952bacd236a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_apply2_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*hs_bindgen_test_manualfunction_pointers_40cb8952bacd236a (void)) (\n",
              "  signed int (*arg1) (\n",
              "  signed int arg1,\n",
              "  signed int arg2\n",
              "),\n",
              "  signed int arg2,\n",
              "  signed int arg3\n",
              ")\n",
              "{\n",
              "  return &apply2;\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed),
                  TypePrim
                    (PrimIntegral PrimInt Signed)]
                (TypePrim
                  (PrimIntegral PrimInt Signed))),
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypePrim
              (PrimIntegral PrimInt Signed)]
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
        "hs_bindgen_test_manualfunction_pointers_653c5bde7704c3ca",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsFunPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Int2int")))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_653c5bde7704c3ca",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_apply1_pointer_arg_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*hs_bindgen_test_manualfunction_pointers_653c5bde7704c3ca (void)) (\n",
              "  int2int *arg1,\n",
              "  signed int arg2\n",
              ")\n",
              "{\n",
              "  return &apply1_pointer_arg;\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "int2int",
                    nameHsIdent = Identifier
                      "Int2int"}
                  (TypeFun
                    [
                      TypePrim
                        (PrimIntegral PrimInt Signed)]
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))),
            TypePrim
              (PrimIntegral PrimInt Signed)]
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
        "hs_bindgen_test_manualfunction_pointers_3bb9417cd7afec81",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsFunPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Int2int")))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_3bb9417cd7afec81",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_apply1_nopointer_arg_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*hs_bindgen_test_manualfunction_pointers_3bb9417cd7afec81 (void)) (\n",
              "  int2int *arg1,\n",
              "  signed int arg2\n",
              ")\n",
              "{\n",
              "  return &apply1_nopointer_arg;\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "int2int",
                    nameHsIdent = Identifier
                      "Int2int"}
                  (TypeFun
                    [
                      TypePrim
                        (PrimIntegral PrimInt Signed)]
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))),
            TypePrim
              (PrimIntegral PrimInt Signed)]
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
        "hs_bindgen_test_manualfunction_pointers_f7a08d090f6f7b0f",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsFunPtr
                (HsFun
                  (HsFunPtr
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "Int2int")))
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsIO
                      (HsPrimType HsPrimCInt)))))))),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_f7a08d090f6f7b0f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_apply1_nopointer_res_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*const (*hs_bindgen_test_manualfunction_pointers_f7a08d090f6f7b0f (void)) (void)) (\n",
              "  int2int *arg1,\n",
              "  signed int arg2\n",
              ")\n",
              "{\n",
              "  return &apply1_nopointer_res;\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
          (TypeQualified
            TypeQualifierConst
            (TypePointer
              (TypeFun
                [
                  TypePointer
                    (TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "int2int",
                          nameHsIdent = Identifier
                            "Int2int"}
                        (TypeFun
                          [
                            TypePrim
                              (PrimIntegral PrimInt Signed)]
                          (TypePrim
                            (PrimIntegral
                              PrimInt
                              Signed))))),
                  TypePrim
                    (PrimIntegral PrimInt Signed)]
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_manualfunction_pointers_c4bb317da29227a6",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsFunPtr
              (HsFun
                (HsFunPtr
                  (HsTypRef
                    (Name
                      "@NsTypeConstr"
                      "Int2int")))
                (HsFun
                  (HsPrimType HsPrimCInt)
                  (HsIO
                    (HsPrimType HsPrimCInt))))))),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_c4bb317da29227a6",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_apply1_nopointer_var_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*const *hs_bindgen_test_manualfunction_pointers_c4bb317da29227a6 (void)) (\n",
              "  int2int *arg1,\n",
              "  signed int arg2\n",
              ")\n",
              "{\n",
              "  return &apply1_nopointer_var;\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Global
        (TypeQualified
          TypeQualifierConst
          (TypePointer
            (TypeFun
              [
                TypePointer
                  (TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "int2int",
                        nameHsIdent = Identifier
                          "Int2int"}
                      (TypeFun
                        [
                          TypePrim
                            (PrimIntegral PrimInt Signed)]
                        (TypePrim
                          (PrimIntegral
                            PrimInt
                            Signed))))),
                TypePrim
                  (PrimIntegral PrimInt Signed)]
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_manualfunction_pointers_6799ff6bc99dff2a",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Apply1Struct")))),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_6799ff6bc99dff2a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_apply1_struct_ptr */\n",
              "__attribute__ ((const))\n",
              "struct Apply1Struct const *hs_bindgen_test_manualfunction_pointers_6799ff6bc99dff2a (void)\n",
              "{\n",
              "  return &apply1_struct;\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Global
        (TypeQualified
          TypeQualifierConst
          (TypeStruct
            NamePair {
              nameC = Name "Apply1Struct",
              nameHsIdent = Identifier
                "Apply1Struct"}
            NameOriginInSource)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_manualfunction_pointers_d32b4879673188b6",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Apply1Union")))),
      foreignImportOrigName =
      "hs_bindgen_test_manualfunction_pointers_d32b4879673188b6",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_apply1_union_ptr */\n",
              "__attribute__ ((const))\n",
              "union Apply1Union const *hs_bindgen_test_manualfunction_pointers_d32b4879673188b6 (void)\n",
              "{\n",
              "  return &apply1_union;\n",
              "}"],
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Global
        (TypeQualified
          TypeQualifierConst
          (TypeUnion
            NamePair {
              nameC = Name "Apply1Union",
              nameHsIdent = Identifier
                "Apply1Union"}
            NameOriginInSource)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple]
