[
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
                (HsFun
                  (HsPrimType HsPrimCInt)
                  (HsIO (HsPrimType HsPrimCInt))))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimCInt)))),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "function_pointers.h:36:16",
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
                        (TypeFun
                          [
                            TypePrim
                              (PrimIntegral PrimInt Signed)]
                          (TypePrim
                            (PrimIntegral PrimInt Signed))),
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
                "function_pointers.h:36:16",
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
            "function_pointers.h:35:8",
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
                    "function_pointers.h:36:16",
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
                            (TypeFun
                              [
                                TypePrim
                                  (PrimIntegral PrimInt Signed)]
                              (TypePrim
                                (PrimIntegral PrimInt Signed))),
                          TypePrim
                            (PrimIntegral PrimInt Signed)]
                        (TypePrim
                          (PrimIntegral
                            PrimInt
                            Signed)))),
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
            "function_pointers.h:35:8",
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
                    (HsFun
                      (HsPrimType HsPrimCInt)
                      (HsIO (HsPrimType HsPrimCInt))))
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsIO
                      (HsPrimType HsPrimCInt)))),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "function_pointers.h:36:16",
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
                            (TypeFun
                              [
                                TypePrim
                                  (PrimIntegral PrimInt Signed)]
                              (TypePrim
                                (PrimIntegral PrimInt Signed))),
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
                    "function_pointers.h:36:16",
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
                "function_pointers.h:35:8",
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
                        "function_pointers.h:36:16",
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
                                (TypeFun
                                  [
                                    TypePrim
                                      (PrimIntegral PrimInt Signed)]
                                  (TypePrim
                                    (PrimIntegral PrimInt Signed))),
                              TypePrim
                                (PrimIntegral PrimInt Signed)]
                            (TypePrim
                              (PrimIntegral
                                PrimInt
                                Signed)))),
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
                "function_pointers.h:35:8",
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
                            (HsFun
                              (HsPrimType HsPrimCInt)
                              (HsIO (HsPrimType HsPrimCInt))))
                          (HsFun
                            (HsPrimType HsPrimCInt)
                            (HsIO
                              (HsPrimType HsPrimCInt)))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "function_pointers.h:36:16",
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
                                    (TypeFun
                                      [
                                        TypePrim
                                          (PrimIntegral PrimInt Signed)]
                                      (TypePrim
                                        (PrimIntegral PrimInt Signed))),
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
                            "function_pointers.h:36:16",
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
                        "function_pointers.h:35:8",
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
                                "function_pointers.h:36:16",
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
                                        (TypeFun
                                          [
                                            TypePrim
                                              (PrimIntegral PrimInt Signed)]
                                          (TypePrim
                                            (PrimIntegral PrimInt Signed))),
                                      TypePrim
                                        (PrimIntegral PrimInt Signed)]
                                    (TypePrim
                                      (PrimIntegral
                                        PrimInt
                                        Signed)))),
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
                        "function_pointers.h:35:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["manual/function_pointers.h"],
                          headerInclude =
                          "manual/function_pointers.h"},
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
                            (HsFun
                              (HsPrimType HsPrimCInt)
                              (HsIO (HsPrimType HsPrimCInt))))
                          (HsFun
                            (HsPrimType HsPrimCInt)
                            (HsIO
                              (HsPrimType HsPrimCInt)))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "function_pointers.h:36:16",
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
                                    (TypeFun
                                      [
                                        TypePrim
                                          (PrimIntegral PrimInt Signed)]
                                      (TypePrim
                                        (PrimIntegral PrimInt Signed))),
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
                            "function_pointers.h:36:16",
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
                        "function_pointers.h:35:8",
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
                                "function_pointers.h:36:16",
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
                                        (TypeFun
                                          [
                                            TypePrim
                                              (PrimIntegral PrimInt Signed)]
                                          (TypePrim
                                            (PrimIntegral PrimInt Signed))),
                                      TypePrim
                                        (PrimIntegral PrimInt Signed)]
                                    (TypePrim
                                      (PrimIntegral
                                        PrimInt
                                        Signed)))),
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
                        "function_pointers.h:35:8",
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
          "function_pointers.h:41:7",
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
                  "function_pointers.h:42:16",
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
                          (TypeFun
                            [
                              TypePrim
                                (PrimIntegral PrimInt Signed)]
                            (TypePrim
                              (PrimIntegral PrimInt Signed))),
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed))))}]},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
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
            "function_pointers.h:41:7",
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
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimCInt))))
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
            "function_pointers.h:42:16",
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
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimCInt))))
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
          "signed int hs_bindgen_test_manualfunction_pointers_55e5eb89e54abf83 (signed int arg1) { return square(arg1); }",
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
          "signed int hs_bindgen_test_manualfunction_pointers_680daf766a044980 (signed int arg1, signed int arg2) { return plus(arg1, arg2); }",
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
          "signed int hs_bindgen_test_manualfunction_pointers_abcb860034253564 (signed int (*arg1) (signed int arg1), signed int arg2) { return apply1(arg1, arg2); }",
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
          "signed int hs_bindgen_test_manualfunction_pointers_1ad13c166a710f40 (signed int (*arg1) (signed int arg1, signed int arg2), signed int arg2, signed int arg3) { return apply2(arg1, arg2, arg3); }",
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
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimCInt))),
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
          "signed int hs_bindgen_test_manualfunction_pointers_a8ef4d9e6ce68f54 (signed int (*arg1) (signed int arg1), signed int arg2) { return apply1_pointer_arg(arg1, arg2); }",
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
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
            "function_pointers.h:20:12",
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
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimCInt))),
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
          "signed int hs_bindgen_test_manualfunction_pointers_7dc4caa1f7f0caf0 (signed int (*arg1) (signed int arg1), signed int arg2) { return apply1_nopointer_arg(arg1, arg2); }",
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
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
            "function_pointers.h:24:12",
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
                (HsFun
                  (HsPrimType HsPrimCInt)
                  (HsIO (HsPrimType HsPrimCInt))))
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
          "signed int (*const hs_bindgen_test_manualfunction_pointers_3612aa0d10e36d5b (void)) (signed int (*arg1) (signed int arg1), signed int arg2) { return apply1_nopointer_res(); }",
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
                    (TypeFun
                      [
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      (TypePrim
                        (PrimIntegral PrimInt Signed))),
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
            "function_pointers.h:29:21",
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
          "signed int hs_bindgen_test_manualfunction_pointers_cb3c687f16289bb3 (signed int arg1) { return square(arg1); }",
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
          "signed int hs_bindgen_test_manualfunction_pointers_a9730564387164c0 (signed int arg1, signed int arg2) { return plus(arg1, arg2); }",
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
          "signed int hs_bindgen_test_manualfunction_pointers_3fb9c4a14d502477 (signed int (*arg1) (signed int arg1), signed int arg2) { return apply1(arg1, arg2); }",
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
          "signed int hs_bindgen_test_manualfunction_pointers_75b5699fdabb6333 (signed int (*arg1) (signed int arg1, signed int arg2), signed int arg2, signed int arg3) { return apply2(arg1, arg2, arg3); }",
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
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimCInt))),
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
          "signed int hs_bindgen_test_manualfunction_pointers_2d5144fc06502862 (signed int (*arg1) (signed int arg1), signed int arg2) { return apply1_pointer_arg(arg1, arg2); }",
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
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
            "function_pointers.h:20:12",
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
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimCInt))),
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
          "signed int hs_bindgen_test_manualfunction_pointers_b7597a0c4856ebb3 (signed int (*arg1) (signed int arg1), signed int arg2) { return apply1_nopointer_arg(arg1, arg2); }",
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
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
            "function_pointers.h:24:12",
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
                (HsFun
                  (HsPrimType HsPrimCInt)
                  (HsIO (HsPrimType HsPrimCInt))))
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
          "signed int (*const hs_bindgen_test_manualfunction_pointers_be3907895c70597f (void)) (signed int (*arg1) (signed int arg1), signed int arg2) { return apply1_nopointer_res(); }",
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
                    (TypeFun
                      [
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      (TypePrim
                        (PrimIntegral PrimInt Signed))),
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
            "function_pointers.h:29:21",
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
          "/* get_square_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_manualfunction_pointers_c41111f40a04cdc9 (void)) (signed int arg1) { return &square; } ",
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
          "/* get_plus_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_manualfunction_pointers_bf838c747898dc42 (void)) (signed int arg1, signed int arg2) { return &plus; } ",
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
          "/* get_apply1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_manualfunction_pointers_4d1935e01bc37070 (void)) (signed int (*arg1) (signed int arg1), signed int arg2) { return &apply1; } ",
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
          "/* get_apply2_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_manualfunction_pointers_40cb8952bacd236a (void)) (signed int (*arg1) (signed int arg1, signed int arg2), signed int arg2, signed int arg3) { return &apply2; } ",
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
                (HsFun
                  (HsPrimType HsPrimCInt)
                  (HsIO (HsPrimType HsPrimCInt))))
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
          "/* get_apply1_pointer_arg_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_manualfunction_pointers_653c5bde7704c3ca (void)) (signed int (*arg1) (signed int arg1), signed int arg2) { return &apply1_pointer_arg; } ",
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
        "hs_bindgen_test_manualfunction_pointers_3bb9417cd7afec81",
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
      "hs_bindgen_test_manualfunction_pointers_3bb9417cd7afec81",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_apply1_nopointer_arg_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_manualfunction_pointers_3bb9417cd7afec81 (void)) (signed int (*arg1) (signed int arg1), signed int arg2) { return &apply1_nopointer_arg; } ",
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
                    (HsFun
                      (HsPrimType HsPrimCInt)
                      (HsIO (HsPrimType HsPrimCInt))))
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
          "/* get_apply1_nopointer_res_ptr */ __attribute__ ((const)) signed int (*const (*hs_bindgen_test_manualfunction_pointers_f7a08d090f6f7b0f (void)) (void)) (signed int (*arg1) (signed int arg1), signed int arg2) { return &apply1_nopointer_res; } ",
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
                    (TypeFun
                      [
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      (TypePrim
                        (PrimIntegral PrimInt Signed))),
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
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsIO (HsPrimType HsPrimCInt))))
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
          "/* get_apply1_nopointer_var_ptr */ __attribute__ ((const)) signed int (*const *hs_bindgen_test_manualfunction_pointers_c4bb317da29227a6 (void)) (signed int (*arg1) (signed int arg1), signed int arg2) { return &apply1_nopointer_var; } ",
          capiWrapperImport =
          "manual/function_pointers.h"},
      foreignImportOrigin = Global
        (TypeQualified
          TypeQualifierConst
          (TypePointer
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
          "/* get_apply1_struct_ptr */ __attribute__ ((const)) struct Apply1Struct const *hs_bindgen_test_manualfunction_pointers_6799ff6bc99dff2a (void) { return &apply1_struct; } ",
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
          "/* get_apply1_union_ptr */ __attribute__ ((const)) union Apply1Union const *hs_bindgen_test_manualfunction_pointers_d32b4879673188b6 (void) { return &apply1_union; } ",
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
