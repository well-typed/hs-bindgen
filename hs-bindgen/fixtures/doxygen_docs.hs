[
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "mAX_NAME_LENGTH",
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
        64
        HsPrimCInt,
      varDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "MAX_NAME_LENGTH",
          commentLocation = Just
            "doxygen_docs.h:39:9",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = []}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Size_type",
      newtypeConstr = HsName
        "@NsConstr"
        "Size_type",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Size_type",
        fieldType = HsExtBinding
          ExtHsRef {
            extHsRefModule = HsModuleName
              "HsBindgen.Runtime.Prelude",
            extHsRefIdentifier =
            HsIdentifier "CSize"}
          TypeSpec {
            typeSpecModule = Just
              (HsModuleName
                "HsBindgen.Runtime.Prelude"),
            typeSpecIdentifier = Just
              (HsIdentifier "CSize"),
            typeSpecInstances = Map.fromList
              [
                _×_
                  Eq
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
                  Enum
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
                  Bounded
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
                  Show
                  (Require
                    InstanceSpec {
                      instanceSpecStrategy = Nothing,
                      instanceSpecConstraints = []}),
                _×_
                  Bits
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
                  Num
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
                  StaticSize
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
                  WriteRaw
                  (Require
                    InstanceSpec {
                      instanceSpecStrategy = Nothing,
                      instanceSpecConstraints = []}),
                _×_
                  Storable
                  (Require
                    InstanceSpec {
                      instanceSpecStrategy = Nothing,
                      instanceSpecConstraints = [
                        ]})]},
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "doxygen_docs.h:54:16",
          declId = NamePair {
            nameC = Name "size_type",
            nameHsIdent = HsIdentifier
              "Size_type"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "doxygen_docs.h",
          declComment = Just
            (Comment
              [
                Paragraph
                  [
                    TextContent
                      "This is the comment",
                    InlineCommand {
                      inlineCommandName = "b",
                      inlineCommandRenderKind =
                      CXCommentInlineCommandRenderKind_Bold,
                      inlineCommandArgs = ["title"]},
                    TextContent ""],
                VerbatimLine "size_type",
                Paragraph [TextContent ""],
                BlockCommand {
                  blockCommandName = "brief",
                  blockCommandArgs = [],
                  blockCommandParagraph = [
                    TextContent
                      "Size type for this library"]}])},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Size_type",
              newtypeField = HsName
                "@NsVar"
                "un_Size_type"},
            typedefType = TypeExtBinding
              ResolvedExtBinding {
                extCName = QualName {
                  qualNameName = Name "size_t",
                  qualNameKind =
                  NameKindOrdinary},
                extHsRef = ExtHsRef {
                  extHsRefModule = HsModuleName
                    "HsBindgen.Runtime.Prelude",
                  extHsRefIdentifier =
                  HsIdentifier "CSize"},
                extHsSpec = TypeSpec {
                  typeSpecModule = Just
                    (HsModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (HsIdentifier "CSize"),
                  typeSpecInstances = Map.fromList
                    [
                      _×_
                        Eq
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
                        Enum
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
                        Bounded
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
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bits
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
                        Num
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
                        StaticSize
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
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]}}},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [
          Eq,
          Ord,
          Enum,
          Ix,
          Bounded,
          Read,
          Show,
          Bits,
          FiniteBits,
          Integral,
          Num,
          Real,
          Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "This is the comment",
              Bold [TextContent "title"]],
          commentOrigin = Just
            "size_type",
          commentLocation = Just
            "doxygen_docs.h:54:16",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Verbatim "size_type",
            Paragraph
              [
                TextContent
                  "Size type for this library"]]}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_type",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_type",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_type",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_type",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_type",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_type",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_type",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_type",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_type",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass =
      FiniteBits,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_type",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_type",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_type",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_type",
      deriveInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_doxygen_docs_7c3b3bc7cc470742",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_7c3b3bc7cc470742",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_global_counter_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_doxygen_docs_7c3b3bc7cc470742 (void) { return &global_counter; } ",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Global
        (TypePrim
          (PrimIntegral PrimInt Signed)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "global_counter",
          commentLocation = Just
            "doxygen_docs.h:61:12",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Verbatim
              "extern int global_counter",
            Paragraph
              [
                TextContent
                  "Global counter variable"],
            Paragraph
              [
                TextContent
                  "This variable tracks the number of operations performed."]]},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_doxygen_docs_a8499810bd5203c6",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPtr
              (HsPrimType HsPrimCChar)))),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_a8499810bd5203c6",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_version_string_ptr */ __attribute__ ((const)) char const **hs_bindgen_test_doxygen_docs_a8499810bd5203c6 (void) { return &version_string; } ",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Global
        (TypePointer
          (TypeConst
            (TypePrim
              (PrimChar
                (PrimSignImplicit
                  (Just Signed)))))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "version_string",
          commentLocation = Just
            "doxygen_docs.h:67:20",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Verbatim
              "extern const char* version_string",
            Paragraph
              [
                TextContent
                  "Version string constant"]]},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclEmpty
    EmptyData {
      emptyDataName = HsName
        "@NsTypeConstr"
        "Forward_declared_struct",
      emptyDataOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "doxygen_docs.h:72:8",
          declId = NamePair {
            nameC = Name
              "forward_declared_struct",
            nameHsIdent = HsIdentifier
              "Forward_declared_struct"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "doxygen_docs.h",
          declComment = Just
            (Comment
              [
                Paragraph
                  [
                    TextContent
                      "This is the comment",
                    InlineCommand {
                      inlineCommandName = "c",
                      inlineCommandRenderKind =
                      CXCommentInlineCommandRenderKind_Monospaced,
                      inlineCommandArgs = ["title"]},
                    TextContent ""],
                BlockCommand {
                  blockCommandName = "brief",
                  blockCommandArgs = [],
                  blockCommandParagraph = [
                    TextContent
                      "Forward declaration with documentation"]}])},
        declKind = OpaqueStruct,
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      emptyDataComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "This is the comment",
              Monospace
                [TextContent "title"]],
          commentOrigin = Just
            "forward_declared_struct",
          commentLocation = Just
            "doxygen_docs.h:72:8",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Forward declaration with documentation"]]}},
  DeclEmpty
    EmptyData {
      emptyDataName = HsName
        "@NsTypeConstr"
        "Forward_declared_union",
      emptyDataOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "doxygen_docs.h:77:7",
          declId = NamePair {
            nameC = Name
              "forward_declared_union",
            nameHsIdent = HsIdentifier
              "Forward_declared_union"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "doxygen_docs.h",
          declComment = Just
            (Comment
              [
                Paragraph [TextContent ""],
                BlockCommand {
                  blockCommandName = "brief",
                  blockCommandArgs = [],
                  blockCommandParagraph = [
                    TextContent
                      "Forward declaration of union"]}])},
        declKind = OpaqueUnion,
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      emptyDataComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "forward_declared_union",
          commentLocation = Just
            "doxygen_docs.h:77:7",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Forward declaration of union"]]}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Color_enum",
      newtypeConstr = HsName
        "@NsConstr"
        "Color_enum",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Color_enum",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "doxygen_docs.h:83:6",
          declId = NamePair {
            nameC = Name "color_enum",
            nameHsIdent = HsIdentifier
              "Color_enum"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "doxygen_docs.h",
          declComment = Just
            (Comment
              [
                Paragraph [TextContent ""],
                VerbatimLine "color_enum",
                Paragraph [TextContent ""],
                BlockCommand {
                  blockCommandName = "brief",
                  blockCommandArgs = [],
                  blockCommandParagraph = [
                    TextContent
                      "Color enumeration without typedef"]}])},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Color_enum",
              newtypeField = HsName
                "@NsVar"
                "un_Color_enum"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "doxygen_docs.h:84:5",
                  fieldName = NamePair {
                    nameC = Name "COLOR_RED",
                    nameHsIdent = HsIdentifier
                      "COLOR_RED"},
                  fieldComment = Just
                    (Comment
                      [
                        Paragraph
                          [TextContent "Red color"]])},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "doxygen_docs.h:85:5",
                  fieldName = NamePair {
                    nameC = Name "COLOR_GREEN",
                    nameHsIdent = HsIdentifier
                      "COLOR_GREEN"},
                  fieldComment = Just
                    (Comment
                      [
                        Paragraph
                          [TextContent "Green color"]])},
                enumConstantValue = 1},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "doxygen_docs.h:86:5",
                  fieldName = NamePair {
                    nameC = Name "COLOR_BLUE",
                    nameHsIdent = HsIdentifier
                      "COLOR_BLUE"},
                  fieldComment = Just
                    (Comment
                      [
                        Paragraph
                          [TextContent "Blue color"]])},
                enumConstantValue = 2}]},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Read, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "color_enum",
          commentLocation = Just
            "doxygen_docs.h:83:6",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Verbatim "color_enum",
            Paragraph
              [
                TextContent
                  "Color enumeration without typedef"]]}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Color_enum",
          structConstr = HsName
            "@NsConstr"
            "Color_enum",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_Color_enum",
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
                  structName = HsName
                    "@NsTypeConstr"
                    "Color_enum",
                  structConstr = HsName
                    "@NsConstr"
                    "Color_enum",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "un_Color_enum",
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
                  structName = HsName
                    "@NsTypeConstr"
                    "Color_enum",
                  structConstr = HsName
                    "@NsConstr"
                    "Color_enum",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "un_Color_enum",
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Color_enum",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Color_enum",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Color_enum",
          structConstr = HsName
            "@NsConstr"
            "Color_enum",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_Color_enum",
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
            _×_
              0
              (NE.fromList ["COLOR_RED"]),
            _×_
              1
              (NE.fromList ["COLOR_GREEN"]),
            _×_
              2
              (NE.fromList ["COLOR_BLUE"])])
        True,
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceSequentialCEnum
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Color_enum",
          structConstr = HsName
            "@NsConstr"
            "Color_enum",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_Color_enum",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (HsName "@NsConstr" "COLOR_RED")
        (HsName
          "@NsConstr"
          "COLOR_BLUE"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Color_enum",
          structConstr = HsName
            "@NsConstr"
            "Color_enum",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_Color_enum",
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
          structName = HsName
            "@NsTypeConstr"
            "Color_enum",
          structConstr = HsName
            "@NsConstr"
            "Color_enum",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_Color_enum",
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
      patSynName = HsName
        "@NsConstr"
        "COLOR_RED",
      patSynType = HsName
        "@NsTypeConstr"
        "Color_enum",
      patSynConstr = HsName
        "@NsConstr"
        "Color_enum",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "doxygen_docs.h:84:5",
            fieldName = NamePair {
              nameC = Name "COLOR_RED",
              nameHsIdent = HsIdentifier
                "COLOR_RED"},
            fieldComment = Just
              (Comment
                [
                  Paragraph
                    [TextContent "Red color"]])},
          enumConstantValue = 0},
      patSynComment = Just
        Comment {
          commentTitle = Just
            [TextContent "Red color"],
          commentOrigin = Just
            "COLOR_RED",
          commentLocation = Just
            "doxygen_docs.h:84:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = []}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "COLOR_GREEN",
      patSynType = HsName
        "@NsTypeConstr"
        "Color_enum",
      patSynConstr = HsName
        "@NsConstr"
        "Color_enum",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "doxygen_docs.h:85:5",
            fieldName = NamePair {
              nameC = Name "COLOR_GREEN",
              nameHsIdent = HsIdentifier
                "COLOR_GREEN"},
            fieldComment = Just
              (Comment
                [
                  Paragraph
                    [TextContent "Green color"]])},
          enumConstantValue = 1},
      patSynComment = Just
        Comment {
          commentTitle = Just
            [TextContent "Green color"],
          commentOrigin = Just
            "COLOR_GREEN",
          commentLocation = Just
            "doxygen_docs.h:85:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = []}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "COLOR_BLUE",
      patSynType = HsName
        "@NsTypeConstr"
        "Color_enum",
      patSynConstr = HsName
        "@NsConstr"
        "Color_enum",
      patSynValue = 2,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "doxygen_docs.h:86:5",
            fieldName = NamePair {
              nameC = Name "COLOR_BLUE",
              nameHsIdent = HsIdentifier
                "COLOR_BLUE"},
            fieldComment = Just
              (Comment
                [
                  Paragraph
                    [TextContent "Blue color"]])},
          enumConstantValue = 2},
      patSynComment = Just
        Comment {
          commentTitle = Just
            [TextContent "Blue color"],
          commentOrigin = Just
            "COLOR_BLUE",
          commentLocation = Just
            "doxygen_docs.h:86:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "process_data",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "input_data"),
          functionParameterType = HsPtr
            (HsExtBinding
              ExtHsRef {
                extHsRefModule = HsModuleName
                  "HsBindgen.Runtime.Prelude",
                extHsRefIdentifier =
                HsIdentifier "Word8"}
              TypeSpec {
                typeSpecModule = Just
                  (HsModuleName
                    "HsBindgen.Runtime.Prelude"),
                typeSpecIdentifier = Just
                  (HsIdentifier "Word8"),
                typeSpecInstances = Map.fromList
                  [
                    _×_
                      Eq
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
                      Enum
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
                      Bounded
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
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bits
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
                      Num
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
                      StaticSize
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
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]}),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "input_data",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = [
                DefinitionList {
                  definitionListTerm = Bold
                    [
                      Monospace
                        [TextContent "input_data"],
                      Emph [TextContent "(input)"]],
                  definitionListContent = [
                    Paragraph
                      [
                        TextContent
                          "Input data buffer"]]}]}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "output_data"),
          functionParameterType = HsPtr
            (HsExtBinding
              ExtHsRef {
                extHsRefModule = HsModuleName
                  "HsBindgen.Runtime.Prelude",
                extHsRefIdentifier =
                HsIdentifier "Word8"}
              TypeSpec {
                typeSpecModule = Just
                  (HsModuleName
                    "HsBindgen.Runtime.Prelude"),
                typeSpecIdentifier = Just
                  (HsIdentifier "Word8"),
                typeSpecInstances = Map.fromList
                  [
                    _×_
                      Eq
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
                      Enum
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
                      Bounded
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
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bits
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
                      Num
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
                      StaticSize
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
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]}),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "output_data",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = [
                DefinitionList {
                  definitionListTerm = Bold
                    [
                      Monospace
                        [TextContent "output_data"],
                      Emph [TextContent "(output)"]],
                  definitionListContent = [
                    Paragraph
                      [
                        TextContent
                          "Output data buffer"]]}]}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "size"),
          functionParameterType = HsPtr
            (HsExtBinding
              ExtHsRef {
                extHsRefModule = HsModuleName
                  "HsBindgen.Runtime.Prelude",
                extHsRefIdentifier =
                HsIdentifier "CSize"}
              TypeSpec {
                typeSpecModule = Just
                  (HsModuleName
                    "HsBindgen.Runtime.Prelude"),
                typeSpecIdentifier = Just
                  (HsIdentifier "CSize"),
                typeSpecInstances = Map.fromList
                  [
                    _×_
                      Eq
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
                      Enum
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
                      Bounded
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
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bits
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
                      Num
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
                      StaticSize
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
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]}),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "size",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = [
                DefinitionList {
                  definitionListTerm = Bold
                    [
                      Monospace [TextContent "size"],
                      Emph
                        [TextContent "(input,output)"]],
                  definitionListContent = [
                    Paragraph
                      [
                        TextContent
                          "Size of data, updated on return"]]}]}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_d3011436e073569c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_doxygen_docs_d3011436e073569c (uint8_t const *arg1, uint8_t *arg2, size_t *arg3) { return process_data(arg1, arg2, arg3); }",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "input_data",
                  nameHsIdent = HsIdentifier
                    "input_data"})
              (TypePointer
                (TypeConst
                  (TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint8_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word8"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word8"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
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
                              Enum
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
                              Bounded
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
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
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
                              Num
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
                              StaticSize
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
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}}))),
            _×_
              (Just
                NamePair {
                  nameC = Name "output_data",
                  nameHsIdent = HsIdentifier
                    "output_data"})
              (TypePointer
                (TypeExtBinding
                  ResolvedExtBinding {
                    extCName = QualName {
                      qualNameName = Name "uint8_t",
                      qualNameKind =
                      NameKindOrdinary},
                    extHsRef = ExtHsRef {
                      extHsRefModule = HsModuleName
                        "HsBindgen.Runtime.Prelude",
                      extHsRefIdentifier =
                      HsIdentifier "Word8"},
                    extHsSpec = TypeSpec {
                      typeSpecModule = Just
                        (HsModuleName
                          "HsBindgen.Runtime.Prelude"),
                      typeSpecIdentifier = Just
                        (HsIdentifier "Word8"),
                      typeSpecInstances = Map.fromList
                        [
                          _×_
                            Eq
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
                            Enum
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
                            Bounded
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
                            Show
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bits
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
                            Num
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
                            StaticSize
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
                            WriteRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Storable
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = [
                                  ]})]}})),
            _×_
              (Just
                NamePair {
                  nameC = Name "size",
                  nameHsIdent = HsIdentifier
                    "size"})
              (TypePointer
                (TypeExtBinding
                  ResolvedExtBinding {
                    extCName = QualName {
                      qualNameName = Name "size_t",
                      qualNameKind =
                      NameKindOrdinary},
                    extHsRef = ExtHsRef {
                      extHsRefModule = HsModuleName
                        "HsBindgen.Runtime.Prelude",
                      extHsRefIdentifier =
                      HsIdentifier "CSize"},
                    extHsSpec = TypeSpec {
                      typeSpecModule = Just
                        (HsModuleName
                          "HsBindgen.Runtime.Prelude"),
                      typeSpecIdentifier = Just
                        (HsIdentifier "CSize"),
                      typeSpecInstances = Map.fromList
                        [
                          _×_
                            Eq
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
                            Enum
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
                            Bounded
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
                            Show
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bits
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
                            Num
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
                            StaticSize
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
                            WriteRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Storable
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = [
                                  ]})]}}))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment =
      Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "process_data",
          commentLocation = Just
            "doxygen_docs.h:105:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren =
          [
            Paragraph
              [
                TextContent
                  "Function with detailed parameter documentation"],
            Paragraph
              [
                TextContent
                  "This function shows different parameter directions and types."],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace
                    [TextContent "input_data"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Input data buffer"]]},
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace
                    [TextContent "output_data"],
                  Emph [TextContent "(output)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Output data buffer"]]},
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "size"],
                  Emph
                    [TextContent "(input,output)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Size of data, updated on return"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent
                  "Status code (0 = success, -1 = error)"]]},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_doxygen_docs_36ecba874daa1325",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr
                (HsExtBinding
                  ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "Word8"}
                  TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "Word8"),
                    typeSpecInstances = Map.fromList
                      [
                        _×_
                          Eq
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
                          Enum
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
                          Bounded
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
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bits
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
                          Num
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
                          StaticSize
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
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}))
              (HsFun
                (HsPtr
                  (HsExtBinding
                    ExtHsRef {
                      extHsRefModule = HsModuleName
                        "HsBindgen.Runtime.Prelude",
                      extHsRefIdentifier =
                      HsIdentifier "Word8"}
                    TypeSpec {
                      typeSpecModule = Just
                        (HsModuleName
                          "HsBindgen.Runtime.Prelude"),
                      typeSpecIdentifier = Just
                        (HsIdentifier "Word8"),
                      typeSpecInstances = Map.fromList
                        [
                          _×_
                            Eq
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
                            Enum
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
                            Bounded
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
                            Show
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bits
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
                            Num
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
                            StaticSize
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
                            WriteRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Storable
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = [
                                  ]})]}))
                (HsFun
                  (HsPtr
                    (HsExtBinding
                      ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "CSize"}
                      TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "CSize"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
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
                              Enum
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
                              Bounded
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
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
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
                              Num
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
                              StaticSize
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
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}))
                  (HsIO
                    (HsPrimType HsPrimCInt))))))),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_36ecba874daa1325",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_process_data_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_doxygen_docs_36ecba874daa1325 (void)) (uint8_t const *arg1, uint8_t *arg2, size_t *arg3) { return &process_data; } ",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeConst
                (TypeExtBinding
                  ResolvedExtBinding {
                    extCName = QualName {
                      qualNameName = Name "uint8_t",
                      qualNameKind =
                      NameKindOrdinary},
                    extHsRef = ExtHsRef {
                      extHsRefModule = HsModuleName
                        "HsBindgen.Runtime.Prelude",
                      extHsRefIdentifier =
                      HsIdentifier "Word8"},
                    extHsSpec = TypeSpec {
                      typeSpecModule = Just
                        (HsModuleName
                          "HsBindgen.Runtime.Prelude"),
                      typeSpecIdentifier = Just
                        (HsIdentifier "Word8"),
                      typeSpecInstances = Map.fromList
                        [
                          _×_
                            Eq
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
                            Enum
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
                            Bounded
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
                            Show
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bits
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
                            Num
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
                            StaticSize
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
                            WriteRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Storable
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = [
                                  ]})]}})),
            TypePointer
              (TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "uint8_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "Word8"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "Word8"),
                    typeSpecInstances = Map.fromList
                      [
                        _×_
                          Eq
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
                          Enum
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
                          Bounded
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
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bits
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
                          Num
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
                          StaticSize
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
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}}),
            TypePointer
              (TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "size_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "CSize"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "CSize"),
                    typeSpecInstances = Map.fromList
                      [
                        _×_
                          Eq
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
                          Enum
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
                          Bounded
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
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bits
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
                          Num
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
                          StaticSize
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
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}})]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment =
      Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "process_data",
          commentLocation = Just
            "doxygen_docs.h:105:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren =
          [
            Paragraph
              [
                TextContent
                  "Function with detailed parameter documentation"],
            Paragraph
              [
                TextContent
                  "This function shows different parameter directions and types."],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace
                    [TextContent "input_data"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Input data buffer"]]},
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace
                    [TextContent "output_data"],
                  Emph [TextContent "(output)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Output data buffer"]]},
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "size"],
                  Emph
                    [TextContent "(input,output)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Size of data, updated on return"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent
                  "Status code (0 = success, -1 = error)"]]},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "process_file",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "filename"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCChar),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "filename",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = [
                DefinitionList {
                  definitionListTerm = Bold
                    [
                      Monospace
                        [TextContent "filename"],
                      Emph [TextContent "(input)"]],
                  definitionListContent = [
                    Paragraph
                      [
                        TextContent "The",
                        Monospace [TextContent "char*"],
                        TextContent
                          "filename to process"]]}]}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCBool)),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_4e9b999ed81440c0",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "_Bool hs_bindgen_test_doxygen_docs_4e9b999ed81440c0 (char *arg1) { return process_file(arg1); }",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "filename",
                  nameHsIdent = HsIdentifier
                    "filename"})
              (TypePointer
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit Nothing))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            PrimBool},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "process_file",
          commentLocation = Just
            "doxygen_docs.h:116:6",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Function with inline commands and formatting"],
            Paragraph
              [
                TextContent
                  "This function uses",
                Monospace
                  [TextContent "inline"],
                Monospace [TextContent "code"],
                TextContent "formatting and",
                Bold [TextContent "bold"],
                TextContent "text.",
                TextContent
                  "It also demonstrates",
                Emph [TextContent "emphasized"],
                TextContent "text."],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace
                    [TextContent "filename"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent "The",
                    Monospace [TextContent "char*"],
                    TextContent
                      "filename to process"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                Monospace [TextContent "true"],
                TextContent "if successful,",
                Monospace [TextContent "false"],
                TextContent "otherwise"]]},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_doxygen_docs_25908c11fd21c934",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr (HsPrimType HsPrimCChar))
              (HsIO
                (HsPrimType HsPrimCBool))))),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_25908c11fd21c934",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_process_file_ptr */ __attribute__ ((const)) _Bool (*hs_bindgen_test_doxygen_docs_25908c11fd21c934 (void)) (char *arg1) { return &process_file; } ",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypePrim
                (PrimChar
                  (PrimSignImplicit Nothing)))]
          (TypePrim PrimBool)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "process_file",
          commentLocation = Just
            "doxygen_docs.h:116:6",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Function with inline commands and formatting"],
            Paragraph
              [
                TextContent
                  "This function uses",
                Monospace
                  [TextContent "inline"],
                Monospace [TextContent "code"],
                TextContent "formatting and",
                Bold [TextContent "bold"],
                TextContent "text.",
                TextContent
                  "It also demonstrates",
                Emph [TextContent "emphasized"],
                TextContent "text."],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace
                    [TextContent "filename"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent "The",
                    Monospace [TextContent "char*"],
                    TextContent
                      "filename to process"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                Monospace [TextContent "true"],
                TextContent "if successful,",
                Monospace [TextContent "false"],
                TextContent "otherwise"]]},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "calculate_value",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "base"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "base",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = [
                DefinitionList {
                  definitionListTerm = Bold
                    [
                      Monospace [TextContent "base"],
                      Emph [TextContent "(input)"]],
                  definitionListContent = [
                    Paragraph
                      [TextContent "Base value"]]}]}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "multiplier"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "multiplier",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = [
                DefinitionList {
                  definitionListTerm = Bold
                    [
                      Monospace
                        [TextContent "multiplier"],
                      Emph [TextContent "(input)"]],
                  definitionListContent = [
                    Paragraph
                      [
                        TextContent
                          "Multiplier value"]]}]}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_788bfbb9e824261c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_doxygen_docs_788bfbb9e824261c (signed int arg1, signed int arg2) { return calculate_value(arg1, arg2); }",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "base",
                  nameHsIdent = HsIdentifier
                    "base"})
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              (Just
                NamePair {
                  nameC = Name "multiplier",
                  nameHsIdent = HsIdentifier
                    "multiplier"})
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
          commentOrigin = Just
            "calculate_value",
          commentLocation = Just
            "doxygen_docs.h:131:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Function with verbatim code blocks"],
            Paragraph
              [TextContent "Example usage:"],
            CodeBlock
              [
                "int result = calculate_value(10, 20);",
                "printf(\"Result: %d@n\", result);"],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "base"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [TextContent "Base value"]]},
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace
                    [TextContent "multiplier"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Multiplier value"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent
                  "Calculated result"]]},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_doxygen_docs_bfa9c55857b9f9a4",
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
      "hs_bindgen_test_doxygen_docs_bfa9c55857b9f9a4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_calculate_value_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_doxygen_docs_bfa9c55857b9f9a4 (void)) (signed int arg1, signed int arg2) { return &calculate_value; } ",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypePrim
              (PrimIntegral PrimInt Signed)]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "calculate_value",
          commentLocation = Just
            "doxygen_docs.h:131:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Function with verbatim code blocks"],
            Paragraph
              [TextContent "Example usage:"],
            CodeBlock
              [
                "int result = calculate_value(10, 20);",
                "printf(\"Result: %d@n\", result);"],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "base"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [TextContent "Base value"]]},
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace
                    [TextContent "multiplier"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Multiplier value"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent
                  "Calculated result"]]},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "html_example",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "value"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "value",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = [
                DefinitionList {
                  definitionListTerm = Bold
                    [
                      Monospace [TextContent "value"],
                      Emph [TextContent "(input)"]],
                  definitionListContent = [
                    Paragraph
                      [
                        TextContent
                          "Input value"]]}]}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCBool)),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_3763cc54606bc611",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "_Bool hs_bindgen_test_doxygen_docs_3763cc54606bc611 (signed int arg1) { return html_example(arg1); }",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "value",
                  nameHsIdent = HsIdentifier
                    "value"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            PrimBool},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "html_example",
          commentLocation = Just
            "doxygen_docs.h:148:6",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Function with HTML formatting"],
            Paragraph
              [
                TextContent
                  "This function demonstrates",
                TextContent "HTML bold",
                TextContent "and",
                TextContent "italic",
                TextContent "text.",
                TextContent "It also shows",
                TextContent "HTML code",
                TextContent "formatting."],
            Paragraph
              [
                TextContent "Input",
                TextContent "Output",
                TextContent "0",
                TextContent "false",
                TextContent "1",
                TextContent "true"],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "value"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [TextContent "Input value"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent "Boolean result"]]},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_doxygen_docs_be37db4c71dfaad8",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO
                (HsPrimType HsPrimCBool))))),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_be37db4c71dfaad8",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_html_example_ptr */ __attribute__ ((const)) _Bool (*hs_bindgen_test_doxygen_docs_be37db4c71dfaad8 (void)) (signed int arg1) { return &html_example; } ",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed)]
          (TypePrim PrimBool)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "html_example",
          commentLocation = Just
            "doxygen_docs.h:148:6",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Function with HTML formatting"],
            Paragraph
              [
                TextContent
                  "This function demonstrates",
                TextContent "HTML bold",
                TextContent "and",
                TextContent "italic",
                TextContent "text.",
                TextContent "It also shows",
                TextContent "HTML code",
                TextContent "formatting."],
            Paragraph
              [
                TextContent "Input",
                TextContent "Output",
                TextContent "0",
                TextContent "false",
                TextContent "1",
                TextContent "true"],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "value"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [TextContent "Input value"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent "Boolean result"]]},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "list_example",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "items"),
          functionParameterType = HsPtr
            (HsPtr
              (HsPrimType HsPrimCChar)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "items",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = [
                DefinitionList {
                  definitionListTerm = Bold
                    [
                      Monospace [TextContent "items"],
                      Emph [TextContent "(input)"]],
                  definitionListContent = [
                    Paragraph
                      [
                        TextContent
                          "Array of items"]]}]}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "count"),
          functionParameterType =
          HsExtBinding
            ExtHsRef {
              extHsRefModule = HsModuleName
                "HsBindgen.Runtime.Prelude",
              extHsRefIdentifier =
              HsIdentifier "CSize"}
            TypeSpec {
              typeSpecModule = Just
                (HsModuleName
                  "HsBindgen.Runtime.Prelude"),
              typeSpecIdentifier = Just
                (HsIdentifier "CSize"),
              typeSpecInstances = Map.fromList
                [
                  _×_
                    Eq
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
                    Enum
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
                    Bounded
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
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bits
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
                    Num
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
                    StaticSize
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
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]},
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "count",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = [
                DefinitionList {
                  definitionListTerm = Bold
                    [
                      Monospace [TextContent "count"],
                      Emph [TextContent "(input)"]],
                  definitionListContent = [
                    Paragraph
                      [
                        TextContent
                          "Number of items"]]}]}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCBool)),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_9d6d039971edcd60",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "_Bool hs_bindgen_test_doxygen_docs_9d6d039971edcd60 (char const **arg1, size_t arg2) { return list_example(arg1, arg2); }",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "items",
                  nameHsIdent = HsIdentifier
                    "items"})
              (TypePointer
                (TypePointer
                  (TypeConst
                    (TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "count",
                  nameHsIdent = HsIdentifier
                    "count"})
              (TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "size_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "CSize"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "CSize"),
                    typeSpecInstances = Map.fromList
                      [
                        _×_
                          Eq
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
                          Enum
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
                          Bounded
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
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bits
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
                          Num
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
                          StaticSize
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
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}})],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            PrimBool},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "list_example",
          commentLocation = Just
            "doxygen_docs.h:174:6",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Function with lists and special formatting"],
            Paragraph
              [
                TextContent
                  "This function demonstrates:"],
            ListItem {
              listItemType = BulletList,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Bullet point lists"]]},
            ListItem {
              listItemType = BulletList,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Nested list item 1"]]},
            ListItem {
              listItemType = BulletList,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Nested list item 2"]]},
            ListItem {
              listItemType = BulletList,
              listItemContent = [
                Paragraph
                  [
                    TextContent "Multiple items"]]},
            ListItem {
              listItemType = BulletList,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Nested formatting"]]},
            Paragraph
              [TextContent "Numbered list:"],
            ListItem {
              listItemType = NumberedList 1,
              listItemContent = [
                Paragraph
                  [
                    TextContent "First",
                    Monospace
                      [TextContent "item"]]]},
            ListItem {
              listItemType = NumberedList 1,
              listItemContent = [
                Paragraph
                  [TextContent "item"]]},
            ListItem {
              listItemType = NumberedList 2,
              listItemContent = [
                Paragraph
                  [
                    TextContent "Second",
                    Bold [TextContent "item"]]]},
            ListItem {
              listItemType = NumberedList 3,
              listItemContent = [
                Paragraph
                  [TextContent "Third item"]]},
            Paragraph
              [
                TextContent
                  "Other numbered list:"],
            ListItem {
              listItemType = NumberedList 1,
              listItemContent = [
                Paragraph [TextContent "A"]]},
            ListItem {
              listItemType = NumberedList 2,
              listItemContent = [
                Paragraph [TextContent "B"]]},
            ListItem {
              listItemType = NumberedList 3,
              listItemContent = [
                Paragraph [TextContent "C"]]},
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "items"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent "Array of items"]]},
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "count"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Number of items"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent "Success status"]]},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_doxygen_docs_c40a51053a97fb29",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr
                (HsPtr
                  (HsPrimType HsPrimCChar)))
              (HsFun
                (HsExtBinding
                  ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "CSize"}
                  TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "CSize"),
                    typeSpecInstances = Map.fromList
                      [
                        _×_
                          Eq
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
                          Enum
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
                          Bounded
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
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bits
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
                          Num
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
                          StaticSize
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
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]})
                (HsIO
                  (HsPrimType HsPrimCBool)))))),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_c40a51053a97fb29",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_list_example_ptr */ __attribute__ ((const)) _Bool (*hs_bindgen_test_doxygen_docs_c40a51053a97fb29 (void)) (char const **arg1, size_t arg2) { return &list_example; } ",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypePointer
                (TypeConst
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed)))))),
            TypeExtBinding
              ResolvedExtBinding {
                extCName = QualName {
                  qualNameName = Name "size_t",
                  qualNameKind =
                  NameKindOrdinary},
                extHsRef = ExtHsRef {
                  extHsRefModule = HsModuleName
                    "HsBindgen.Runtime.Prelude",
                  extHsRefIdentifier =
                  HsIdentifier "CSize"},
                extHsSpec = TypeSpec {
                  typeSpecModule = Just
                    (HsModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (HsIdentifier "CSize"),
                  typeSpecInstances = Map.fromList
                    [
                      _×_
                        Eq
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
                        Enum
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
                        Bounded
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
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bits
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
                        Num
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
                        StaticSize
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
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]}}]
          (TypePrim PrimBool)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "list_example",
          commentLocation = Just
            "doxygen_docs.h:174:6",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Function with lists and special formatting"],
            Paragraph
              [
                TextContent
                  "This function demonstrates:"],
            ListItem {
              listItemType = BulletList,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Bullet point lists"]]},
            ListItem {
              listItemType = BulletList,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Nested list item 1"]]},
            ListItem {
              listItemType = BulletList,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Nested list item 2"]]},
            ListItem {
              listItemType = BulletList,
              listItemContent = [
                Paragraph
                  [
                    TextContent "Multiple items"]]},
            ListItem {
              listItemType = BulletList,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Nested formatting"]]},
            Paragraph
              [TextContent "Numbered list:"],
            ListItem {
              listItemType = NumberedList 1,
              listItemContent = [
                Paragraph
                  [
                    TextContent "First",
                    Monospace
                      [TextContent "item"]]]},
            ListItem {
              listItemType = NumberedList 1,
              listItemContent = [
                Paragraph
                  [TextContent "item"]]},
            ListItem {
              listItemType = NumberedList 2,
              listItemContent = [
                Paragraph
                  [
                    TextContent "Second",
                    Bold [TextContent "item"]]]},
            ListItem {
              listItemType = NumberedList 3,
              listItemContent = [
                Paragraph
                  [TextContent "Third item"]]},
            Paragraph
              [
                TextContent
                  "Other numbered list:"],
            ListItem {
              listItemType = NumberedList 1,
              listItemContent = [
                Paragraph [TextContent "A"]]},
            ListItem {
              listItemType = NumberedList 2,
              listItemContent = [
                Paragraph [TextContent "B"]]},
            ListItem {
              listItemType = NumberedList 3,
              listItemContent = [
                Paragraph [TextContent "C"]]},
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "items"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent "Array of items"]]},
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "count"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Number of items"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent "Success status"]]},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "dangerous_function",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "ptr"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimVoid),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "ptr",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = [
                DefinitionList {
                  definitionListTerm = Bold
                    [
                      Monospace [TextContent "ptr"],
                      Emph [TextContent "(input)"]],
                  definitionListContent = [
                    Paragraph
                      [
                        TextContent
                          "Pointer to data"]]}]}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_b9c683fb9a695cc9",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_doxygen_docs_b9c683fb9a695cc9 (void *arg1) { return dangerous_function(arg1); }",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "ptr",
                  nameHsIdent = HsIdentifier
                    "ptr"})
              (TypePointer TypeVoid)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "dangerous_function",
          commentLocation = Just
            "doxygen_docs.h:186:7",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Function with warnings and notes"],
            Paragraph
              [
                Bold
                  [Emph [TextContent "WARNING:"]],
                TextContent
                  "This function may cause side effects"],
            Paragraph
              [
                Bold [TextContent "Note:"],
                TextContent
                  "Use with caution in multithreaded environments"],
            Paragraph
              [
                Bold [TextContent "see:"],
                TextContent
                  "related_function() for similar functionality"],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "ptr"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Pointer to data"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent
                  "Modified pointer"]]},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_doxygen_docs_291c4151bd4c3637",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr (HsPrimType HsPrimVoid))
              (HsIO
                (HsPtr
                  (HsPrimType HsPrimVoid)))))),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_291c4151bd4c3637",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_dangerous_function_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_doxygen_docs_291c4151bd4c3637 (void)) (void *arg1) { return &dangerous_function; } ",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Global
        (TypeFun
          [TypePointer TypeVoid]
          (TypePointer TypeVoid)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "dangerous_function",
          commentLocation = Just
            "doxygen_docs.h:186:7",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Function with warnings and notes"],
            Paragraph
              [
                Bold
                  [Emph [TextContent "WARNING:"]],
                TextContent
                  "This function may cause side effects"],
            Paragraph
              [
                Bold [TextContent "Note:"],
                TextContent
                  "Use with caution in multithreaded environments"],
            Paragraph
              [
                Bold [TextContent "see:"],
                TextContent
                  "related_function() for similar functionality"],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "ptr"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Pointer to data"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent
                  "Modified pointer"]]},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "detailed_return_codes",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "input"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCChar),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "input",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = [
                DefinitionList {
                  definitionListTerm = Bold
                    [
                      Monospace [TextContent "input"],
                      Emph [TextContent "(input)"]],
                  definitionListContent = [
                    Paragraph
                      [
                        TextContent
                          "Input string"]]}]}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_fb3e3158714e01f5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_doxygen_docs_fb3e3158714e01f5 (char const *arg1) { return detailed_return_codes(arg1); }",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "input",
                  nameHsIdent = HsIdentifier
                    "input"})
              (TypePointer
                (TypeConst
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "detailed_return_codes",
          commentLocation = Just
            "doxygen_docs.h:197:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Function with return value details"],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "input"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [TextContent "Input string"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent "0",
                TextContent "Success"],
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent "-1",
                TextContent "Invalid input"],
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent "-2",
                TextContent
                  "Memory allocation failed"],
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent "-3",
                TextContent
                  "Processing error"]]},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_doxygen_docs_a92bbc7b93300d3b",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr (HsPrimType HsPrimCChar))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_a92bbc7b93300d3b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_detailed_return_codes_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_doxygen_docs_a92bbc7b93300d3b (void)) (char const *arg1) { return &detailed_return_codes; } ",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeConst
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))))]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "detailed_return_codes",
          commentLocation = Just
            "doxygen_docs.h:197:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Function with return value details"],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "input"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [TextContent "Input string"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent "0",
                TextContent "Success"],
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent "-1",
                TextContent "Invalid input"],
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent "-2",
                TextContent
                  "Memory allocation failed"],
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent "-3",
                TextContent
                  "Processing error"]]},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "old_function",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "old_param"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "old_param",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = [
                DefinitionList {
                  definitionListTerm = Bold
                    [
                      Monospace
                        [TextContent "old_param"],
                      Emph [TextContent "(input)"]],
                  definitionListContent = [
                    Paragraph
                      [
                        TextContent
                          "Legacy parameter"]]}]}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_84140a44d1bd8380",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_doxygen_docs_84140a44d1bd8380 (signed int arg1) { return old_function(arg1); }",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "old_param",
                  nameHsIdent = HsIdentifier
                    "old_param"})
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
          commentOrigin = Just
            "old_function",
          commentLocation = Just
            "doxygen_docs.h:206:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Function with deprecated annotation"],
            Paragraph
              [
                Bold
                  [TextContent "deprecated:"],
                TextContent
                  "Use new_function() instead"],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace
                    [TextContent "old_param"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Legacy parameter"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent "Legacy result"]]},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_doxygen_docs_985c4be1f80d7557",
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
      "hs_bindgen_test_doxygen_docs_985c4be1f80d7557",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_old_function_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_doxygen_docs_985c4be1f80d7557 (void)) (signed int arg1) { return &old_function; } ",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed)]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "old_function",
          commentLocation = Just
            "doxygen_docs.h:206:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Function with deprecated annotation"],
            Paragraph
              [
                Bold
                  [TextContent "deprecated:"],
                TextContent
                  "Use new_function() instead"],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace
                    [TextContent "old_param"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Legacy parameter"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent "Legacy result"]]},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "versioned_function",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "data'"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "data'",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_7549924ba8d1e7b8",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_doxygen_docs_7549924ba8d1e7b8 (signed int arg1) { return versioned_function(arg1); }",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "data",
                  nameHsIdent = HsIdentifier
                    "data'"})
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
          commentOrigin = Just
            "versioned_function",
          commentLocation = Just
            "doxygen_docs.h:216:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Function with version information"],
            Paragraph
              [Metadata (Since " 1.0")],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "data"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [TextContent "Input data"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent "Processed data"]]},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_doxygen_docs_6b616614abe7d3f0",
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
      "hs_bindgen_test_doxygen_docs_6b616614abe7d3f0",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_versioned_function_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_doxygen_docs_6b616614abe7d3f0 (void)) (signed int arg1) { return &versioned_function; } ",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed)]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "versioned_function",
          commentLocation = Just
            "doxygen_docs.h:216:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Function with version information"],
            Paragraph
              [Metadata (Since " 1.0")],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "data"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [TextContent "Input data"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent "Processed data"]]},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Event_callback_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Event_callback_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Event_callback_t",
        fieldType = HsFunPtr
          (HsFun
            (HsPrimType HsPrimCInt)
            (HsFun
              (HsPtr (HsPrimType HsPrimVoid))
              (HsIO
                (HsPrimType HsPrimCInt)))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "doxygen_docs.h:225:15",
          declId = NamePair {
            nameC = Name "event_callback_t",
            nameHsIdent = HsIdentifier
              "Event_callback_t"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "doxygen_docs.h",
          declComment = Just
            (Comment
              [
                Paragraph [TextContent ""],
                BlockCommand {
                  blockCommandName = "brief",
                  blockCommandArgs = [],
                  blockCommandParagraph = [
                    TextContent
                      "Callback function type"]},
                Paragraph [TextContent ""],
                ParamCommand {
                  paramCommandName = "event_type",
                  paramCommandIndex = Just 0,
                  paramCommandDirection = Just
                    CXCommentParamPassDirection_In,
                  paramCommandIsDirectionExplicit =
                  False,
                  paramCommandContent = [
                    Paragraph
                      [
                        TextContent "Type of event",
                        TextContent ""]]},
                ParamCommand {
                  paramCommandName = "user_data",
                  paramCommandIndex = Just 1,
                  paramCommandDirection = Just
                    CXCommentParamPassDirection_In,
                  paramCommandIsDirectionExplicit =
                  False,
                  paramCommandContent = [
                    Paragraph
                      [
                        TextContent
                          "User-provided data",
                        TextContent ""]]},
                BlockCommand {
                  blockCommandName = "return",
                  blockCommandArgs = [],
                  blockCommandParagraph = [
                    TextContent
                      "Handling result"]}])},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Event_callback_t",
              newtypeField = HsName
                "@NsVar"
                "un_Event_callback_t"},
            typedefType = TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed),
                  TypePointer TypeVoid]
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "event_callback_t",
          commentLocation = Just
            "doxygen_docs.h:225:15",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Callback function type"],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace
                    [TextContent "event_type"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [TextContent "Type of event"]]},
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace
                    [TextContent "user_data"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "User-provided data"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent
                  "Handling result"]]}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Event_callback_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Event_callback_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Event_callback_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Event_callback_t",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Config_t",
      structConstr = HsName
        "@NsConstr"
        "Config_t",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "config_t_id",
          fieldType = HsExtBinding
            ExtHsRef {
              extHsRefModule = HsModuleName
                "HsBindgen.Runtime.Prelude",
              extHsRefIdentifier =
              HsIdentifier "Word32"}
            TypeSpec {
              typeSpecModule = Just
                (HsModuleName
                  "HsBindgen.Runtime.Prelude"),
              typeSpecIdentifier = Just
                (HsIdentifier "Word32"),
              typeSpecInstances = Map.fromList
                [
                  _×_
                    Eq
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
                    Enum
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
                    Bounded
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
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bits
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
                    Num
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
                    StaticSize
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
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]},
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "doxygen_docs.h:234:14",
                fieldName = NamePair {
                  nameC = Name "id",
                  nameHsIdent = HsIdentifier
                    "config_t_id"},
                fieldComment = Just
                  (Comment
                    [
                      Paragraph [TextContent ""],
                      BlockCommand {
                        blockCommandName = "brief",
                        blockCommandArgs = [],
                        blockCommandParagraph = [
                          TextContent
                            "Unique identifier"]}])},
              structFieldType = TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "uint32_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "Word32"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "Word32"),
                    typeSpecInstances = Map.fromList
                      [
                        _×_
                          Eq
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
                          Enum
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
                          Bounded
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
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bits
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
                          Num
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
                          StaticSize
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
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}},
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "id",
              commentLocation = Just
                "doxygen_docs.h:234:14",
              commentHeader = Just
                "doxygen_docs.h",
              commentChildren = [
                Paragraph
                  [
                    TextContent
                      "Unique identifier"]]}},
        Field {
          fieldName = HsName
            "@NsVar"
            "config_t_name",
          fieldType = HsConstArray
            64
            (HsPrimType HsPrimCChar),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "doxygen_docs.h:237:10",
                fieldName = NamePair {
                  nameC = Name "name",
                  nameHsIdent = HsIdentifier
                    "config_t_name"},
                fieldComment = Just
                  (Comment
                    [
                      Paragraph [TextContent ""],
                      BlockCommand {
                        blockCommandName = "brief",
                        blockCommandArgs = [],
                        blockCommandParagraph = [
                          TextContent
                            "Human-readable name"]}])},
              structFieldType = TypeConstArray
                64
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "name",
              commentLocation = Just
                "doxygen_docs.h:237:10",
              commentHeader = Just
                "doxygen_docs.h",
              commentChildren = [
                Paragraph
                  [
                    TextContent
                      "Human-readable name"]]}},
        Field {
          fieldName = HsName
            "@NsVar"
            "config_t_flags",
          fieldType = HsExtBinding
            ExtHsRef {
              extHsRefModule = HsModuleName
                "HsBindgen.Runtime.Prelude",
              extHsRefIdentifier =
              HsIdentifier "Word32"}
            TypeSpec {
              typeSpecModule = Just
                (HsModuleName
                  "HsBindgen.Runtime.Prelude"),
              typeSpecIdentifier = Just
                (HsIdentifier "Word32"),
              typeSpecInstances = Map.fromList
                [
                  _×_
                    Eq
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
                    Enum
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
                    Bounded
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
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bits
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
                    Num
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
                    StaticSize
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
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]},
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "doxygen_docs.h:240:14",
                fieldName = NamePair {
                  nameC = Name "flags",
                  nameHsIdent = HsIdentifier
                    "config_t_flags"},
                fieldComment = Just
                  (Comment
                    [
                      Paragraph [TextContent ""],
                      BlockCommand {
                        blockCommandName = "brief",
                        blockCommandArgs = [],
                        blockCommandParagraph = [
                          TextContent
                            "Configuration flags"]}])},
              structFieldType = TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "uint32_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "Word32"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "Word32"),
                    typeSpecInstances = Map.fromList
                      [
                        _×_
                          Eq
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
                          Enum
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
                          Bounded
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
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bits
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
                          Num
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
                          StaticSize
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
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}},
              structFieldOffset = 544,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "flags",
              commentLocation = Just
                "doxygen_docs.h:240:14",
              commentHeader = Just
                "doxygen_docs.h",
              commentChildren = [
                Paragraph
                  [
                    TextContent
                      "Configuration flags"]]}},
        Field {
          fieldName = HsName
            "@NsVar"
            "config_t_callback",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Event_callback_t"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "doxygen_docs.h:247:22",
                fieldName = NamePair {
                  nameC = Name "callback",
                  nameHsIdent = HsIdentifier
                    "config_t_callback"},
                fieldComment = Just
                  (Comment
                    [
                      Paragraph [TextContent ""],
                      BlockCommand {
                        blockCommandName = "brief",
                        blockCommandArgs = [],
                        blockCommandParagraph = [
                          TextContent
                            "Optional callback function"]},
                      Paragraph
                        [
                          TextContent "See also:",
                          InlineRefCommand
                            (ById
                              NamePair {
                                nameC = Name "event_callback_t",
                                nameHsIdent = HsIdentifier
                                  "Event_callback_t"})],
                      Paragraph [TextContent ""]])},
              structFieldType = TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "event_callback_t",
                    nameHsIdent = HsIdentifier
                      "Event_callback_t"}),
              structFieldOffset = 576,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "callback",
              commentLocation = Just
                "doxygen_docs.h:247:22",
              commentHeader = Just
                "doxygen_docs.h",
              commentChildren = [
                Paragraph
                  [
                    TextContent
                      "Optional callback function"],
                Paragraph
                  [
                    TextContent "See also:",
                    Identifier
                      "Event_callback_t"]]}},
        Field {
          fieldName = HsName
            "@NsVar"
            "config_t_user_data",
          fieldType = HsPtr
            (HsPrimType HsPrimVoid),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "doxygen_docs.h:250:11",
                fieldName = NamePair {
                  nameC = Name "user_data",
                  nameHsIdent = HsIdentifier
                    "config_t_user_data"},
                fieldComment = Just
                  (Comment
                    [
                      Paragraph [TextContent ""],
                      BlockCommand {
                        blockCommandName = "brief",
                        blockCommandArgs = [],
                        blockCommandParagraph = [
                          TextContent
                            "User data for callback"]}])},
              structFieldType = TypePointer
                TypeVoid,
              structFieldOffset = 640,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "user_data",
              commentLocation = Just
                "doxygen_docs.h:250:11",
              commentHeader = Just
                "doxygen_docs.h",
              commentChildren = [
                Paragraph
                  [
                    TextContent
                      "User data for callback"]]}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "doxygen_docs.h:232:9",
            declId = NamePair {
              nameC = Name "config_t",
              nameHsIdent = HsIdentifier
                "Config_t"},
            declOrigin = NameOriginGenerated
              (AnonId "doxygen_docs.h:232:9"),
            declAliases = [Name "config_t"],
            declHeader = "doxygen_docs.h",
            declComment = Just
              (Comment
                [
                  Paragraph [TextContent ""],
                  BlockCommand {
                    blockCommandName = "brief",
                    blockCommandArgs = [],
                    blockCommandParagraph = [
                      TextContent
                        "Structure with documented fields"]},
                  Paragraph
                    [
                      TextContent
                        "This structure demonstrates field documentation."]])},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Config_t"),
              structSizeof = 88,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:234:14",
                    fieldName = NamePair {
                      nameC = Name "id",
                      nameHsIdent = HsIdentifier
                        "config_t_id"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent
                                "Unique identifier"]}])},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint32_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word32"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word32"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
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
                              Enum
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
                              Bounded
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
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
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
                              Num
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
                              StaticSize
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
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:237:10",
                    fieldName = NamePair {
                      nameC = Name "name",
                      nameHsIdent = HsIdentifier
                        "config_t_name"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent
                                "Human-readable name"]}])},
                  structFieldType = TypeConstArray
                    64
                    (TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed)))),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:240:14",
                    fieldName = NamePair {
                      nameC = Name "flags",
                      nameHsIdent = HsIdentifier
                        "config_t_flags"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent
                                "Configuration flags"]}])},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint32_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word32"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word32"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
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
                              Enum
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
                              Bounded
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
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
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
                              Num
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
                              StaticSize
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
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 544,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:247:22",
                    fieldName = NamePair {
                      nameC = Name "callback",
                      nameHsIdent = HsIdentifier
                        "config_t_callback"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent
                                "Optional callback function"]},
                          Paragraph
                            [
                              TextContent "See also:",
                              InlineRefCommand
                                (ById
                                  NamePair {
                                    nameC = Name "event_callback_t",
                                    nameHsIdent = HsIdentifier
                                      "Event_callback_t"})],
                          Paragraph [TextContent ""]])},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "event_callback_t",
                        nameHsIdent = HsIdentifier
                          "Event_callback_t"}),
                  structFieldOffset = 576,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:250:11",
                    fieldName = NamePair {
                      nameC = Name "user_data",
                      nameHsIdent = HsIdentifier
                        "config_t_user_data"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent
                                "User data for callback"]}])},
                  structFieldType = TypePointer
                    TypeVoid,
                  structFieldOffset = 640,
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
          commentOrigin = Just "config_t",
          commentLocation = Just
            "doxygen_docs.h:232:9",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Structure with documented fields"],
            Paragraph
              [
                TextContent
                  "This structure demonstrates field documentation."]]}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Config_t",
          structConstr = HsName
            "@NsConstr"
            "Config_t",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "config_t_id",
              fieldType = HsExtBinding
                ExtHsRef {
                  extHsRefModule = HsModuleName
                    "HsBindgen.Runtime.Prelude",
                  extHsRefIdentifier =
                  HsIdentifier "Word32"}
                TypeSpec {
                  typeSpecModule = Just
                    (HsModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (HsIdentifier "Word32"),
                  typeSpecInstances = Map.fromList
                    [
                      _×_
                        Eq
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
                        Enum
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
                        Bounded
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
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bits
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
                        Num
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
                        StaticSize
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
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]},
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:234:14",
                    fieldName = NamePair {
                      nameC = Name "id",
                      nameHsIdent = HsIdentifier
                        "config_t_id"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent
                                "Unique identifier"]}])},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint32_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word32"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word32"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
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
                              Enum
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
                              Bounded
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
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
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
                              Num
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
                              StaticSize
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
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "id",
                  commentLocation = Just
                    "doxygen_docs.h:234:14",
                  commentHeader = Just
                    "doxygen_docs.h",
                  commentChildren = [
                    Paragraph
                      [
                        TextContent
                          "Unique identifier"]]}},
            Field {
              fieldName = HsName
                "@NsVar"
                "config_t_name",
              fieldType = HsConstArray
                64
                (HsPrimType HsPrimCChar),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:237:10",
                    fieldName = NamePair {
                      nameC = Name "name",
                      nameHsIdent = HsIdentifier
                        "config_t_name"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent
                                "Human-readable name"]}])},
                  structFieldType = TypeConstArray
                    64
                    (TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed)))),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "name",
                  commentLocation = Just
                    "doxygen_docs.h:237:10",
                  commentHeader = Just
                    "doxygen_docs.h",
                  commentChildren = [
                    Paragraph
                      [
                        TextContent
                          "Human-readable name"]]}},
            Field {
              fieldName = HsName
                "@NsVar"
                "config_t_flags",
              fieldType = HsExtBinding
                ExtHsRef {
                  extHsRefModule = HsModuleName
                    "HsBindgen.Runtime.Prelude",
                  extHsRefIdentifier =
                  HsIdentifier "Word32"}
                TypeSpec {
                  typeSpecModule = Just
                    (HsModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (HsIdentifier "Word32"),
                  typeSpecInstances = Map.fromList
                    [
                      _×_
                        Eq
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
                        Enum
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
                        Bounded
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
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bits
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
                        Num
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
                        StaticSize
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
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]},
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:240:14",
                    fieldName = NamePair {
                      nameC = Name "flags",
                      nameHsIdent = HsIdentifier
                        "config_t_flags"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent
                                "Configuration flags"]}])},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint32_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word32"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word32"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
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
                              Enum
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
                              Bounded
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
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
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
                              Num
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
                              StaticSize
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
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 544,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "flags",
                  commentLocation = Just
                    "doxygen_docs.h:240:14",
                  commentHeader = Just
                    "doxygen_docs.h",
                  commentChildren = [
                    Paragraph
                      [
                        TextContent
                          "Configuration flags"]]}},
            Field {
              fieldName = HsName
                "@NsVar"
                "config_t_callback",
              fieldType = HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Event_callback_t"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:247:22",
                    fieldName = NamePair {
                      nameC = Name "callback",
                      nameHsIdent = HsIdentifier
                        "config_t_callback"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent
                                "Optional callback function"]},
                          Paragraph
                            [
                              TextContent "See also:",
                              InlineRefCommand
                                (ById
                                  NamePair {
                                    nameC = Name "event_callback_t",
                                    nameHsIdent = HsIdentifier
                                      "Event_callback_t"})],
                          Paragraph [TextContent ""]])},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "event_callback_t",
                        nameHsIdent = HsIdentifier
                          "Event_callback_t"}),
                  structFieldOffset = 576,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "callback",
                  commentLocation = Just
                    "doxygen_docs.h:247:22",
                  commentHeader = Just
                    "doxygen_docs.h",
                  commentChildren = [
                    Paragraph
                      [
                        TextContent
                          "Optional callback function"],
                    Paragraph
                      [
                        TextContent "See also:",
                        Identifier
                          "Event_callback_t"]]}},
            Field {
              fieldName = HsName
                "@NsVar"
                "config_t_user_data",
              fieldType = HsPtr
                (HsPrimType HsPrimVoid),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:250:11",
                    fieldName = NamePair {
                      nameC = Name "user_data",
                      nameHsIdent = HsIdentifier
                        "config_t_user_data"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent
                                "User data for callback"]}])},
                  structFieldType = TypePointer
                    TypeVoid,
                  structFieldOffset = 640,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just
                    "user_data",
                  commentLocation = Just
                    "doxygen_docs.h:250:11",
                  commentHeader = Just
                    "doxygen_docs.h",
                  commentChildren = [
                    Paragraph
                      [
                        TextContent
                          "User data for callback"]]}}],
          structOrigin =
          Just
            Decl {
              declInfo =
              DeclInfo {
                declLoc =
                "doxygen_docs.h:232:9",
                declId = NamePair {
                  nameC = Name "config_t",
                  nameHsIdent = HsIdentifier
                    "Config_t"},
                declOrigin = NameOriginGenerated
                  (AnonId "doxygen_docs.h:232:9"),
                declAliases = [Name "config_t"],
                declHeader = "doxygen_docs.h",
                declComment =
                Just
                  (Comment
                    [
                      Paragraph [TextContent ""],
                      BlockCommand {
                        blockCommandName = "brief",
                        blockCommandArgs = [],
                        blockCommandParagraph = [
                          TextContent
                            "Structure with documented fields"]},
                      Paragraph
                        [
                          TextContent
                            "This structure demonstrates field documentation."]])},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Config_t"),
                  structSizeof = 88,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "doxygen_docs.h:234:14",
                        fieldName = NamePair {
                          nameC = Name "id",
                          nameHsIdent = HsIdentifier
                            "config_t_id"},
                        fieldComment = Just
                          (Comment
                            [
                              Paragraph [TextContent ""],
                              BlockCommand {
                                blockCommandName = "brief",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent
                                    "Unique identifier"]}])},
                      structFieldType = TypeExtBinding
                        ResolvedExtBinding {
                          extCName = QualName {
                            qualNameName = Name "uint32_t",
                            qualNameKind =
                            NameKindOrdinary},
                          extHsRef = ExtHsRef {
                            extHsRefModule = HsModuleName
                              "HsBindgen.Runtime.Prelude",
                            extHsRefIdentifier =
                            HsIdentifier "Word32"},
                          extHsSpec = TypeSpec {
                            typeSpecModule = Just
                              (HsModuleName
                                "HsBindgen.Runtime.Prelude"),
                            typeSpecIdentifier = Just
                              (HsIdentifier "Word32"),
                            typeSpecInstances = Map.fromList
                              [
                                _×_
                                  Eq
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
                                  Enum
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
                                  Bounded
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
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bits
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
                                  Num
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
                                  StaticSize
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
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]}},
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "doxygen_docs.h:237:10",
                        fieldName = NamePair {
                          nameC = Name "name",
                          nameHsIdent = HsIdentifier
                            "config_t_name"},
                        fieldComment = Just
                          (Comment
                            [
                              Paragraph [TextContent ""],
                              BlockCommand {
                                blockCommandName = "brief",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent
                                    "Human-readable name"]}])},
                      structFieldType = TypeConstArray
                        64
                        (TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed)))),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "doxygen_docs.h:240:14",
                        fieldName = NamePair {
                          nameC = Name "flags",
                          nameHsIdent = HsIdentifier
                            "config_t_flags"},
                        fieldComment = Just
                          (Comment
                            [
                              Paragraph [TextContent ""],
                              BlockCommand {
                                blockCommandName = "brief",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent
                                    "Configuration flags"]}])},
                      structFieldType = TypeExtBinding
                        ResolvedExtBinding {
                          extCName = QualName {
                            qualNameName = Name "uint32_t",
                            qualNameKind =
                            NameKindOrdinary},
                          extHsRef = ExtHsRef {
                            extHsRefModule = HsModuleName
                              "HsBindgen.Runtime.Prelude",
                            extHsRefIdentifier =
                            HsIdentifier "Word32"},
                          extHsSpec = TypeSpec {
                            typeSpecModule = Just
                              (HsModuleName
                                "HsBindgen.Runtime.Prelude"),
                            typeSpecIdentifier = Just
                              (HsIdentifier "Word32"),
                            typeSpecInstances = Map.fromList
                              [
                                _×_
                                  Eq
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
                                  Enum
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
                                  Bounded
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
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bits
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
                                  Num
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
                                  StaticSize
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
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]}},
                      structFieldOffset = 544,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "doxygen_docs.h:247:22",
                        fieldName = NamePair {
                          nameC = Name "callback",
                          nameHsIdent = HsIdentifier
                            "config_t_callback"},
                        fieldComment = Just
                          (Comment
                            [
                              Paragraph [TextContent ""],
                              BlockCommand {
                                blockCommandName = "brief",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent
                                    "Optional callback function"]},
                              Paragraph
                                [
                                  TextContent "See also:",
                                  InlineRefCommand
                                    (ById
                                      NamePair {
                                        nameC = Name "event_callback_t",
                                        nameHsIdent = HsIdentifier
                                          "Event_callback_t"})],
                              Paragraph [TextContent ""]])},
                      structFieldType = TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "event_callback_t",
                            nameHsIdent = HsIdentifier
                              "Event_callback_t"}),
                      structFieldOffset = 576,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "doxygen_docs.h:250:11",
                        fieldName = NamePair {
                          nameC = Name "user_data",
                          nameHsIdent = HsIdentifier
                            "config_t_user_data"},
                        fieldComment = Just
                          (Comment
                            [
                              Paragraph [TextContent ""],
                              BlockCommand {
                                blockCommandName = "brief",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent
                                    "User data for callback"]}])},
                      structFieldType = TypePointer
                        TypeVoid,
                      structFieldOffset = 640,
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
              commentOrigin = Just "config_t",
              commentLocation = Just
                "doxygen_docs.h:232:9",
              commentHeader = Just
                "doxygen_docs.h",
              commentChildren = [
                Paragraph
                  [
                    TextContent
                      "Structure with documented fields"],
                Paragraph
                  [
                    TextContent
                      "This structure demonstrates field documentation."]]}}
        StorableInstance {
          storableSizeOf = 88,
          storableAlignment = 8,
          storablePeek =
          Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Config_t",
                  structConstr = HsName
                    "@NsConstr"
                    "Config_t",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "config_t_id",
                      fieldType = HsExtBinding
                        ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "Word32"}
                        TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "Word32"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
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
                                Enum
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
                                Bounded
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
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
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
                                Num
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
                                StaticSize
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
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:234:14",
                            fieldName = NamePair {
                              nameC = Name "id",
                              nameHsIdent = HsIdentifier
                                "config_t_id"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent
                                        "Unique identifier"]}])},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint32_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtHsRef {
                                extHsRefModule = HsModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extHsRefIdentifier =
                                HsIdentifier "Word32"},
                              extHsSpec = TypeSpec {
                                typeSpecModule = Just
                                  (HsModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                typeSpecIdentifier = Just
                                  (HsIdentifier "Word32"),
                                typeSpecInstances = Map.fromList
                                  [
                                    _×_
                                      Eq
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
                                      Enum
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
                                      Bounded
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
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bits
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
                                      Num
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
                                      StaticSize
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
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}},
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "id",
                          commentLocation = Just
                            "doxygen_docs.h:234:14",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [
                                TextContent
                                  "Unique identifier"]]}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "config_t_name",
                      fieldType = HsConstArray
                        64
                        (HsPrimType HsPrimCChar),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:237:10",
                            fieldName = NamePair {
                              nameC = Name "name",
                              nameHsIdent = HsIdentifier
                                "config_t_name"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent
                                        "Human-readable name"]}])},
                          structFieldType = TypeConstArray
                            64
                            (TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed)))),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "name",
                          commentLocation = Just
                            "doxygen_docs.h:237:10",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [
                                TextContent
                                  "Human-readable name"]]}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "config_t_flags",
                      fieldType = HsExtBinding
                        ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "Word32"}
                        TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "Word32"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
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
                                Enum
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
                                Bounded
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
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
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
                                Num
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
                                StaticSize
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
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:240:14",
                            fieldName = NamePair {
                              nameC = Name "flags",
                              nameHsIdent = HsIdentifier
                                "config_t_flags"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent
                                        "Configuration flags"]}])},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint32_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtHsRef {
                                extHsRefModule = HsModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extHsRefIdentifier =
                                HsIdentifier "Word32"},
                              extHsSpec = TypeSpec {
                                typeSpecModule = Just
                                  (HsModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                typeSpecIdentifier = Just
                                  (HsIdentifier "Word32"),
                                typeSpecInstances = Map.fromList
                                  [
                                    _×_
                                      Eq
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
                                      Enum
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
                                      Bounded
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
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bits
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
                                      Num
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
                                      StaticSize
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
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}},
                          structFieldOffset = 544,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "flags",
                          commentLocation = Just
                            "doxygen_docs.h:240:14",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [
                                TextContent
                                  "Configuration flags"]]}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "config_t_callback",
                      fieldType = HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Event_callback_t"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:247:22",
                            fieldName = NamePair {
                              nameC = Name "callback",
                              nameHsIdent = HsIdentifier
                                "config_t_callback"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent
                                        "Optional callback function"]},
                                  Paragraph
                                    [
                                      TextContent "See also:",
                                      InlineRefCommand
                                        (ById
                                          NamePair {
                                            nameC = Name "event_callback_t",
                                            nameHsIdent = HsIdentifier
                                              "Event_callback_t"})],
                                  Paragraph [TextContent ""]])},
                          structFieldType = TypeTypedef
                            (TypedefRegular
                              NamePair {
                                nameC = Name "event_callback_t",
                                nameHsIdent = HsIdentifier
                                  "Event_callback_t"}),
                          structFieldOffset = 576,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "callback",
                          commentLocation = Just
                            "doxygen_docs.h:247:22",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [
                                TextContent
                                  "Optional callback function"],
                            Paragraph
                              [
                                TextContent "See also:",
                                Identifier
                                  "Event_callback_t"]]}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "config_t_user_data",
                      fieldType = HsPtr
                        (HsPrimType HsPrimVoid),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:250:11",
                            fieldName = NamePair {
                              nameC = Name "user_data",
                              nameHsIdent = HsIdentifier
                                "config_t_user_data"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent
                                        "User data for callback"]}])},
                          structFieldType = TypePointer
                            TypeVoid,
                          structFieldOffset = 640,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "user_data",
                          commentLocation = Just
                            "doxygen_docs.h:250:11",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [
                                TextContent
                                  "User data for callback"]]}}],
                  structOrigin =
                  Just
                    Decl {
                      declInfo =
                      DeclInfo {
                        declLoc =
                        "doxygen_docs.h:232:9",
                        declId = NamePair {
                          nameC = Name "config_t",
                          nameHsIdent = HsIdentifier
                            "Config_t"},
                        declOrigin = NameOriginGenerated
                          (AnonId "doxygen_docs.h:232:9"),
                        declAliases = [Name "config_t"],
                        declHeader = "doxygen_docs.h",
                        declComment =
                        Just
                          (Comment
                            [
                              Paragraph [TextContent ""],
                              BlockCommand {
                                blockCommandName = "brief",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent
                                    "Structure with documented fields"]},
                              Paragraph
                                [
                                  TextContent
                                    "This structure demonstrates field documentation."]])},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Config_t"),
                          structSizeof = 88,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:234:14",
                                fieldName = NamePair {
                                  nameC = Name "id",
                                  nameHsIdent = HsIdentifier
                                    "config_t_id"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent
                                            "Unique identifier"]}])},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint32_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtHsRef {
                                    extHsRefModule = HsModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extHsRefIdentifier =
                                    HsIdentifier "Word32"},
                                  extHsSpec = TypeSpec {
                                    typeSpecModule = Just
                                      (HsModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    typeSpecIdentifier = Just
                                      (HsIdentifier "Word32"),
                                    typeSpecInstances = Map.fromList
                                      [
                                        _×_
                                          Eq
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
                                          Enum
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
                                          Bounded
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
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bits
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
                                          Num
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
                                          StaticSize
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
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:237:10",
                                fieldName = NamePair {
                                  nameC = Name "name",
                                  nameHsIdent = HsIdentifier
                                    "config_t_name"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent
                                            "Human-readable name"]}])},
                              structFieldType = TypeConstArray
                                64
                                (TypePrim
                                  (PrimChar
                                    (PrimSignImplicit
                                      (Just Signed)))),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:240:14",
                                fieldName = NamePair {
                                  nameC = Name "flags",
                                  nameHsIdent = HsIdentifier
                                    "config_t_flags"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent
                                            "Configuration flags"]}])},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint32_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtHsRef {
                                    extHsRefModule = HsModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extHsRefIdentifier =
                                    HsIdentifier "Word32"},
                                  extHsSpec = TypeSpec {
                                    typeSpecModule = Just
                                      (HsModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    typeSpecIdentifier = Just
                                      (HsIdentifier "Word32"),
                                    typeSpecInstances = Map.fromList
                                      [
                                        _×_
                                          Eq
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
                                          Enum
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
                                          Bounded
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
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bits
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
                                          Num
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
                                          StaticSize
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
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 544,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:247:22",
                                fieldName = NamePair {
                                  nameC = Name "callback",
                                  nameHsIdent = HsIdentifier
                                    "config_t_callback"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent
                                            "Optional callback function"]},
                                      Paragraph
                                        [
                                          TextContent "See also:",
                                          InlineRefCommand
                                            (ById
                                              NamePair {
                                                nameC = Name "event_callback_t",
                                                nameHsIdent = HsIdentifier
                                                  "Event_callback_t"})],
                                      Paragraph [TextContent ""]])},
                              structFieldType = TypeTypedef
                                (TypedefRegular
                                  NamePair {
                                    nameC = Name "event_callback_t",
                                    nameHsIdent = HsIdentifier
                                      "Event_callback_t"}),
                              structFieldOffset = 576,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:250:11",
                                fieldName = NamePair {
                                  nameC = Name "user_data",
                                  nameHsIdent = HsIdentifier
                                    "config_t_user_data"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent
                                            "User data for callback"]}])},
                              structFieldType = TypePointer
                                TypeVoid,
                              structFieldOffset = 640,
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
                  structComment =
                  Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "config_t",
                      commentLocation = Just
                        "doxygen_docs.h:232:9",
                      commentHeader = Just
                        "doxygen_docs.h",
                      commentChildren =
                      [
                        Paragraph
                          [
                            TextContent
                              "Structure with documented fields"],
                        Paragraph
                          [
                            TextContent
                              "This structure demonstrates field documentation."]]}})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 4,
                PeekByteOff (Idx 0) 68,
                PeekByteOff (Idx 0) 72,
                PeekByteOff (Idx 0) 80]),
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
                    "Config_t",
                  structConstr = HsName
                    "@NsConstr"
                    "Config_t",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "config_t_id",
                      fieldType = HsExtBinding
                        ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "Word32"}
                        TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "Word32"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
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
                                Enum
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
                                Bounded
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
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
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
                                Num
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
                                StaticSize
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
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:234:14",
                            fieldName = NamePair {
                              nameC = Name "id",
                              nameHsIdent = HsIdentifier
                                "config_t_id"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent
                                        "Unique identifier"]}])},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint32_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtHsRef {
                                extHsRefModule = HsModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extHsRefIdentifier =
                                HsIdentifier "Word32"},
                              extHsSpec = TypeSpec {
                                typeSpecModule = Just
                                  (HsModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                typeSpecIdentifier = Just
                                  (HsIdentifier "Word32"),
                                typeSpecInstances = Map.fromList
                                  [
                                    _×_
                                      Eq
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
                                      Enum
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
                                      Bounded
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
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bits
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
                                      Num
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
                                      StaticSize
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
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}},
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "id",
                          commentLocation = Just
                            "doxygen_docs.h:234:14",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [
                                TextContent
                                  "Unique identifier"]]}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "config_t_name",
                      fieldType = HsConstArray
                        64
                        (HsPrimType HsPrimCChar),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:237:10",
                            fieldName = NamePair {
                              nameC = Name "name",
                              nameHsIdent = HsIdentifier
                                "config_t_name"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent
                                        "Human-readable name"]}])},
                          structFieldType = TypeConstArray
                            64
                            (TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed)))),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "name",
                          commentLocation = Just
                            "doxygen_docs.h:237:10",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [
                                TextContent
                                  "Human-readable name"]]}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "config_t_flags",
                      fieldType = HsExtBinding
                        ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "Word32"}
                        TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "Word32"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
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
                                Enum
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
                                Bounded
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
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
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
                                Num
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
                                StaticSize
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
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:240:14",
                            fieldName = NamePair {
                              nameC = Name "flags",
                              nameHsIdent = HsIdentifier
                                "config_t_flags"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent
                                        "Configuration flags"]}])},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint32_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtHsRef {
                                extHsRefModule = HsModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extHsRefIdentifier =
                                HsIdentifier "Word32"},
                              extHsSpec = TypeSpec {
                                typeSpecModule = Just
                                  (HsModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                typeSpecIdentifier = Just
                                  (HsIdentifier "Word32"),
                                typeSpecInstances = Map.fromList
                                  [
                                    _×_
                                      Eq
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
                                      Enum
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
                                      Bounded
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
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bits
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
                                      Num
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
                                      StaticSize
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
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}},
                          structFieldOffset = 544,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "flags",
                          commentLocation = Just
                            "doxygen_docs.h:240:14",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [
                                TextContent
                                  "Configuration flags"]]}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "config_t_callback",
                      fieldType = HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Event_callback_t"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:247:22",
                            fieldName = NamePair {
                              nameC = Name "callback",
                              nameHsIdent = HsIdentifier
                                "config_t_callback"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent
                                        "Optional callback function"]},
                                  Paragraph
                                    [
                                      TextContent "See also:",
                                      InlineRefCommand
                                        (ById
                                          NamePair {
                                            nameC = Name "event_callback_t",
                                            nameHsIdent = HsIdentifier
                                              "Event_callback_t"})],
                                  Paragraph [TextContent ""]])},
                          structFieldType = TypeTypedef
                            (TypedefRegular
                              NamePair {
                                nameC = Name "event_callback_t",
                                nameHsIdent = HsIdentifier
                                  "Event_callback_t"}),
                          structFieldOffset = 576,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "callback",
                          commentLocation = Just
                            "doxygen_docs.h:247:22",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [
                                TextContent
                                  "Optional callback function"],
                            Paragraph
                              [
                                TextContent "See also:",
                                Identifier
                                  "Event_callback_t"]]}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "config_t_user_data",
                      fieldType = HsPtr
                        (HsPrimType HsPrimVoid),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:250:11",
                            fieldName = NamePair {
                              nameC = Name "user_data",
                              nameHsIdent = HsIdentifier
                                "config_t_user_data"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent
                                        "User data for callback"]}])},
                          structFieldType = TypePointer
                            TypeVoid,
                          structFieldOffset = 640,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "user_data",
                          commentLocation = Just
                            "doxygen_docs.h:250:11",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [
                                TextContent
                                  "User data for callback"]]}}],
                  structOrigin =
                  Just
                    Decl {
                      declInfo =
                      DeclInfo {
                        declLoc =
                        "doxygen_docs.h:232:9",
                        declId = NamePair {
                          nameC = Name "config_t",
                          nameHsIdent = HsIdentifier
                            "Config_t"},
                        declOrigin = NameOriginGenerated
                          (AnonId "doxygen_docs.h:232:9"),
                        declAliases = [Name "config_t"],
                        declHeader = "doxygen_docs.h",
                        declComment =
                        Just
                          (Comment
                            [
                              Paragraph [TextContent ""],
                              BlockCommand {
                                blockCommandName = "brief",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent
                                    "Structure with documented fields"]},
                              Paragraph
                                [
                                  TextContent
                                    "This structure demonstrates field documentation."]])},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Config_t"),
                          structSizeof = 88,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:234:14",
                                fieldName = NamePair {
                                  nameC = Name "id",
                                  nameHsIdent = HsIdentifier
                                    "config_t_id"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent
                                            "Unique identifier"]}])},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint32_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtHsRef {
                                    extHsRefModule = HsModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extHsRefIdentifier =
                                    HsIdentifier "Word32"},
                                  extHsSpec = TypeSpec {
                                    typeSpecModule = Just
                                      (HsModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    typeSpecIdentifier = Just
                                      (HsIdentifier "Word32"),
                                    typeSpecInstances = Map.fromList
                                      [
                                        _×_
                                          Eq
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
                                          Enum
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
                                          Bounded
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
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bits
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
                                          Num
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
                                          StaticSize
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
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:237:10",
                                fieldName = NamePair {
                                  nameC = Name "name",
                                  nameHsIdent = HsIdentifier
                                    "config_t_name"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent
                                            "Human-readable name"]}])},
                              structFieldType = TypeConstArray
                                64
                                (TypePrim
                                  (PrimChar
                                    (PrimSignImplicit
                                      (Just Signed)))),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:240:14",
                                fieldName = NamePair {
                                  nameC = Name "flags",
                                  nameHsIdent = HsIdentifier
                                    "config_t_flags"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent
                                            "Configuration flags"]}])},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint32_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtHsRef {
                                    extHsRefModule = HsModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extHsRefIdentifier =
                                    HsIdentifier "Word32"},
                                  extHsSpec = TypeSpec {
                                    typeSpecModule = Just
                                      (HsModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    typeSpecIdentifier = Just
                                      (HsIdentifier "Word32"),
                                    typeSpecInstances = Map.fromList
                                      [
                                        _×_
                                          Eq
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
                                          Enum
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
                                          Bounded
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
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bits
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
                                          Num
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
                                          StaticSize
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
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 544,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:247:22",
                                fieldName = NamePair {
                                  nameC = Name "callback",
                                  nameHsIdent = HsIdentifier
                                    "config_t_callback"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent
                                            "Optional callback function"]},
                                      Paragraph
                                        [
                                          TextContent "See also:",
                                          InlineRefCommand
                                            (ById
                                              NamePair {
                                                nameC = Name "event_callback_t",
                                                nameHsIdent = HsIdentifier
                                                  "Event_callback_t"})],
                                      Paragraph [TextContent ""]])},
                              structFieldType = TypeTypedef
                                (TypedefRegular
                                  NamePair {
                                    nameC = Name "event_callback_t",
                                    nameHsIdent = HsIdentifier
                                      "Event_callback_t"}),
                              structFieldOffset = 576,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:250:11",
                                fieldName = NamePair {
                                  nameC = Name "user_data",
                                  nameHsIdent = HsIdentifier
                                    "config_t_user_data"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent
                                            "User data for callback"]}])},
                              structFieldType = TypePointer
                                TypeVoid,
                              structFieldOffset = 640,
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
                  structComment =
                  Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "config_t",
                      commentLocation = Just
                        "doxygen_docs.h:232:9",
                      commentHeader = Just
                        "doxygen_docs.h",
                      commentChildren =
                      [
                        Paragraph
                          [
                            TextContent
                              "Structure with documented fields"],
                        Paragraph
                          [
                            TextContent
                              "This structure demonstrates field documentation."]]}}
                (Add 5)
                (Seq
                  [
                    PokeByteOff (Idx 6) 0 (Idx 0),
                    PokeByteOff (Idx 6) 4 (Idx 1),
                    PokeByteOff (Idx 6) 68 (Idx 2),
                    PokeByteOff (Idx 6) 72 (Idx 3),
                    PokeByteOff
                      (Idx 6)
                      80
                      (Idx 4)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Config_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Config_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Status_code_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Status_code_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Status_code_t",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "doxygen_docs.h:258:9",
          declId = NamePair {
            nameC = Name "status_code_t",
            nameHsIdent = HsIdentifier
              "Status_code_t"},
          declOrigin = NameOriginGenerated
            (AnonId "doxygen_docs.h:258:9"),
          declAliases = [
            Name "status_code_t"],
          declHeader = "doxygen_docs.h",
          declComment = Just
            (Comment
              [
                Paragraph [TextContent ""],
                BlockCommand {
                  blockCommandName = "brief",
                  blockCommandArgs = [],
                  blockCommandParagraph = [
                    TextContent
                      "Enumeration with documented values"]},
                Paragraph
                  [
                    TextContent
                      "This enum shows different status codes."]])},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Status_code_t",
              newtypeField = HsName
                "@NsVar"
                "un_Status_code_t"},
            enumType = TypePrim
              (PrimIntegral PrimInt Signed),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "doxygen_docs.h:260:5",
                  fieldName = NamePair {
                    nameC = Name "STATUS_OK",
                    nameHsIdent = HsIdentifier
                      "STATUS_OK"},
                  fieldComment = Just
                    (Comment
                      [
                        Paragraph [TextContent ""],
                        BlockCommand {
                          blockCommandName = "brief",
                          blockCommandArgs = [],
                          blockCommandParagraph = [
                            TextContent
                              "Operation successful"]}])},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "doxygen_docs.h:263:5",
                  fieldName = NamePair {
                    nameC = Name
                      "STATUS_INVALID_PARAM",
                    nameHsIdent = HsIdentifier
                      "STATUS_INVALID_PARAM"},
                  fieldComment = Just
                    (Comment
                      [
                        Paragraph [TextContent ""],
                        BlockCommand {
                          blockCommandName = "brief",
                          blockCommandArgs = [],
                          blockCommandParagraph = [
                            TextContent
                              "Invalid parameter provided"]}])},
                enumConstantValue = `-1`},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "doxygen_docs.h:266:5",
                  fieldName = NamePair {
                    nameC = Name "STATUS_NO_MEMORY",
                    nameHsIdent = HsIdentifier
                      "STATUS_NO_MEMORY"},
                  fieldComment = Just
                    (Comment
                      [
                        Paragraph [TextContent ""],
                        BlockCommand {
                          blockCommandName = "brief",
                          blockCommandArgs = [],
                          blockCommandParagraph = [
                            TextContent
                              "Memory allocation failed"]}])},
                enumConstantValue = `-2`},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "doxygen_docs.h:269:5",
                  fieldName = NamePair {
                    nameC = Name "STATUS_TIMEOUT",
                    nameHsIdent = HsIdentifier
                      "STATUS_TIMEOUT"},
                  fieldComment = Just
                    (Comment
                      [
                        Paragraph [TextContent ""],
                        BlockCommand {
                          blockCommandName = "brief",
                          blockCommandArgs = [],
                          blockCommandParagraph = [
                            TextContent
                              "Operation timed out"]}])},
                enumConstantValue = `-3`},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "doxygen_docs.h:272:5",
                  fieldName = NamePair {
                    nameC = Name "STATUS_ERROR",
                    nameHsIdent = HsIdentifier
                      "STATUS_ERROR"},
                  fieldComment = Just
                    (Comment
                      [
                        Paragraph [TextContent ""],
                        BlockCommand {
                          blockCommandName = "brief",
                          blockCommandArgs = [],
                          blockCommandParagraph = [
                            TextContent
                              "Generic error"]}])},
                enumConstantValue = `-99`}]},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Read, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "status_code_t",
          commentLocation = Just
            "doxygen_docs.h:258:9",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Enumeration with documented values"],
            Paragraph
              [
                TextContent
                  "This enum shows different status codes."]]}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Status_code_t",
          structConstr = HsName
            "@NsConstr"
            "Status_code_t",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_Status_code_t",
              fieldType = HsPrimType
                HsPrimCInt,
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
                  structName = HsName
                    "@NsTypeConstr"
                    "Status_code_t",
                  structConstr = HsName
                    "@NsConstr"
                    "Status_code_t",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "un_Status_code_t",
                      fieldType = HsPrimType
                        HsPrimCInt,
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
                  structName = HsName
                    "@NsTypeConstr"
                    "Status_code_t",
                  structConstr = HsName
                    "@NsConstr"
                    "Status_code_t",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "un_Status_code_t",
                      fieldType = HsPrimType
                        HsPrimCInt,
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Status_code_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Status_code_t",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Status_code_t",
          structConstr = HsName
            "@NsConstr"
            "Status_code_t",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_Status_code_t",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (HsPrimType HsPrimCInt)
        (Map.fromList
          [
            _×_
              `-99`
              (NE.fromList ["STATUS_ERROR"]),
            _×_
              `-3`
              (NE.fromList
                ["STATUS_TIMEOUT"]),
            _×_
              `-2`
              (NE.fromList
                ["STATUS_NO_MEMORY"]),
            _×_
              `-1`
              (NE.fromList
                ["STATUS_INVALID_PARAM"]),
            _×_
              0
              (NE.fromList ["STATUS_OK"])])
        False,
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Status_code_t",
          structConstr = HsName
            "@NsConstr"
            "Status_code_t",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_Status_code_t",
              fieldType = HsPrimType
                HsPrimCInt,
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
          structName = HsName
            "@NsTypeConstr"
            "Status_code_t",
          structConstr = HsName
            "@NsConstr"
            "Status_code_t",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_Status_code_t",
              fieldType = HsPrimType
                HsPrimCInt,
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
      patSynName = HsName
        "@NsConstr"
        "STATUS_OK",
      patSynType = HsName
        "@NsTypeConstr"
        "Status_code_t",
      patSynConstr = HsName
        "@NsConstr"
        "Status_code_t",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "doxygen_docs.h:260:5",
            fieldName = NamePair {
              nameC = Name "STATUS_OK",
              nameHsIdent = HsIdentifier
                "STATUS_OK"},
            fieldComment = Just
              (Comment
                [
                  Paragraph [TextContent ""],
                  BlockCommand {
                    blockCommandName = "brief",
                    blockCommandArgs = [],
                    blockCommandParagraph = [
                      TextContent
                        "Operation successful"]}])},
          enumConstantValue = 0},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "STATUS_OK",
          commentLocation = Just
            "doxygen_docs.h:260:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Operation successful"]]}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "STATUS_INVALID_PARAM",
      patSynType = HsName
        "@NsTypeConstr"
        "Status_code_t",
      patSynConstr = HsName
        "@NsConstr"
        "Status_code_t",
      patSynValue = `-1`,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "doxygen_docs.h:263:5",
            fieldName = NamePair {
              nameC = Name
                "STATUS_INVALID_PARAM",
              nameHsIdent = HsIdentifier
                "STATUS_INVALID_PARAM"},
            fieldComment = Just
              (Comment
                [
                  Paragraph [TextContent ""],
                  BlockCommand {
                    blockCommandName = "brief",
                    blockCommandArgs = [],
                    blockCommandParagraph = [
                      TextContent
                        "Invalid parameter provided"]}])},
          enumConstantValue = `-1`},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "STATUS_INVALID_PARAM",
          commentLocation = Just
            "doxygen_docs.h:263:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Invalid parameter provided"]]}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "STATUS_NO_MEMORY",
      patSynType = HsName
        "@NsTypeConstr"
        "Status_code_t",
      patSynConstr = HsName
        "@NsConstr"
        "Status_code_t",
      patSynValue = `-2`,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "doxygen_docs.h:266:5",
            fieldName = NamePair {
              nameC = Name "STATUS_NO_MEMORY",
              nameHsIdent = HsIdentifier
                "STATUS_NO_MEMORY"},
            fieldComment = Just
              (Comment
                [
                  Paragraph [TextContent ""],
                  BlockCommand {
                    blockCommandName = "brief",
                    blockCommandArgs = [],
                    blockCommandParagraph = [
                      TextContent
                        "Memory allocation failed"]}])},
          enumConstantValue = `-2`},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "STATUS_NO_MEMORY",
          commentLocation = Just
            "doxygen_docs.h:266:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Memory allocation failed"]]}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "STATUS_TIMEOUT",
      patSynType = HsName
        "@NsTypeConstr"
        "Status_code_t",
      patSynConstr = HsName
        "@NsConstr"
        "Status_code_t",
      patSynValue = `-3`,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "doxygen_docs.h:269:5",
            fieldName = NamePair {
              nameC = Name "STATUS_TIMEOUT",
              nameHsIdent = HsIdentifier
                "STATUS_TIMEOUT"},
            fieldComment = Just
              (Comment
                [
                  Paragraph [TextContent ""],
                  BlockCommand {
                    blockCommandName = "brief",
                    blockCommandArgs = [],
                    blockCommandParagraph = [
                      TextContent
                        "Operation timed out"]}])},
          enumConstantValue = `-3`},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "STATUS_TIMEOUT",
          commentLocation = Just
            "doxygen_docs.h:269:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Operation timed out"]]}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "STATUS_ERROR",
      patSynType = HsName
        "@NsTypeConstr"
        "Status_code_t",
      patSynConstr = HsName
        "@NsConstr"
        "Status_code_t",
      patSynValue = `-99`,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "doxygen_docs.h:272:5",
            fieldName = NamePair {
              nameC = Name "STATUS_ERROR",
              nameHsIdent = HsIdentifier
                "STATUS_ERROR"},
            fieldComment = Just
              (Comment
                [
                  Paragraph [TextContent ""],
                  BlockCommand {
                    blockCommandName = "brief",
                    blockCommandArgs = [],
                    blockCommandParagraph = [
                      TextContent
                        "Generic error"]}])},
          enumConstantValue = `-99`},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "STATUS_ERROR",
          commentLocation = Just
            "doxygen_docs.h:272:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent "Generic error"]]}},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Data_union_t_as_parts",
      structConstr = HsName
        "@NsConstr"
        "Data_union_t_as_parts",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "data_union_t_as_parts_low",
          fieldType = HsExtBinding
            ExtHsRef {
              extHsRefModule = HsModuleName
                "HsBindgen.Runtime.Prelude",
              extHsRefIdentifier =
              HsIdentifier "Word16"}
            TypeSpec {
              typeSpecModule = Just
                (HsModuleName
                  "HsBindgen.Runtime.Prelude"),
              typeSpecIdentifier = Just
                (HsIdentifier "Word16"),
              typeSpecInstances = Map.fromList
                [
                  _×_
                    Eq
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
                    Enum
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
                    Bounded
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
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bits
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
                    Num
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
                    StaticSize
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
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]},
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "doxygen_docs.h:291:18",
                fieldName = NamePair {
                  nameC = Name "low",
                  nameHsIdent = HsIdentifier
                    "data_union_t_as_parts_low"},
                fieldComment = Just
                  (Comment
                    [
                      Paragraph [TextContent ""],
                      BlockCommand {
                        blockCommandName = "brief",
                        blockCommandArgs = [],
                        blockCommandParagraph = [
                          TextContent "Low 16 bits"]}])},
              structFieldType = TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "uint16_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "Word16"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "Word16"),
                    typeSpecInstances = Map.fromList
                      [
                        _×_
                          Eq
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
                          Enum
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
                          Bounded
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
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bits
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
                          Num
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
                          StaticSize
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
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}},
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "low",
              commentLocation = Just
                "doxygen_docs.h:291:18",
              commentHeader = Just
                "doxygen_docs.h",
              commentChildren = [
                Paragraph
                  [TextContent "Low 16 bits"]]}},
        Field {
          fieldName = HsName
            "@NsVar"
            "data_union_t_as_parts_high",
          fieldType = HsExtBinding
            ExtHsRef {
              extHsRefModule = HsModuleName
                "HsBindgen.Runtime.Prelude",
              extHsRefIdentifier =
              HsIdentifier "Word16"}
            TypeSpec {
              typeSpecModule = Just
                (HsModuleName
                  "HsBindgen.Runtime.Prelude"),
              typeSpecIdentifier = Just
                (HsIdentifier "Word16"),
              typeSpecInstances = Map.fromList
                [
                  _×_
                    Eq
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
                    Enum
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
                    Bounded
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
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bits
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
                    Num
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
                    StaticSize
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
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]},
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "doxygen_docs.h:292:18",
                fieldName = NamePair {
                  nameC = Name "high",
                  nameHsIdent = HsIdentifier
                    "data_union_t_as_parts_high"},
                fieldComment = Just
                  (Comment
                    [
                      Paragraph [TextContent ""],
                      BlockCommand {
                        blockCommandName = "brief",
                        blockCommandArgs = [],
                        blockCommandParagraph = [
                          TextContent "High 16 bits"]}])},
              structFieldType = TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "uint16_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "Word16"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "Word16"),
                    typeSpecInstances = Map.fromList
                      [
                        _×_
                          Eq
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
                          Enum
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
                          Bounded
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
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bits
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
                          Num
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
                          StaticSize
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
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
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
              commentOrigin = Just "high",
              commentLocation = Just
                "doxygen_docs.h:292:18",
              commentHeader = Just
                "doxygen_docs.h",
              commentChildren = [
                Paragraph
                  [
                    TextContent "High 16 bits"]]}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "doxygen_docs.h:290:5",
            declId = NamePair {
              nameC = Name
                "data_union_t_as_parts",
              nameHsIdent = HsIdentifier
                "Data_union_t_as_parts"},
            declOrigin = NameOriginGenerated
              (AnonId "doxygen_docs.h:290:5"),
            declAliases = [],
            declHeader = "doxygen_docs.h",
            declComment = Just
              (Comment
                [
                  Paragraph [TextContent ""],
                  BlockCommand {
                    blockCommandName = "brief",
                    blockCommandArgs = [],
                    blockCommandParagraph = [
                      TextContent
                        "Structured representation",
                      TextContent ""]},
                  BlockCommand {
                    blockCommandName = "details",
                    blockCommandArgs = [],
                    blockCommandParagraph = [
                      TextContent
                        "Allows access to high and low parts separately"]}])},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Data_union_t_as_parts"),
              structSizeof = 4,
              structAlignment = 2,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:291:18",
                    fieldName = NamePair {
                      nameC = Name "low",
                      nameHsIdent = HsIdentifier
                        "data_union_t_as_parts_low"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent "Low 16 bits"]}])},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint16_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word16"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word16"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
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
                              Enum
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
                              Bounded
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
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
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
                              Num
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
                              StaticSize
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
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:292:18",
                    fieldName = NamePair {
                      nameC = Name "high",
                      nameHsIdent = HsIdentifier
                        "data_union_t_as_parts_high"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent "High 16 bits"]}])},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint16_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word16"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word16"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
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
                              Enum
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
                              Bounded
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
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
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
                              Num
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
                              StaticSize
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
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 16,
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
          commentOrigin = Just
            "data_union_t_as_parts",
          commentLocation = Just
            "doxygen_docs.h:290:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Structured representation"],
            Paragraph
              [
                TextContent
                  "Allows access to high and low parts separately"]]}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Data_union_t_as_parts",
          structConstr = HsName
            "@NsConstr"
            "Data_union_t_as_parts",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "data_union_t_as_parts_low",
              fieldType = HsExtBinding
                ExtHsRef {
                  extHsRefModule = HsModuleName
                    "HsBindgen.Runtime.Prelude",
                  extHsRefIdentifier =
                  HsIdentifier "Word16"}
                TypeSpec {
                  typeSpecModule = Just
                    (HsModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (HsIdentifier "Word16"),
                  typeSpecInstances = Map.fromList
                    [
                      _×_
                        Eq
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
                        Enum
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
                        Bounded
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
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bits
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
                        Num
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
                        StaticSize
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
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]},
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:291:18",
                    fieldName = NamePair {
                      nameC = Name "low",
                      nameHsIdent = HsIdentifier
                        "data_union_t_as_parts_low"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent "Low 16 bits"]}])},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint16_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word16"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word16"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
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
                              Enum
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
                              Bounded
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
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
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
                              Num
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
                              StaticSize
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
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "low",
                  commentLocation = Just
                    "doxygen_docs.h:291:18",
                  commentHeader = Just
                    "doxygen_docs.h",
                  commentChildren = [
                    Paragraph
                      [TextContent "Low 16 bits"]]}},
            Field {
              fieldName = HsName
                "@NsVar"
                "data_union_t_as_parts_high",
              fieldType = HsExtBinding
                ExtHsRef {
                  extHsRefModule = HsModuleName
                    "HsBindgen.Runtime.Prelude",
                  extHsRefIdentifier =
                  HsIdentifier "Word16"}
                TypeSpec {
                  typeSpecModule = Just
                    (HsModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (HsIdentifier "Word16"),
                  typeSpecInstances = Map.fromList
                    [
                      _×_
                        Eq
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
                        Enum
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
                        Bounded
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
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bits
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
                        Num
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
                        StaticSize
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
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]},
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:292:18",
                    fieldName = NamePair {
                      nameC = Name "high",
                      nameHsIdent = HsIdentifier
                        "data_union_t_as_parts_high"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent "High 16 bits"]}])},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint16_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word16"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word16"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
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
                              Enum
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
                              Bounded
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
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
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
                              Num
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
                              StaticSize
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
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
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
                  commentOrigin = Just "high",
                  commentLocation = Just
                    "doxygen_docs.h:292:18",
                  commentHeader = Just
                    "doxygen_docs.h",
                  commentChildren = [
                    Paragraph
                      [
                        TextContent "High 16 bits"]]}}],
          structOrigin =
          Just
            Decl {
              declInfo =
              DeclInfo {
                declLoc =
                "doxygen_docs.h:290:5",
                declId = NamePair {
                  nameC = Name
                    "data_union_t_as_parts",
                  nameHsIdent = HsIdentifier
                    "Data_union_t_as_parts"},
                declOrigin = NameOriginGenerated
                  (AnonId "doxygen_docs.h:290:5"),
                declAliases = [],
                declHeader = "doxygen_docs.h",
                declComment =
                Just
                  (Comment
                    [
                      Paragraph [TextContent ""],
                      BlockCommand {
                        blockCommandName = "brief",
                        blockCommandArgs = [],
                        blockCommandParagraph = [
                          TextContent
                            "Structured representation",
                          TextContent ""]},
                      BlockCommand {
                        blockCommandName = "details",
                        blockCommandArgs = [],
                        blockCommandParagraph =
                        [
                          TextContent
                            "Allows access to high and low parts separately"]}])},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName
                      "@NsConstr"
                      "Data_union_t_as_parts"),
                  structSizeof = 4,
                  structAlignment = 2,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "doxygen_docs.h:291:18",
                        fieldName = NamePair {
                          nameC = Name "low",
                          nameHsIdent = HsIdentifier
                            "data_union_t_as_parts_low"},
                        fieldComment = Just
                          (Comment
                            [
                              Paragraph [TextContent ""],
                              BlockCommand {
                                blockCommandName = "brief",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent "Low 16 bits"]}])},
                      structFieldType = TypeExtBinding
                        ResolvedExtBinding {
                          extCName = QualName {
                            qualNameName = Name "uint16_t",
                            qualNameKind =
                            NameKindOrdinary},
                          extHsRef = ExtHsRef {
                            extHsRefModule = HsModuleName
                              "HsBindgen.Runtime.Prelude",
                            extHsRefIdentifier =
                            HsIdentifier "Word16"},
                          extHsSpec = TypeSpec {
                            typeSpecModule = Just
                              (HsModuleName
                                "HsBindgen.Runtime.Prelude"),
                            typeSpecIdentifier = Just
                              (HsIdentifier "Word16"),
                            typeSpecInstances = Map.fromList
                              [
                                _×_
                                  Eq
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
                                  Enum
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
                                  Bounded
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
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bits
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
                                  Num
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
                                  StaticSize
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
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]}},
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "doxygen_docs.h:292:18",
                        fieldName = NamePair {
                          nameC = Name "high",
                          nameHsIdent = HsIdentifier
                            "data_union_t_as_parts_high"},
                        fieldComment = Just
                          (Comment
                            [
                              Paragraph [TextContent ""],
                              BlockCommand {
                                blockCommandName = "brief",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent "High 16 bits"]}])},
                      structFieldType = TypeExtBinding
                        ResolvedExtBinding {
                          extCName = QualName {
                            qualNameName = Name "uint16_t",
                            qualNameKind =
                            NameKindOrdinary},
                          extHsRef = ExtHsRef {
                            extHsRefModule = HsModuleName
                              "HsBindgen.Runtime.Prelude",
                            extHsRefIdentifier =
                            HsIdentifier "Word16"},
                          extHsSpec = TypeSpec {
                            typeSpecModule = Just
                              (HsModuleName
                                "HsBindgen.Runtime.Prelude"),
                            typeSpecIdentifier = Just
                              (HsIdentifier "Word16"),
                            typeSpecInstances = Map.fromList
                              [
                                _×_
                                  Eq
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
                                  Enum
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
                                  Bounded
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
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bits
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
                                  Num
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
                                  StaticSize
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
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]}},
                      structFieldOffset = 16,
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
              commentOrigin = Just
                "data_union_t_as_parts",
              commentLocation = Just
                "doxygen_docs.h:290:5",
              commentHeader = Just
                "doxygen_docs.h",
              commentChildren = [
                Paragraph
                  [
                    TextContent
                      "Structured representation"],
                Paragraph
                  [
                    TextContent
                      "Allows access to high and low parts separately"]]}}
        StorableInstance {
          storableSizeOf = 4,
          storableAlignment = 2,
          storablePeek =
          Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Data_union_t_as_parts",
                  structConstr = HsName
                    "@NsConstr"
                    "Data_union_t_as_parts",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "data_union_t_as_parts_low",
                      fieldType = HsExtBinding
                        ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "Word16"}
                        TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "Word16"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
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
                                Enum
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
                                Bounded
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
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
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
                                Num
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
                                StaticSize
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
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:291:18",
                            fieldName = NamePair {
                              nameC = Name "low",
                              nameHsIdent = HsIdentifier
                                "data_union_t_as_parts_low"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent "Low 16 bits"]}])},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint16_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtHsRef {
                                extHsRefModule = HsModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extHsRefIdentifier =
                                HsIdentifier "Word16"},
                              extHsSpec = TypeSpec {
                                typeSpecModule = Just
                                  (HsModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                typeSpecIdentifier = Just
                                  (HsIdentifier "Word16"),
                                typeSpecInstances = Map.fromList
                                  [
                                    _×_
                                      Eq
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
                                      Enum
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
                                      Bounded
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
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bits
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
                                      Num
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
                                      StaticSize
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
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}},
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "low",
                          commentLocation = Just
                            "doxygen_docs.h:291:18",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [TextContent "Low 16 bits"]]}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "data_union_t_as_parts_high",
                      fieldType = HsExtBinding
                        ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "Word16"}
                        TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "Word16"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
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
                                Enum
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
                                Bounded
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
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
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
                                Num
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
                                StaticSize
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
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:292:18",
                            fieldName = NamePair {
                              nameC = Name "high",
                              nameHsIdent = HsIdentifier
                                "data_union_t_as_parts_high"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent "High 16 bits"]}])},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint16_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtHsRef {
                                extHsRefModule = HsModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extHsRefIdentifier =
                                HsIdentifier "Word16"},
                              extHsSpec = TypeSpec {
                                typeSpecModule = Just
                                  (HsModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                typeSpecIdentifier = Just
                                  (HsIdentifier "Word16"),
                                typeSpecInstances = Map.fromList
                                  [
                                    _×_
                                      Eq
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
                                      Enum
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
                                      Bounded
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
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bits
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
                                      Num
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
                                      StaticSize
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
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
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
                          commentOrigin = Just "high",
                          commentLocation = Just
                            "doxygen_docs.h:292:18",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [
                                TextContent "High 16 bits"]]}}],
                  structOrigin =
                  Just
                    Decl {
                      declInfo =
                      DeclInfo {
                        declLoc =
                        "doxygen_docs.h:290:5",
                        declId = NamePair {
                          nameC = Name
                            "data_union_t_as_parts",
                          nameHsIdent = HsIdentifier
                            "Data_union_t_as_parts"},
                        declOrigin = NameOriginGenerated
                          (AnonId "doxygen_docs.h:290:5"),
                        declAliases = [],
                        declHeader = "doxygen_docs.h",
                        declComment =
                        Just
                          (Comment
                            [
                              Paragraph [TextContent ""],
                              BlockCommand {
                                blockCommandName = "brief",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent
                                    "Structured representation",
                                  TextContent ""]},
                              BlockCommand {
                                blockCommandName = "details",
                                blockCommandArgs = [],
                                blockCommandParagraph =
                                [
                                  TextContent
                                    "Allows access to high and low parts separately"]}])},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Data_union_t_as_parts"),
                          structSizeof = 4,
                          structAlignment = 2,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:291:18",
                                fieldName = NamePair {
                                  nameC = Name "low",
                                  nameHsIdent = HsIdentifier
                                    "data_union_t_as_parts_low"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent "Low 16 bits"]}])},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint16_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtHsRef {
                                    extHsRefModule = HsModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extHsRefIdentifier =
                                    HsIdentifier "Word16"},
                                  extHsSpec = TypeSpec {
                                    typeSpecModule = Just
                                      (HsModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    typeSpecIdentifier = Just
                                      (HsIdentifier "Word16"),
                                    typeSpecInstances = Map.fromList
                                      [
                                        _×_
                                          Eq
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
                                          Enum
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
                                          Bounded
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
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bits
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
                                          Num
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
                                          StaticSize
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
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:292:18",
                                fieldName = NamePair {
                                  nameC = Name "high",
                                  nameHsIdent = HsIdentifier
                                    "data_union_t_as_parts_high"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent "High 16 bits"]}])},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint16_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtHsRef {
                                    extHsRefModule = HsModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extHsRefIdentifier =
                                    HsIdentifier "Word16"},
                                  extHsSpec = TypeSpec {
                                    typeSpecModule = Just
                                      (HsModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    typeSpecIdentifier = Just
                                      (HsIdentifier "Word16"),
                                    typeSpecInstances = Map.fromList
                                      [
                                        _×_
                                          Eq
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
                                          Enum
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
                                          Bounded
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
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bits
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
                                          Num
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
                                          StaticSize
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
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 16,
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
                  structComment =
                  Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "data_union_t_as_parts",
                      commentLocation = Just
                        "doxygen_docs.h:290:5",
                      commentHeader = Just
                        "doxygen_docs.h",
                      commentChildren =
                      [
                        Paragraph
                          [
                            TextContent
                              "Structured representation"],
                        Paragraph
                          [
                            TextContent
                              "Allows access to high and low parts separately"]]}})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 2]),
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
                    "Data_union_t_as_parts",
                  structConstr = HsName
                    "@NsConstr"
                    "Data_union_t_as_parts",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "data_union_t_as_parts_low",
                      fieldType = HsExtBinding
                        ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "Word16"}
                        TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "Word16"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
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
                                Enum
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
                                Bounded
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
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
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
                                Num
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
                                StaticSize
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
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:291:18",
                            fieldName = NamePair {
                              nameC = Name "low",
                              nameHsIdent = HsIdentifier
                                "data_union_t_as_parts_low"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent "Low 16 bits"]}])},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint16_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtHsRef {
                                extHsRefModule = HsModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extHsRefIdentifier =
                                HsIdentifier "Word16"},
                              extHsSpec = TypeSpec {
                                typeSpecModule = Just
                                  (HsModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                typeSpecIdentifier = Just
                                  (HsIdentifier "Word16"),
                                typeSpecInstances = Map.fromList
                                  [
                                    _×_
                                      Eq
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
                                      Enum
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
                                      Bounded
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
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bits
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
                                      Num
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
                                      StaticSize
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
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}},
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "low",
                          commentLocation = Just
                            "doxygen_docs.h:291:18",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [TextContent "Low 16 bits"]]}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "data_union_t_as_parts_high",
                      fieldType = HsExtBinding
                        ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "Word16"}
                        TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "Word16"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
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
                                Enum
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
                                Bounded
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
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
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
                                Num
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
                                StaticSize
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
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:292:18",
                            fieldName = NamePair {
                              nameC = Name "high",
                              nameHsIdent = HsIdentifier
                                "data_union_t_as_parts_high"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent "High 16 bits"]}])},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint16_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtHsRef {
                                extHsRefModule = HsModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extHsRefIdentifier =
                                HsIdentifier "Word16"},
                              extHsSpec = TypeSpec {
                                typeSpecModule = Just
                                  (HsModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                typeSpecIdentifier = Just
                                  (HsIdentifier "Word16"),
                                typeSpecInstances = Map.fromList
                                  [
                                    _×_
                                      Eq
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
                                      Enum
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
                                      Bounded
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
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bits
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
                                      Num
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
                                      StaticSize
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
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
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
                          commentOrigin = Just "high",
                          commentLocation = Just
                            "doxygen_docs.h:292:18",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [
                                TextContent "High 16 bits"]]}}],
                  structOrigin =
                  Just
                    Decl {
                      declInfo =
                      DeclInfo {
                        declLoc =
                        "doxygen_docs.h:290:5",
                        declId = NamePair {
                          nameC = Name
                            "data_union_t_as_parts",
                          nameHsIdent = HsIdentifier
                            "Data_union_t_as_parts"},
                        declOrigin = NameOriginGenerated
                          (AnonId "doxygen_docs.h:290:5"),
                        declAliases = [],
                        declHeader = "doxygen_docs.h",
                        declComment =
                        Just
                          (Comment
                            [
                              Paragraph [TextContent ""],
                              BlockCommand {
                                blockCommandName = "brief",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent
                                    "Structured representation",
                                  TextContent ""]},
                              BlockCommand {
                                blockCommandName = "details",
                                blockCommandArgs = [],
                                blockCommandParagraph =
                                [
                                  TextContent
                                    "Allows access to high and low parts separately"]}])},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Data_union_t_as_parts"),
                          structSizeof = 4,
                          structAlignment = 2,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:291:18",
                                fieldName = NamePair {
                                  nameC = Name "low",
                                  nameHsIdent = HsIdentifier
                                    "data_union_t_as_parts_low"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent "Low 16 bits"]}])},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint16_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtHsRef {
                                    extHsRefModule = HsModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extHsRefIdentifier =
                                    HsIdentifier "Word16"},
                                  extHsSpec = TypeSpec {
                                    typeSpecModule = Just
                                      (HsModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    typeSpecIdentifier = Just
                                      (HsIdentifier "Word16"),
                                    typeSpecInstances = Map.fromList
                                      [
                                        _×_
                                          Eq
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
                                          Enum
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
                                          Bounded
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
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bits
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
                                          Num
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
                                          StaticSize
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
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:292:18",
                                fieldName = NamePair {
                                  nameC = Name "high",
                                  nameHsIdent = HsIdentifier
                                    "data_union_t_as_parts_high"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent "High 16 bits"]}])},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint16_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtHsRef {
                                    extHsRefModule = HsModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extHsRefIdentifier =
                                    HsIdentifier "Word16"},
                                  extHsSpec = TypeSpec {
                                    typeSpecModule = Just
                                      (HsModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    typeSpecIdentifier = Just
                                      (HsIdentifier "Word16"),
                                    typeSpecInstances = Map.fromList
                                      [
                                        _×_
                                          Eq
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
                                          Enum
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
                                          Bounded
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
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bits
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
                                          Num
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
                                          StaticSize
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
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 16,
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
                  structComment =
                  Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "data_union_t_as_parts",
                      commentLocation = Just
                        "doxygen_docs.h:290:5",
                      commentHeader = Just
                        "doxygen_docs.h",
                      commentChildren =
                      [
                        Paragraph
                          [
                            TextContent
                              "Structured representation"],
                        Paragraph
                          [
                            TextContent
                              "Allows access to high and low parts separately"]]}}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      2
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
        "Data_union_t_as_parts",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Data_union_t_as_parts",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Data_union_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Data_union_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Data_union_t",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin =
      Decl {
        declInfo =
        DeclInfo {
          declLoc =
          "doxygen_docs.h:281:9",
          declId = NamePair {
            nameC = Name "data_union_t",
            nameHsIdent = HsIdentifier
              "Data_union_t"},
          declOrigin = NameOriginGenerated
            (AnonId "doxygen_docs.h:281:9"),
          declAliases = [
            Name "data_union_t"],
          declHeader = "doxygen_docs.h",
          declComment =
          Just
            (Comment
              [
                Paragraph [TextContent ""],
                VerbatimLine "data_union_t",
                Paragraph [TextContent ""],
                BlockCommand {
                  blockCommandName = "brief",
                  blockCommandArgs = [],
                  blockCommandParagraph = [
                    TextContent
                      "Union with documented fields"]},
                Paragraph
                  [
                    TextContent
                      "This union demonstrates different data representations."]])},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Data_union_t",
              newtypeField = HsName
                "@NsVar"
                "un_Data_union_t"},
            unionSizeof = 4,
            unionAlignment = 4,
            unionFields = [
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc =
                  "doxygen_docs.h:282:13",
                  fieldName = NamePair {
                    nameC = Name "as_int",
                    nameHsIdent = HsIdentifier
                      "data_union_t_as_int"},
                  fieldComment = Just
                    (Comment
                      [
                        Paragraph [TextContent ""],
                        BlockCommand {
                          blockCommandName = "brief",
                          blockCommandArgs = [],
                          blockCommandParagraph = [
                            TextContent
                              "Integer representation"]}])},
                unionFieldType = TypeExtBinding
                  ResolvedExtBinding {
                    extCName = QualName {
                      qualNameName = Name "int32_t",
                      qualNameKind =
                      NameKindOrdinary},
                    extHsRef = ExtHsRef {
                      extHsRefModule = HsModuleName
                        "HsBindgen.Runtime.Prelude",
                      extHsRefIdentifier =
                      HsIdentifier "Int32"},
                    extHsSpec = TypeSpec {
                      typeSpecModule = Just
                        (HsModuleName
                          "HsBindgen.Runtime.Prelude"),
                      typeSpecIdentifier = Just
                        (HsIdentifier "Int32"),
                      typeSpecInstances = Map.fromList
                        [
                          _×_
                            Eq
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
                            Enum
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
                            Bounded
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
                            Show
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bits
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
                            Num
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
                            StaticSize
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
                            WriteRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Storable
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = [
                                  ]})]}}},
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc =
                  "doxygen_docs.h:283:11",
                  fieldName = NamePair {
                    nameC = Name "as_float",
                    nameHsIdent = HsIdentifier
                      "data_union_t_as_float"},
                  fieldComment = Just
                    (Comment
                      [
                        Paragraph [TextContent ""],
                        BlockCommand {
                          blockCommandName = "brief",
                          blockCommandArgs = [],
                          blockCommandParagraph = [
                            TextContent
                              "Float representation"]}])},
                unionFieldType = TypePrim
                  (PrimFloating PrimFloat)},
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc =
                  "doxygen_docs.h:284:13",
                  fieldName = NamePair {
                    nameC = Name "as_bytes",
                    nameHsIdent = HsIdentifier
                      "data_union_t_as_bytes"},
                  fieldComment = Just
                    (Comment
                      [
                        Paragraph [TextContent ""],
                        BlockCommand {
                          blockCommandName = "brief",
                          blockCommandArgs = [],
                          blockCommandParagraph = [
                            TextContent
                              "Byte array representation"]}])},
                unionFieldType = TypeConstArray
                  4
                  (TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint8_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word8"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word8"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
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
                              Enum
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
                              Bounded
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
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
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
                              Num
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
                              StaticSize
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
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}})},
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc =
                  "doxygen_docs.h:293:30",
                  fieldName = NamePair {
                    nameC = Name "as_parts",
                    nameHsIdent = HsIdentifier
                      "data_union_t_as_parts"},
                  fieldComment = Just
                    (Comment
                      [
                        Paragraph
                          [
                            TextContent
                              "As Parts Struct"]])},
                unionFieldType = TypeStruct
                  NamePair {
                    nameC = Name
                      "data_union_t_as_parts",
                    nameHsIdent = HsIdentifier
                      "Data_union_t_as_parts"}
                  (NameOriginGenerated
                    (AnonId
                      "doxygen_docs.h:290:5"))}]},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "data_union_t",
          commentLocation = Just
            "doxygen_docs.h:281:9",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Verbatim "data_union_t",
            Paragraph
              [
                TextContent
                  "Union with documented fields"],
            Paragraph
              [
                TextContent
                  "This union demonstrates different data representations."]]}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveVia
        (HsSizedByteArray 4 4),
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Data_union_t",
      deriveInstanceComment =
      Nothing},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = HsName
        "@NsVar"
        "get_data_union_t_as_int",
      unionGetterType = HsExtBinding
        ExtHsRef {
          extHsRefModule = HsModuleName
            "HsBindgen.Runtime.Prelude",
          extHsRefIdentifier =
          HsIdentifier "Int32"}
        TypeSpec {
          typeSpecModule = Just
            (HsModuleName
              "HsBindgen.Runtime.Prelude"),
          typeSpecIdentifier = Just
            (HsIdentifier "Int32"),
          typeSpecInstances = Map.fromList
            [
              _×_
                Eq
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
                Enum
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
                Bounded
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
                Show
                (Require
                  InstanceSpec {
                    instanceSpecStrategy = Nothing,
                    instanceSpecConstraints = []}),
              _×_
                Bits
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
                Num
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
                StaticSize
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
                WriteRaw
                (Require
                  InstanceSpec {
                    instanceSpecStrategy = Nothing,
                    instanceSpecConstraints = []}),
              _×_
                Storable
                (Require
                  InstanceSpec {
                    instanceSpecStrategy = Nothing,
                    instanceSpecConstraints = [
                      ]})]},
      unionGetterConstr = HsName
        "@NsTypeConstr"
        "Data_union_t",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "as_int",
          commentLocation = Just
            "doxygen_docs.h:282:13",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Integer representation"],
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_data_union_t_as_int"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = HsName
        "@NsVar"
        "set_data_union_t_as_int",
      unionSetterType = HsExtBinding
        ExtHsRef {
          extHsRefModule = HsModuleName
            "HsBindgen.Runtime.Prelude",
          extHsRefIdentifier =
          HsIdentifier "Int32"}
        TypeSpec {
          typeSpecModule = Just
            (HsModuleName
              "HsBindgen.Runtime.Prelude"),
          typeSpecIdentifier = Just
            (HsIdentifier "Int32"),
          typeSpecInstances = Map.fromList
            [
              _×_
                Eq
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
                Enum
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
                Bounded
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
                Show
                (Require
                  InstanceSpec {
                    instanceSpecStrategy = Nothing,
                    instanceSpecConstraints = []}),
              _×_
                Bits
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
                Num
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
                StaticSize
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
                WriteRaw
                (Require
                  InstanceSpec {
                    instanceSpecStrategy = Nothing,
                    instanceSpecConstraints = []}),
              _×_
                Storable
                (Require
                  InstanceSpec {
                    instanceSpecStrategy = Nothing,
                    instanceSpecConstraints = [
                      ]})]},
      unionSetterConstr = HsName
        "@NsTypeConstr"
        "Data_union_t",
      unionSetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeader = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "get_data_union_t_as_int"]]}},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = HsName
        "@NsVar"
        "get_data_union_t_as_float",
      unionGetterType = HsPrimType
        HsPrimCFloat,
      unionGetterConstr = HsName
        "@NsTypeConstr"
        "Data_union_t",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "as_float",
          commentLocation = Just
            "doxygen_docs.h:283:11",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Float representation"],
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_data_union_t_as_float"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = HsName
        "@NsVar"
        "set_data_union_t_as_float",
      unionSetterType = HsPrimType
        HsPrimCFloat,
      unionSetterConstr = HsName
        "@NsTypeConstr"
        "Data_union_t",
      unionSetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeader = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "get_data_union_t_as_float"]]}},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = HsName
        "@NsVar"
        "get_data_union_t_as_bytes",
      unionGetterType = HsConstArray
        4
        (HsExtBinding
          ExtHsRef {
            extHsRefModule = HsModuleName
              "HsBindgen.Runtime.Prelude",
            extHsRefIdentifier =
            HsIdentifier "Word8"}
          TypeSpec {
            typeSpecModule = Just
              (HsModuleName
                "HsBindgen.Runtime.Prelude"),
            typeSpecIdentifier = Just
              (HsIdentifier "Word8"),
            typeSpecInstances = Map.fromList
              [
                _×_
                  Eq
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
                  Enum
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
                  Bounded
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
                  Show
                  (Require
                    InstanceSpec {
                      instanceSpecStrategy = Nothing,
                      instanceSpecConstraints = []}),
                _×_
                  Bits
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
                  Num
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
                  StaticSize
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
                  WriteRaw
                  (Require
                    InstanceSpec {
                      instanceSpecStrategy = Nothing,
                      instanceSpecConstraints = []}),
                _×_
                  Storable
                  (Require
                    InstanceSpec {
                      instanceSpecStrategy = Nothing,
                      instanceSpecConstraints = [
                        ]})]}),
      unionGetterConstr = HsName
        "@NsTypeConstr"
        "Data_union_t",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "as_bytes",
          commentLocation = Just
            "doxygen_docs.h:284:13",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Byte array representation"],
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_data_union_t_as_bytes"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = HsName
        "@NsVar"
        "set_data_union_t_as_bytes",
      unionSetterType = HsConstArray
        4
        (HsExtBinding
          ExtHsRef {
            extHsRefModule = HsModuleName
              "HsBindgen.Runtime.Prelude",
            extHsRefIdentifier =
            HsIdentifier "Word8"}
          TypeSpec {
            typeSpecModule = Just
              (HsModuleName
                "HsBindgen.Runtime.Prelude"),
            typeSpecIdentifier = Just
              (HsIdentifier "Word8"),
            typeSpecInstances = Map.fromList
              [
                _×_
                  Eq
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
                  Enum
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
                  Bounded
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
                  Show
                  (Require
                    InstanceSpec {
                      instanceSpecStrategy = Nothing,
                      instanceSpecConstraints = []}),
                _×_
                  Bits
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
                  Num
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
                  StaticSize
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
                  WriteRaw
                  (Require
                    InstanceSpec {
                      instanceSpecStrategy = Nothing,
                      instanceSpecConstraints = []}),
                _×_
                  Storable
                  (Require
                    InstanceSpec {
                      instanceSpecStrategy = Nothing,
                      instanceSpecConstraints = [
                        ]})]}),
      unionSetterConstr = HsName
        "@NsTypeConstr"
        "Data_union_t",
      unionSetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeader = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "get_data_union_t_as_bytes"]]}},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = HsName
        "@NsVar"
        "get_data_union_t_as_parts",
      unionGetterType = HsTypRef
        (HsName
          "@NsTypeConstr"
          "Data_union_t_as_parts"),
      unionGetterConstr = HsName
        "@NsTypeConstr"
        "Data_union_t",
      unionGetterComment = Just
        Comment {
          commentTitle = Just
            [TextContent "As Parts Struct"],
          commentOrigin = Just "as_parts",
          commentLocation = Just
            "doxygen_docs.h:293:30",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_data_union_t_as_parts"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = HsName
        "@NsVar"
        "set_data_union_t_as_parts",
      unionSetterType = HsTypRef
        (HsName
          "@NsTypeConstr"
          "Data_union_t_as_parts"),
      unionSetterConstr = HsName
        "@NsTypeConstr"
        "Data_union_t",
      unionSetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeader = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "get_data_union_t_as_parts"]]}},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Bitfield_t",
      structConstr = HsName
        "@NsConstr"
        "Bitfield_t",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "bitfield_t_flag1",
          fieldType = HsPrimType
            HsPrimCUInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "doxygen_docs.h:303:14",
                fieldName = NamePair {
                  nameC = Name "flag1",
                  nameHsIdent = HsIdentifier
                    "bitfield_t_flag1"},
                fieldComment = Just
                  (Comment
                    [
                      Paragraph [TextContent ""],
                      BlockCommand {
                        blockCommandName = "brief",
                        blockCommandArgs = [],
                        blockCommandParagraph = [
                          TextContent
                            "First flag (1 bit)"]}])},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Unsigned),
              structFieldOffset = 0,
              structFieldWidth = Just 1},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "flag1",
              commentLocation = Just
                "doxygen_docs.h:303:14",
              commentHeader = Just
                "doxygen_docs.h",
              commentChildren = [
                Paragraph
                  [
                    TextContent
                      "First flag (1 bit)"]]}},
        Field {
          fieldName = HsName
            "@NsVar"
            "bitfield_t_flag2",
          fieldType = HsPrimType
            HsPrimCUInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "doxygen_docs.h:304:14",
                fieldName = NamePair {
                  nameC = Name "flag2",
                  nameHsIdent = HsIdentifier
                    "bitfield_t_flag2"},
                fieldComment = Just
                  (Comment
                    [
                      Paragraph [TextContent ""],
                      BlockCommand {
                        blockCommandName = "brief",
                        blockCommandArgs = [],
                        blockCommandParagraph = [
                          TextContent
                            "Second flag (1 bit)"]}])},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Unsigned),
              structFieldOffset = 1,
              structFieldWidth = Just 1},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "flag2",
              commentLocation = Just
                "doxygen_docs.h:304:14",
              commentHeader = Just
                "doxygen_docs.h",
              commentChildren = [
                Paragraph
                  [
                    TextContent
                      "Second flag (1 bit)"]]}},
        Field {
          fieldName = HsName
            "@NsVar"
            "bitfield_t_counter",
          fieldType = HsPrimType
            HsPrimCUInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "doxygen_docs.h:305:14",
                fieldName = NamePair {
                  nameC = Name "counter",
                  nameHsIdent = HsIdentifier
                    "bitfield_t_counter"},
                fieldComment = Just
                  (Comment
                    [
                      Paragraph [TextContent ""],
                      BlockCommand {
                        blockCommandName = "brief",
                        blockCommandArgs = [],
                        blockCommandParagraph = [
                          TextContent
                            "Counter value (6 bits)"]}])},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Unsigned),
              structFieldOffset = 2,
              structFieldWidth = Just 6},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "counter",
              commentLocation = Just
                "doxygen_docs.h:305:14",
              commentHeader = Just
                "doxygen_docs.h",
              commentChildren = [
                Paragraph
                  [
                    TextContent
                      "Counter value (6 bits)"]]}},
        Field {
          fieldName = HsName
            "@NsVar"
            "bitfield_t_reserved",
          fieldType = HsPrimType
            HsPrimCUInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "doxygen_docs.h:306:14",
                fieldName = NamePair {
                  nameC = Name "reserved",
                  nameHsIdent = HsIdentifier
                    "bitfield_t_reserved"},
                fieldComment = Just
                  (Comment
                    [
                      Paragraph [TextContent ""],
                      BlockCommand {
                        blockCommandName = "brief",
                        blockCommandArgs = [],
                        blockCommandParagraph = [
                          TextContent
                            "Reserved bits (24 bits)"]}])},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Unsigned),
              structFieldOffset = 8,
              structFieldWidth = Just 24},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "reserved",
              commentLocation = Just
                "doxygen_docs.h:306:14",
              commentHeader = Just
                "doxygen_docs.h",
              commentChildren = [
                Paragraph
                  [
                    TextContent
                      "Reserved bits (24 bits)"]]}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "doxygen_docs.h:302:9",
            declId = NamePair {
              nameC = Name "bitfield_t",
              nameHsIdent = HsIdentifier
                "Bitfield_t"},
            declOrigin = NameOriginGenerated
              (AnonId "doxygen_docs.h:302:9"),
            declAliases = [
              Name "bitfield_t"],
            declHeader = "doxygen_docs.h",
            declComment = Just
              (Comment
                [
                  Paragraph [TextContent ""],
                  VerbatimLine "bitfield_t",
                  Paragraph [TextContent ""],
                  BlockCommand {
                    blockCommandName = "brief",
                    blockCommandArgs = [],
                    blockCommandParagraph = [
                      TextContent
                        "Bit field structure"]},
                  Paragraph
                    [
                      TextContent
                        "Demonstrates bit field documentation."]])},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Bitfield_t"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:303:14",
                    fieldName = NamePair {
                      nameC = Name "flag1",
                      nameHsIdent = HsIdentifier
                        "bitfield_t_flag1"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent
                                "First flag (1 bit)"]}])},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 0,
                  structFieldWidth = Just 1},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:304:14",
                    fieldName = NamePair {
                      nameC = Name "flag2",
                      nameHsIdent = HsIdentifier
                        "bitfield_t_flag2"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent
                                "Second flag (1 bit)"]}])},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 1,
                  structFieldWidth = Just 1},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:305:14",
                    fieldName = NamePair {
                      nameC = Name "counter",
                      nameHsIdent = HsIdentifier
                        "bitfield_t_counter"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent
                                "Counter value (6 bits)"]}])},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 2,
                  structFieldWidth = Just 6},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:306:14",
                    fieldName = NamePair {
                      nameC = Name "reserved",
                      nameHsIdent = HsIdentifier
                        "bitfield_t_reserved"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent
                                "Reserved bits (24 bits)"]}])},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 8,
                  structFieldWidth = Just 24}],
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
          commentOrigin = Just
            "bitfield_t",
          commentLocation = Just
            "doxygen_docs.h:302:9",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Verbatim "bitfield_t",
            Paragraph
              [
                TextContent
                  "Bit field structure"],
            Paragraph
              [
                TextContent
                  "Demonstrates bit field documentation."]]}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Bitfield_t",
          structConstr = HsName
            "@NsConstr"
            "Bitfield_t",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "bitfield_t_flag1",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:303:14",
                    fieldName = NamePair {
                      nameC = Name "flag1",
                      nameHsIdent = HsIdentifier
                        "bitfield_t_flag1"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent
                                "First flag (1 bit)"]}])},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 0,
                  structFieldWidth = Just 1},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "flag1",
                  commentLocation = Just
                    "doxygen_docs.h:303:14",
                  commentHeader = Just
                    "doxygen_docs.h",
                  commentChildren = [
                    Paragraph
                      [
                        TextContent
                          "First flag (1 bit)"]]}},
            Field {
              fieldName = HsName
                "@NsVar"
                "bitfield_t_flag2",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:304:14",
                    fieldName = NamePair {
                      nameC = Name "flag2",
                      nameHsIdent = HsIdentifier
                        "bitfield_t_flag2"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent
                                "Second flag (1 bit)"]}])},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 1,
                  structFieldWidth = Just 1},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "flag2",
                  commentLocation = Just
                    "doxygen_docs.h:304:14",
                  commentHeader = Just
                    "doxygen_docs.h",
                  commentChildren = [
                    Paragraph
                      [
                        TextContent
                          "Second flag (1 bit)"]]}},
            Field {
              fieldName = HsName
                "@NsVar"
                "bitfield_t_counter",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:305:14",
                    fieldName = NamePair {
                      nameC = Name "counter",
                      nameHsIdent = HsIdentifier
                        "bitfield_t_counter"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent
                                "Counter value (6 bits)"]}])},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 2,
                  structFieldWidth = Just 6},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "counter",
                  commentLocation = Just
                    "doxygen_docs.h:305:14",
                  commentHeader = Just
                    "doxygen_docs.h",
                  commentChildren = [
                    Paragraph
                      [
                        TextContent
                          "Counter value (6 bits)"]]}},
            Field {
              fieldName = HsName
                "@NsVar"
                "bitfield_t_reserved",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:306:14",
                    fieldName = NamePair {
                      nameC = Name "reserved",
                      nameHsIdent = HsIdentifier
                        "bitfield_t_reserved"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent
                                "Reserved bits (24 bits)"]}])},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 8,
                  structFieldWidth = Just 24},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "reserved",
                  commentLocation = Just
                    "doxygen_docs.h:306:14",
                  commentHeader = Just
                    "doxygen_docs.h",
                  commentChildren = [
                    Paragraph
                      [
                        TextContent
                          "Reserved bits (24 bits)"]]}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "doxygen_docs.h:302:9",
                declId = NamePair {
                  nameC = Name "bitfield_t",
                  nameHsIdent = HsIdentifier
                    "Bitfield_t"},
                declOrigin = NameOriginGenerated
                  (AnonId "doxygen_docs.h:302:9"),
                declAliases = [
                  Name "bitfield_t"],
                declHeader = "doxygen_docs.h",
                declComment = Just
                  (Comment
                    [
                      Paragraph [TextContent ""],
                      VerbatimLine "bitfield_t",
                      Paragraph [TextContent ""],
                      BlockCommand {
                        blockCommandName = "brief",
                        blockCommandArgs = [],
                        blockCommandParagraph = [
                          TextContent
                            "Bit field structure"]},
                      Paragraph
                        [
                          TextContent
                            "Demonstrates bit field documentation."]])},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName
                      "@NsConstr"
                      "Bitfield_t"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "doxygen_docs.h:303:14",
                        fieldName = NamePair {
                          nameC = Name "flag1",
                          nameHsIdent = HsIdentifier
                            "bitfield_t_flag1"},
                        fieldComment = Just
                          (Comment
                            [
                              Paragraph [TextContent ""],
                              BlockCommand {
                                blockCommandName = "brief",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent
                                    "First flag (1 bit)"]}])},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Unsigned),
                      structFieldOffset = 0,
                      structFieldWidth = Just 1},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "doxygen_docs.h:304:14",
                        fieldName = NamePair {
                          nameC = Name "flag2",
                          nameHsIdent = HsIdentifier
                            "bitfield_t_flag2"},
                        fieldComment = Just
                          (Comment
                            [
                              Paragraph [TextContent ""],
                              BlockCommand {
                                blockCommandName = "brief",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent
                                    "Second flag (1 bit)"]}])},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Unsigned),
                      structFieldOffset = 1,
                      structFieldWidth = Just 1},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "doxygen_docs.h:305:14",
                        fieldName = NamePair {
                          nameC = Name "counter",
                          nameHsIdent = HsIdentifier
                            "bitfield_t_counter"},
                        fieldComment = Just
                          (Comment
                            [
                              Paragraph [TextContent ""],
                              BlockCommand {
                                blockCommandName = "brief",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent
                                    "Counter value (6 bits)"]}])},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Unsigned),
                      structFieldOffset = 2,
                      structFieldWidth = Just 6},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "doxygen_docs.h:306:14",
                        fieldName = NamePair {
                          nameC = Name "reserved",
                          nameHsIdent = HsIdentifier
                            "bitfield_t_reserved"},
                        fieldComment = Just
                          (Comment
                            [
                              Paragraph [TextContent ""],
                              BlockCommand {
                                blockCommandName = "brief",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent
                                    "Reserved bits (24 bits)"]}])},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Unsigned),
                      structFieldOffset = 8,
                      structFieldWidth = Just 24}],
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
              commentOrigin = Just
                "bitfield_t",
              commentLocation = Just
                "doxygen_docs.h:302:9",
              commentHeader = Just
                "doxygen_docs.h",
              commentChildren = [
                Verbatim "bitfield_t",
                Paragraph
                  [
                    TextContent
                      "Bit field structure"],
                Paragraph
                  [
                    TextContent
                      "Demonstrates bit field documentation."]]}}
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
                    "Bitfield_t",
                  structConstr = HsName
                    "@NsConstr"
                    "Bitfield_t",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bitfield_t_flag1",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:303:14",
                            fieldName = NamePair {
                              nameC = Name "flag1",
                              nameHsIdent = HsIdentifier
                                "bitfield_t_flag1"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent
                                        "First flag (1 bit)"]}])},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Unsigned),
                          structFieldOffset = 0,
                          structFieldWidth = Just 1},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "flag1",
                          commentLocation = Just
                            "doxygen_docs.h:303:14",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [
                                TextContent
                                  "First flag (1 bit)"]]}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bitfield_t_flag2",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:304:14",
                            fieldName = NamePair {
                              nameC = Name "flag2",
                              nameHsIdent = HsIdentifier
                                "bitfield_t_flag2"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent
                                        "Second flag (1 bit)"]}])},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Unsigned),
                          structFieldOffset = 1,
                          structFieldWidth = Just 1},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "flag2",
                          commentLocation = Just
                            "doxygen_docs.h:304:14",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [
                                TextContent
                                  "Second flag (1 bit)"]]}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bitfield_t_counter",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:305:14",
                            fieldName = NamePair {
                              nameC = Name "counter",
                              nameHsIdent = HsIdentifier
                                "bitfield_t_counter"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent
                                        "Counter value (6 bits)"]}])},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Unsigned),
                          structFieldOffset = 2,
                          structFieldWidth = Just 6},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "counter",
                          commentLocation = Just
                            "doxygen_docs.h:305:14",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [
                                TextContent
                                  "Counter value (6 bits)"]]}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bitfield_t_reserved",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:306:14",
                            fieldName = NamePair {
                              nameC = Name "reserved",
                              nameHsIdent = HsIdentifier
                                "bitfield_t_reserved"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent
                                        "Reserved bits (24 bits)"]}])},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Unsigned),
                          structFieldOffset = 8,
                          structFieldWidth = Just 24},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "reserved",
                          commentLocation = Just
                            "doxygen_docs.h:306:14",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [
                                TextContent
                                  "Reserved bits (24 bits)"]]}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "doxygen_docs.h:302:9",
                        declId = NamePair {
                          nameC = Name "bitfield_t",
                          nameHsIdent = HsIdentifier
                            "Bitfield_t"},
                        declOrigin = NameOriginGenerated
                          (AnonId "doxygen_docs.h:302:9"),
                        declAliases = [
                          Name "bitfield_t"],
                        declHeader = "doxygen_docs.h",
                        declComment = Just
                          (Comment
                            [
                              Paragraph [TextContent ""],
                              VerbatimLine "bitfield_t",
                              Paragraph [TextContent ""],
                              BlockCommand {
                                blockCommandName = "brief",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent
                                    "Bit field structure"]},
                              Paragraph
                                [
                                  TextContent
                                    "Demonstrates bit field documentation."]])},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Bitfield_t"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:303:14",
                                fieldName = NamePair {
                                  nameC = Name "flag1",
                                  nameHsIdent = HsIdentifier
                                    "bitfield_t_flag1"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent
                                            "First flag (1 bit)"]}])},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Unsigned),
                              structFieldOffset = 0,
                              structFieldWidth = Just 1},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:304:14",
                                fieldName = NamePair {
                                  nameC = Name "flag2",
                                  nameHsIdent = HsIdentifier
                                    "bitfield_t_flag2"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent
                                            "Second flag (1 bit)"]}])},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Unsigned),
                              structFieldOffset = 1,
                              structFieldWidth = Just 1},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:305:14",
                                fieldName = NamePair {
                                  nameC = Name "counter",
                                  nameHsIdent = HsIdentifier
                                    "bitfield_t_counter"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent
                                            "Counter value (6 bits)"]}])},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Unsigned),
                              structFieldOffset = 2,
                              structFieldWidth = Just 6},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:306:14",
                                fieldName = NamePair {
                                  nameC = Name "reserved",
                                  nameHsIdent = HsIdentifier
                                    "bitfield_t_reserved"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent
                                            "Reserved bits (24 bits)"]}])},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Unsigned),
                              structFieldOffset = 8,
                              structFieldWidth = Just 24}],
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
                      commentOrigin = Just
                        "bitfield_t",
                      commentLocation = Just
                        "doxygen_docs.h:302:9",
                      commentHeader = Just
                        "doxygen_docs.h",
                      commentChildren = [
                        Verbatim "bitfield_t",
                        Paragraph
                          [
                            TextContent
                              "Bit field structure"],
                        Paragraph
                          [
                            TextContent
                              "Demonstrates bit field documentation."]]}})
              [
                PeekBitOffWidth (Idx 0) 0 1,
                PeekBitOffWidth (Idx 0) 1 1,
                PeekBitOffWidth (Idx 0) 2 6,
                PeekBitOffWidth (Idx 0) 8 24]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Bitfield_t",
                  structConstr = HsName
                    "@NsConstr"
                    "Bitfield_t",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bitfield_t_flag1",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:303:14",
                            fieldName = NamePair {
                              nameC = Name "flag1",
                              nameHsIdent = HsIdentifier
                                "bitfield_t_flag1"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent
                                        "First flag (1 bit)"]}])},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Unsigned),
                          structFieldOffset = 0,
                          structFieldWidth = Just 1},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "flag1",
                          commentLocation = Just
                            "doxygen_docs.h:303:14",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [
                                TextContent
                                  "First flag (1 bit)"]]}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bitfield_t_flag2",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:304:14",
                            fieldName = NamePair {
                              nameC = Name "flag2",
                              nameHsIdent = HsIdentifier
                                "bitfield_t_flag2"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent
                                        "Second flag (1 bit)"]}])},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Unsigned),
                          structFieldOffset = 1,
                          structFieldWidth = Just 1},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "flag2",
                          commentLocation = Just
                            "doxygen_docs.h:304:14",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [
                                TextContent
                                  "Second flag (1 bit)"]]}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bitfield_t_counter",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:305:14",
                            fieldName = NamePair {
                              nameC = Name "counter",
                              nameHsIdent = HsIdentifier
                                "bitfield_t_counter"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent
                                        "Counter value (6 bits)"]}])},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Unsigned),
                          structFieldOffset = 2,
                          structFieldWidth = Just 6},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "counter",
                          commentLocation = Just
                            "doxygen_docs.h:305:14",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [
                                TextContent
                                  "Counter value (6 bits)"]]}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bitfield_t_reserved",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:306:14",
                            fieldName = NamePair {
                              nameC = Name "reserved",
                              nameHsIdent = HsIdentifier
                                "bitfield_t_reserved"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent
                                        "Reserved bits (24 bits)"]}])},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Unsigned),
                          structFieldOffset = 8,
                          structFieldWidth = Just 24},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "reserved",
                          commentLocation = Just
                            "doxygen_docs.h:306:14",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [
                                TextContent
                                  "Reserved bits (24 bits)"]]}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "doxygen_docs.h:302:9",
                        declId = NamePair {
                          nameC = Name "bitfield_t",
                          nameHsIdent = HsIdentifier
                            "Bitfield_t"},
                        declOrigin = NameOriginGenerated
                          (AnonId "doxygen_docs.h:302:9"),
                        declAliases = [
                          Name "bitfield_t"],
                        declHeader = "doxygen_docs.h",
                        declComment = Just
                          (Comment
                            [
                              Paragraph [TextContent ""],
                              VerbatimLine "bitfield_t",
                              Paragraph [TextContent ""],
                              BlockCommand {
                                blockCommandName = "brief",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent
                                    "Bit field structure"]},
                              Paragraph
                                [
                                  TextContent
                                    "Demonstrates bit field documentation."]])},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Bitfield_t"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:303:14",
                                fieldName = NamePair {
                                  nameC = Name "flag1",
                                  nameHsIdent = HsIdentifier
                                    "bitfield_t_flag1"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent
                                            "First flag (1 bit)"]}])},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Unsigned),
                              structFieldOffset = 0,
                              structFieldWidth = Just 1},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:304:14",
                                fieldName = NamePair {
                                  nameC = Name "flag2",
                                  nameHsIdent = HsIdentifier
                                    "bitfield_t_flag2"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent
                                            "Second flag (1 bit)"]}])},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Unsigned),
                              structFieldOffset = 1,
                              structFieldWidth = Just 1},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:305:14",
                                fieldName = NamePair {
                                  nameC = Name "counter",
                                  nameHsIdent = HsIdentifier
                                    "bitfield_t_counter"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent
                                            "Counter value (6 bits)"]}])},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Unsigned),
                              structFieldOffset = 2,
                              structFieldWidth = Just 6},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:306:14",
                                fieldName = NamePair {
                                  nameC = Name "reserved",
                                  nameHsIdent = HsIdentifier
                                    "bitfield_t_reserved"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent
                                            "Reserved bits (24 bits)"]}])},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Unsigned),
                              structFieldOffset = 8,
                              structFieldWidth = Just 24}],
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
                      commentOrigin = Just
                        "bitfield_t",
                      commentLocation = Just
                        "doxygen_docs.h:302:9",
                      commentHeader = Just
                        "doxygen_docs.h",
                      commentChildren = [
                        Verbatim "bitfield_t",
                        Paragraph
                          [
                            TextContent
                              "Bit field structure"],
                        Paragraph
                          [
                            TextContent
                              "Demonstrates bit field documentation."]]}}
                (Add 4)
                (Seq
                  [
                    PokeBitOffWidth
                      (Idx 5)
                      0
                      1
                      (Idx 0),
                    PokeBitOffWidth
                      (Idx 5)
                      1
                      1
                      (Idx 1),
                    PokeBitOffWidth
                      (Idx 5)
                      2
                      6
                      (Idx 2),
                    PokeBitOffWidth
                      (Idx 5)
                      8
                      24
                      (Idx 3)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Bitfield_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Bitfield_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Processor_fn_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Processor_fn_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Processor_fn_t",
        fieldType = HsFunPtr
          (HsFun
            (HsPrimType HsPrimCInt)
            (HsFun
              (HsPtr (HsPrimType HsPrimVoid))
              (HsIO
                (HsPrimType HsPrimCInt)))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "doxygen_docs.h:317:15",
          declId = NamePair {
            nameC = Name "processor_fn_t",
            nameHsIdent = HsIdentifier
              "Processor_fn_t"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "doxygen_docs.h",
          declComment = Just
            (Comment
              [
                Paragraph [TextContent ""],
                VerbatimLine "processor_fn_t",
                Paragraph [TextContent ""],
                BlockCommand {
                  blockCommandName = "brief",
                  blockCommandArgs = [],
                  blockCommandParagraph = [
                    TextContent
                      "Function pointer typedef"]},
                Paragraph [TextContent ""],
                ParamCommand {
                  paramCommandName = "input",
                  paramCommandIndex = Just 0,
                  paramCommandDirection = Just
                    CXCommentParamPassDirection_In,
                  paramCommandIsDirectionExplicit =
                  False,
                  paramCommandContent = [
                    Paragraph
                      [
                        TextContent "Input value",
                        TextContent ""]]},
                ParamCommand {
                  paramCommandName = "context",
                  paramCommandIndex = Just 1,
                  paramCommandDirection = Just
                    CXCommentParamPassDirection_In,
                  paramCommandIsDirectionExplicit =
                  False,
                  paramCommandContent = [
                    Paragraph
                      [
                        TextContent "Context pointer",
                        TextContent ""]]},
                BlockCommand {
                  blockCommandName = "return",
                  blockCommandArgs = [],
                  blockCommandParagraph = [
                    TextContent
                      "Processed value"]}])},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Processor_fn_t",
              newtypeField = HsName
                "@NsVar"
                "un_Processor_fn_t"},
            typedefType = TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed),
                  TypePointer TypeVoid]
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "processor_fn_t",
          commentLocation = Just
            "doxygen_docs.h:317:15",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Verbatim "processor_fn_t",
            Paragraph
              [
                TextContent
                  "Function pointer typedef"],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "input"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [TextContent "Input value"]]},
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace
                    [TextContent "context"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Context pointer"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent
                  "Processed value"]]}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Processor_fn_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Processor_fn_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Processor_fn_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Processor_fn_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Filename_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Filename_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Filename_t",
        fieldType = HsConstArray
          256
          (HsPrimType HsPrimCChar),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "doxygen_docs.h:323:14",
          declId = NamePair {
            nameC = Name "filename_t",
            nameHsIdent = HsIdentifier
              "Filename_t"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "doxygen_docs.h",
          declComment = Just
            (Comment
              [
                Paragraph [TextContent ""],
                VerbatimLine "filename_t",
                Paragraph [TextContent ""],
                BlockCommand {
                  blockCommandName = "brief",
                  blockCommandArgs = [],
                  blockCommandParagraph = [
                    TextContent
                      "Array typedef with size"]}])},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Filename_t",
              newtypeField = HsName
                "@NsVar"
                "un_Filename_t"},
            typedefType = TypeConstArray
              256
              (TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "filename_t",
          commentLocation = Just
            "doxygen_docs.h:323:14",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Verbatim "filename_t",
            Paragraph
              [
                TextContent
                  "Array typedef with size"]]}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Filename_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Filename_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Filename_t",
      deriveInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "process_buffer_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "buffer"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCChar),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "buffer",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = [
                DefinitionList {
                  definitionListTerm = Bold
                    [
                      Monospace
                        [TextContent "buffer"],
                      Emph [TextContent "(input)"]],
                  definitionListContent = [
                    Paragraph
                      [
                        TextContent
                          "Buffer with minimum size"]]}]}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "size"),
          functionParameterType =
          HsExtBinding
            ExtHsRef {
              extHsRefModule = HsModuleName
                "HsBindgen.Runtime.Prelude",
              extHsRefIdentifier =
              HsIdentifier "CSize"}
            TypeSpec {
              typeSpecModule = Just
                (HsModuleName
                  "HsBindgen.Runtime.Prelude"),
              typeSpecIdentifier = Just
                (HsIdentifier "CSize"),
              typeSpecInstances = Map.fromList
                [
                  _×_
                    Eq
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
                    Enum
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
                    Bounded
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
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bits
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
                    Num
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
                    StaticSize
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
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]},
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "size",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = [
                DefinitionList {
                  definitionListTerm = Bold
                    [
                      Monospace [TextContent "size"],
                      Emph [TextContent "(input)"]],
                  definitionListContent = [
                    Paragraph
                      [
                        TextContent
                          "Actual buffer size"]]}]}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_1ce818d0ed47ea3e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_doxygen_docs_1ce818d0ed47ea3e (char *arg1, size_t arg2) { return process_buffer(arg1, arg2); }",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "buffer",
                  nameHsIdent = HsIdentifier
                    "buffer"})
              (TypeConstArray
                64
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "size",
                  nameHsIdent = HsIdentifier
                    "size"})
              (TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "size_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "CSize"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "CSize"),
                    typeSpecInstances = Map.fromList
                      [
                        _×_
                          Eq
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
                          Enum
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
                          Bounded
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
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bits
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
                          Num
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
                          StaticSize
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
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}})],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "process_buffer",
          commentLocation = Just
            "doxygen_docs.h:332:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Static array parameter"],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace
                    [TextContent "buffer"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Buffer with minimum size"]]},
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "size"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Actual buffer size"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent
                  "Number of bytes written"]]},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_doxygen_docs_fdf2d8f3abafc975",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsConstArray
                64
                (HsPrimType HsPrimCChar))
              (HsFun
                (HsExtBinding
                  ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "CSize"}
                  TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "CSize"),
                    typeSpecInstances = Map.fromList
                      [
                        _×_
                          Eq
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
                          Enum
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
                          Bounded
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
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bits
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
                          Num
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
                          StaticSize
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
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]})
                (HsIO
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_fdf2d8f3abafc975",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_process_buffer_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_doxygen_docs_fdf2d8f3abafc975 (void)) (char arg1[64], size_t arg2) { return &process_buffer; } ",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeConstArray
              64
              (TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed)))),
            TypeExtBinding
              ResolvedExtBinding {
                extCName = QualName {
                  qualNameName = Name "size_t",
                  qualNameKind =
                  NameKindOrdinary},
                extHsRef = ExtHsRef {
                  extHsRefModule = HsModuleName
                    "HsBindgen.Runtime.Prelude",
                  extHsRefIdentifier =
                  HsIdentifier "CSize"},
                extHsSpec = TypeSpec {
                  typeSpecModule = Just
                    (HsModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (HsIdentifier "CSize"),
                  typeSpecInstances = Map.fromList
                    [
                      _×_
                        Eq
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
                        Enum
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
                        Bounded
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
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bits
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
                        Num
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
                        StaticSize
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
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]}}]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "process_buffer",
          commentLocation = Just
            "doxygen_docs.h:332:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Static array parameter"],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace
                    [TextContent "buffer"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Buffer with minimum size"]]},
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "size"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Actual buffer size"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent
                  "Number of bytes written"]]},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "my_memcpy",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "dest"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimVoid),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "dest",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = [
                DefinitionList {
                  definitionListTerm = Bold
                    [
                      Monospace [TextContent "dest"],
                      Emph [TextContent "(input)"]],
                  definitionListContent = [
                    Paragraph
                      [
                        TextContent
                          "Destination buffer (restrict)"]]}]}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "src"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimVoid),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "src",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = [
                DefinitionList {
                  definitionListTerm = Bold
                    [
                      Monospace [TextContent "src"],
                      Emph [TextContent "(input)"]],
                  definitionListContent = [
                    Paragraph
                      [
                        TextContent
                          "Source buffer (restrict)"]]}]}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "n"),
          functionParameterType =
          HsExtBinding
            ExtHsRef {
              extHsRefModule = HsModuleName
                "HsBindgen.Runtime.Prelude",
              extHsRefIdentifier =
              HsIdentifier "CSize"}
            TypeSpec {
              typeSpecModule = Just
                (HsModuleName
                  "HsBindgen.Runtime.Prelude"),
              typeSpecIdentifier = Just
                (HsIdentifier "CSize"),
              typeSpecInstances = Map.fromList
                [
                  _×_
                    Eq
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
                    Enum
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
                    Bounded
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
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bits
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
                    Num
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
                    StaticSize
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
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]},
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "n",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = [
                DefinitionList {
                  definitionListTerm = Bold
                    [
                      Monospace [TextContent "n"],
                      Emph [TextContent "(input)"]],
                  definitionListContent = [
                    Paragraph
                      [
                        TextContent
                          "Number of bytes"]]}]}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_bcbe640b60445a4f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_doxygen_docs_bcbe640b60445a4f (void *arg1, void const *arg2, size_t arg3) { return my_memcpy(arg1, arg2, arg3); }",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "dest",
                  nameHsIdent = HsIdentifier
                    "dest"})
              (TypePointer TypeVoid),
            _×_
              (Just
                NamePair {
                  nameC = Name "src",
                  nameHsIdent = HsIdentifier
                    "src"})
              (TypePointer
                (TypeConst TypeVoid)),
            _×_
              (Just
                NamePair {
                  nameC = Name "n",
                  nameHsIdent = HsIdentifier "n"})
              (TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "size_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "CSize"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "CSize"),
                    typeSpecInstances = Map.fromList
                      [
                        _×_
                          Eq
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
                          Enum
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
                          Bounded
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
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bits
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
                          Num
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
                          StaticSize
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
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}})],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_memcpy",
          commentLocation = Just
            "doxygen_docs.h:342:7",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Function with restrict pointers"],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "dest"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Destination buffer (restrict)"]]},
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "src"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Source buffer (restrict)"]]},
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "n"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Number of bytes"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent
                  "Destination pointer"]]},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_doxygen_docs_de9d3228e8bac25c",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr (HsPrimType HsPrimVoid))
              (HsFun
                (HsPtr (HsPrimType HsPrimVoid))
                (HsFun
                  (HsExtBinding
                    ExtHsRef {
                      extHsRefModule = HsModuleName
                        "HsBindgen.Runtime.Prelude",
                      extHsRefIdentifier =
                      HsIdentifier "CSize"}
                    TypeSpec {
                      typeSpecModule = Just
                        (HsModuleName
                          "HsBindgen.Runtime.Prelude"),
                      typeSpecIdentifier = Just
                        (HsIdentifier "CSize"),
                      typeSpecInstances = Map.fromList
                        [
                          _×_
                            Eq
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
                            Enum
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
                            Bounded
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
                            Show
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bits
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
                            Num
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
                            StaticSize
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
                            WriteRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Storable
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = [
                                  ]})]})
                  (HsIO
                    (HsPtr
                      (HsPrimType HsPrimVoid)))))))),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_de9d3228e8bac25c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_my_memcpy_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_doxygen_docs_de9d3228e8bac25c (void)) (void *arg1, void const *arg2, size_t arg3) { return &my_memcpy; } ",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer TypeVoid,
            TypePointer
              (TypeConst TypeVoid),
            TypeExtBinding
              ResolvedExtBinding {
                extCName = QualName {
                  qualNameName = Name "size_t",
                  qualNameKind =
                  NameKindOrdinary},
                extHsRef = ExtHsRef {
                  extHsRefModule = HsModuleName
                    "HsBindgen.Runtime.Prelude",
                  extHsRefIdentifier =
                  HsIdentifier "CSize"},
                extHsSpec = TypeSpec {
                  typeSpecModule = Just
                    (HsModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (HsIdentifier "CSize"),
                  typeSpecInstances = Map.fromList
                    [
                      _×_
                        Eq
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
                        Enum
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
                        Bounded
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
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bits
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
                        Num
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
                        StaticSize
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
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]}}]
          (TypePointer TypeVoid)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_memcpy",
          commentLocation = Just
            "doxygen_docs.h:342:7",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Function with restrict pointers"],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "dest"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Destination buffer (restrict)"]]},
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "src"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Source buffer (restrict)"]]},
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "n"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Number of bytes"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent
                  "Destination pointer"]]},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "double_value",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = [
                DefinitionList {
                  definitionListTerm = Bold
                    [
                      Monospace [TextContent "x"],
                      Emph [TextContent "(input)"]],
                  definitionListContent = [
                    Paragraph
                      [
                        TextContent
                          "Input value"]]}]}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_1c952f01cc07bb64",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_doxygen_docs_1c952f01cc07bb64 (signed int arg1) { return double_value(arg1); }",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
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
          commentOrigin = Just
            "double_value",
          commentLocation = Just
            "doxygen_docs.h:350:19",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [TextContent "Inline function"],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "x"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [TextContent "Input value"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent "Doubled value"]]},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_doxygen_docs_faec60e7f49d446c",
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
      "hs_bindgen_test_doxygen_docs_faec60e7f49d446c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_double_value_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_doxygen_docs_faec60e7f49d446c (void)) (signed int arg1) { return &double_value; } ",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed)]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "double_value",
          commentLocation = Just
            "doxygen_docs.h:350:19",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [TextContent "Inline function"],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "x"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [TextContent "Input value"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent "Doubled value"]]},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Flexible_array",
      structConstr = HsName
        "@NsConstr"
        "Flexible_array",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "flexible_array_count",
          fieldType = HsExtBinding
            ExtHsRef {
              extHsRefModule = HsModuleName
                "HsBindgen.Runtime.Prelude",
              extHsRefIdentifier =
              HsIdentifier "CSize"}
            TypeSpec {
              typeSpecModule = Just
                (HsModuleName
                  "HsBindgen.Runtime.Prelude"),
              typeSpecIdentifier = Just
                (HsIdentifier "CSize"),
              typeSpecInstances = Map.fromList
                [
                  _×_
                    Eq
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
                    Enum
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
                    Bounded
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
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bits
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
                    Num
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
                    StaticSize
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
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]},
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "doxygen_docs.h:361:12",
                fieldName = NamePair {
                  nameC = Name "count",
                  nameHsIdent = HsIdentifier
                    "flexible_array_count"},
                fieldComment = Just
                  (Comment
                    [
                      Paragraph [TextContent ""],
                      BlockCommand {
                        blockCommandName = "brief",
                        blockCommandArgs = [],
                        blockCommandParagraph = [
                          TextContent
                            "Number of elements"]}])},
              structFieldType = TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "size_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "CSize"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "CSize"),
                    typeSpecInstances = Map.fromList
                      [
                        _×_
                          Eq
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
                          Enum
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
                          Bounded
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
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bits
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
                          Num
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
                          StaticSize
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
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}},
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "count",
              commentLocation = Just
                "doxygen_docs.h:361:12",
              commentHeader = Just
                "doxygen_docs.h",
              commentChildren = [
                Paragraph
                  [
                    TextContent
                      "Number of elements"]]}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "doxygen_docs.h:360:8",
            declId = NamePair {
              nameC = Name "flexible_array",
              nameHsIdent = HsIdentifier
                "Flexible_array"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "doxygen_docs.h",
            declComment = Just
              (Comment
                [
                  Paragraph [TextContent ""],
                  BlockCommand {
                    blockCommandName = "brief",
                    blockCommandArgs = [],
                    blockCommandParagraph = [
                      TextContent
                        "Function with flexible array member"]},
                  Paragraph [TextContent ""],
                  ParamCommand {
                    paramCommandName = "count",
                    paramCommandIndex = Nothing,
                    paramCommandDirection = Just
                      CXCommentParamPassDirection_In,
                    paramCommandIsDirectionExplicit =
                    False,
                    paramCommandContent = [
                      Paragraph
                        [
                          TextContent
                            "Number of elements",
                          TextContent ""]]},
                  BlockCommand {
                    blockCommandName = "return",
                    blockCommandArgs = [],
                    blockCommandParagraph = [
                      TextContent
                        "Allocated structure"]}])},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Flexible_array"),
              structSizeof = 8,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:361:12",
                    fieldName = NamePair {
                      nameC = Name "count",
                      nameHsIdent = HsIdentifier
                        "flexible_array_count"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent
                                "Number of elements"]}])},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "size_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "CSize"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "CSize"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
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
                              Enum
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
                              Bounded
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
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
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
                              Num
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
                              StaticSize
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
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 0,
                  structFieldWidth = Nothing}],
              structFlam = Just
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:362:9",
                    fieldName = NamePair {
                      nameC = Name "data",
                      nameHsIdent = HsIdentifier
                        "flexible_array_data"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent
                                "Flexible array member"]}])},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 64,
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
          commentOrigin = Just
            "flexible_array",
          commentLocation = Just
            "doxygen_docs.h:360:8",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Function with flexible array member"],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "count"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Number of elements"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent
                  "Allocated structure"]]}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Flexible_array",
          structConstr = HsName
            "@NsConstr"
            "Flexible_array",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "flexible_array_count",
              fieldType = HsExtBinding
                ExtHsRef {
                  extHsRefModule = HsModuleName
                    "HsBindgen.Runtime.Prelude",
                  extHsRefIdentifier =
                  HsIdentifier "CSize"}
                TypeSpec {
                  typeSpecModule = Just
                    (HsModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (HsIdentifier "CSize"),
                  typeSpecInstances = Map.fromList
                    [
                      _×_
                        Eq
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
                        Enum
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
                        Bounded
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
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bits
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
                        Num
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
                        StaticSize
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
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]},
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:361:12",
                    fieldName = NamePair {
                      nameC = Name "count",
                      nameHsIdent = HsIdentifier
                        "flexible_array_count"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent
                                "Number of elements"]}])},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "size_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "CSize"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "CSize"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
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
                              Enum
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
                              Bounded
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
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
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
                              Num
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
                              StaticSize
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
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "count",
                  commentLocation = Just
                    "doxygen_docs.h:361:12",
                  commentHeader = Just
                    "doxygen_docs.h",
                  commentChildren = [
                    Paragraph
                      [
                        TextContent
                          "Number of elements"]]}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "doxygen_docs.h:360:8",
                declId = NamePair {
                  nameC = Name "flexible_array",
                  nameHsIdent = HsIdentifier
                    "Flexible_array"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "doxygen_docs.h",
                declComment = Just
                  (Comment
                    [
                      Paragraph [TextContent ""],
                      BlockCommand {
                        blockCommandName = "brief",
                        blockCommandArgs = [],
                        blockCommandParagraph = [
                          TextContent
                            "Function with flexible array member"]},
                      Paragraph [TextContent ""],
                      ParamCommand {
                        paramCommandName = "count",
                        paramCommandIndex = Nothing,
                        paramCommandDirection = Just
                          CXCommentParamPassDirection_In,
                        paramCommandIsDirectionExplicit =
                        False,
                        paramCommandContent = [
                          Paragraph
                            [
                              TextContent
                                "Number of elements",
                              TextContent ""]]},
                      BlockCommand {
                        blockCommandName = "return",
                        blockCommandArgs = [],
                        blockCommandParagraph = [
                          TextContent
                            "Allocated structure"]}])},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName
                      "@NsConstr"
                      "Flexible_array"),
                  structSizeof = 8,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "doxygen_docs.h:361:12",
                        fieldName = NamePair {
                          nameC = Name "count",
                          nameHsIdent = HsIdentifier
                            "flexible_array_count"},
                        fieldComment = Just
                          (Comment
                            [
                              Paragraph [TextContent ""],
                              BlockCommand {
                                blockCommandName = "brief",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent
                                    "Number of elements"]}])},
                      structFieldType = TypeExtBinding
                        ResolvedExtBinding {
                          extCName = QualName {
                            qualNameName = Name "size_t",
                            qualNameKind =
                            NameKindOrdinary},
                          extHsRef = ExtHsRef {
                            extHsRefModule = HsModuleName
                              "HsBindgen.Runtime.Prelude",
                            extHsRefIdentifier =
                            HsIdentifier "CSize"},
                          extHsSpec = TypeSpec {
                            typeSpecModule = Just
                              (HsModuleName
                                "HsBindgen.Runtime.Prelude"),
                            typeSpecIdentifier = Just
                              (HsIdentifier "CSize"),
                            typeSpecInstances = Map.fromList
                              [
                                _×_
                                  Eq
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
                                  Enum
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
                                  Bounded
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
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bits
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
                                  Num
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
                                  StaticSize
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
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]}},
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Just
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "doxygen_docs.h:362:9",
                        fieldName = NamePair {
                          nameC = Name "data",
                          nameHsIdent = HsIdentifier
                            "flexible_array_data"},
                        fieldComment = Just
                          (Comment
                            [
                              Paragraph [TextContent ""],
                              BlockCommand {
                                blockCommandName = "brief",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent
                                    "Flexible array member"]}])},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 64,
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
              commentOrigin = Just
                "flexible_array",
              commentLocation = Just
                "doxygen_docs.h:360:8",
              commentHeader = Just
                "doxygen_docs.h",
              commentChildren = [
                Paragraph
                  [
                    TextContent
                      "Function with flexible array member"],
                DefinitionList {
                  definitionListTerm = Bold
                    [
                      Monospace [TextContent "count"],
                      Emph [TextContent "(input)"]],
                  definitionListContent = [
                    Paragraph
                      [
                        TextContent
                          "Number of elements"]]},
                Paragraph
                  [
                    Bold [TextContent "returns:"],
                    TextContent
                      "Allocated structure"]]}}
        StorableInstance {
          storableSizeOf = 8,
          storableAlignment = 8,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Flexible_array",
                  structConstr = HsName
                    "@NsConstr"
                    "Flexible_array",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "flexible_array_count",
                      fieldType = HsExtBinding
                        ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "CSize"}
                        TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "CSize"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
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
                                Enum
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
                                Bounded
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
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
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
                                Num
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
                                StaticSize
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
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:361:12",
                            fieldName = NamePair {
                              nameC = Name "count",
                              nameHsIdent = HsIdentifier
                                "flexible_array_count"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent
                                        "Number of elements"]}])},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "size_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtHsRef {
                                extHsRefModule = HsModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extHsRefIdentifier =
                                HsIdentifier "CSize"},
                              extHsSpec = TypeSpec {
                                typeSpecModule = Just
                                  (HsModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                typeSpecIdentifier = Just
                                  (HsIdentifier "CSize"),
                                typeSpecInstances = Map.fromList
                                  [
                                    _×_
                                      Eq
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
                                      Enum
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
                                      Bounded
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
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bits
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
                                      Num
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
                                      StaticSize
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
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}},
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "count",
                          commentLocation = Just
                            "doxygen_docs.h:361:12",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [
                                TextContent
                                  "Number of elements"]]}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "doxygen_docs.h:360:8",
                        declId = NamePair {
                          nameC = Name "flexible_array",
                          nameHsIdent = HsIdentifier
                            "Flexible_array"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "doxygen_docs.h",
                        declComment = Just
                          (Comment
                            [
                              Paragraph [TextContent ""],
                              BlockCommand {
                                blockCommandName = "brief",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent
                                    "Function with flexible array member"]},
                              Paragraph [TextContent ""],
                              ParamCommand {
                                paramCommandName = "count",
                                paramCommandIndex = Nothing,
                                paramCommandDirection = Just
                                  CXCommentParamPassDirection_In,
                                paramCommandIsDirectionExplicit =
                                False,
                                paramCommandContent = [
                                  Paragraph
                                    [
                                      TextContent
                                        "Number of elements",
                                      TextContent ""]]},
                              BlockCommand {
                                blockCommandName = "return",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent
                                    "Allocated structure"]}])},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Flexible_array"),
                          structSizeof = 8,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:361:12",
                                fieldName = NamePair {
                                  nameC = Name "count",
                                  nameHsIdent = HsIdentifier
                                    "flexible_array_count"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent
                                            "Number of elements"]}])},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "size_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtHsRef {
                                    extHsRefModule = HsModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extHsRefIdentifier =
                                    HsIdentifier "CSize"},
                                  extHsSpec = TypeSpec {
                                    typeSpecModule = Just
                                      (HsModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    typeSpecIdentifier = Just
                                      (HsIdentifier "CSize"),
                                    typeSpecInstances = Map.fromList
                                      [
                                        _×_
                                          Eq
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
                                          Enum
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
                                          Bounded
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
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bits
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
                                          Num
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
                                          StaticSize
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
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Just
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:362:9",
                                fieldName = NamePair {
                                  nameC = Name "data",
                                  nameHsIdent = HsIdentifier
                                    "flexible_array_data"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent
                                            "Flexible array member"]}])},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 64,
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
                      commentOrigin = Just
                        "flexible_array",
                      commentLocation = Just
                        "doxygen_docs.h:360:8",
                      commentHeader = Just
                        "doxygen_docs.h",
                      commentChildren = [
                        Paragraph
                          [
                            TextContent
                              "Function with flexible array member"],
                        DefinitionList {
                          definitionListTerm = Bold
                            [
                              Monospace [TextContent "count"],
                              Emph [TextContent "(input)"]],
                          definitionListContent = [
                            Paragraph
                              [
                                TextContent
                                  "Number of elements"]]},
                        Paragraph
                          [
                            Bold [TextContent "returns:"],
                            TextContent
                              "Allocated structure"]]}})
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
                    "Flexible_array",
                  structConstr = HsName
                    "@NsConstr"
                    "Flexible_array",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "flexible_array_count",
                      fieldType = HsExtBinding
                        ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "CSize"}
                        TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "CSize"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
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
                                Enum
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
                                Bounded
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
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
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
                                Num
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
                                StaticSize
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
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "doxygen_docs.h:361:12",
                            fieldName = NamePair {
                              nameC = Name "count",
                              nameHsIdent = HsIdentifier
                                "flexible_array_count"},
                            fieldComment = Just
                              (Comment
                                [
                                  Paragraph [TextContent ""],
                                  BlockCommand {
                                    blockCommandName = "brief",
                                    blockCommandArgs = [],
                                    blockCommandParagraph = [
                                      TextContent
                                        "Number of elements"]}])},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "size_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtHsRef {
                                extHsRefModule = HsModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extHsRefIdentifier =
                                HsIdentifier "CSize"},
                              extHsSpec = TypeSpec {
                                typeSpecModule = Just
                                  (HsModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                typeSpecIdentifier = Just
                                  (HsIdentifier "CSize"),
                                typeSpecInstances = Map.fromList
                                  [
                                    _×_
                                      Eq
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
                                      Enum
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
                                      Bounded
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
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bits
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
                                      Num
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
                                      StaticSize
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
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}},
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "count",
                          commentLocation = Just
                            "doxygen_docs.h:361:12",
                          commentHeader = Just
                            "doxygen_docs.h",
                          commentChildren = [
                            Paragraph
                              [
                                TextContent
                                  "Number of elements"]]}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "doxygen_docs.h:360:8",
                        declId = NamePair {
                          nameC = Name "flexible_array",
                          nameHsIdent = HsIdentifier
                            "Flexible_array"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "doxygen_docs.h",
                        declComment = Just
                          (Comment
                            [
                              Paragraph [TextContent ""],
                              BlockCommand {
                                blockCommandName = "brief",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent
                                    "Function with flexible array member"]},
                              Paragraph [TextContent ""],
                              ParamCommand {
                                paramCommandName = "count",
                                paramCommandIndex = Nothing,
                                paramCommandDirection = Just
                                  CXCommentParamPassDirection_In,
                                paramCommandIsDirectionExplicit =
                                False,
                                paramCommandContent = [
                                  Paragraph
                                    [
                                      TextContent
                                        "Number of elements",
                                      TextContent ""]]},
                              BlockCommand {
                                blockCommandName = "return",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent
                                    "Allocated structure"]}])},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Flexible_array"),
                          structSizeof = 8,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:361:12",
                                fieldName = NamePair {
                                  nameC = Name "count",
                                  nameHsIdent = HsIdentifier
                                    "flexible_array_count"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent
                                            "Number of elements"]}])},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "size_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtHsRef {
                                    extHsRefModule = HsModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extHsRefIdentifier =
                                    HsIdentifier "CSize"},
                                  extHsSpec = TypeSpec {
                                    typeSpecModule = Just
                                      (HsModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    typeSpecIdentifier = Just
                                      (HsIdentifier "CSize"),
                                    typeSpecInstances = Map.fromList
                                      [
                                        _×_
                                          Eq
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
                                          Enum
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
                                          Bounded
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
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bits
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
                                          Num
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
                                          StaticSize
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
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Just
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "doxygen_docs.h:362:9",
                                fieldName = NamePair {
                                  nameC = Name "data",
                                  nameHsIdent = HsIdentifier
                                    "flexible_array_data"},
                                fieldComment = Just
                                  (Comment
                                    [
                                      Paragraph [TextContent ""],
                                      BlockCommand {
                                        blockCommandName = "brief",
                                        blockCommandArgs = [],
                                        blockCommandParagraph = [
                                          TextContent
                                            "Flexible array member"]}])},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 64,
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
                      commentOrigin = Just
                        "flexible_array",
                      commentLocation = Just
                        "doxygen_docs.h:360:8",
                      commentHeader = Just
                        "doxygen_docs.h",
                      commentChildren = [
                        Paragraph
                          [
                            TextContent
                              "Function with flexible array member"],
                        DefinitionList {
                          definitionListTerm = Bold
                            [
                              Monospace [TextContent "count"],
                              Emph [TextContent "(input)"]],
                          definitionListContent = [
                            Paragraph
                              [
                                TextContent
                                  "Number of elements"]]},
                        Paragraph
                          [
                            Bold [TextContent "returns:"],
                            TextContent
                              "Allocated structure"]]}}
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
        "Flexible_array",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Flexible_array",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasFLAM
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Flexible_array",
          structConstr = HsName
            "@NsConstr"
            "Flexible_array",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "flexible_array_count",
              fieldType = HsExtBinding
                ExtHsRef {
                  extHsRefModule = HsModuleName
                    "HsBindgen.Runtime.Prelude",
                  extHsRefIdentifier =
                  HsIdentifier "CSize"}
                TypeSpec {
                  typeSpecModule = Just
                    (HsModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (HsIdentifier "CSize"),
                  typeSpecInstances = Map.fromList
                    [
                      _×_
                        Eq
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
                        Enum
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
                        Bounded
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
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bits
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
                        Num
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
                        StaticSize
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
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]},
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "doxygen_docs.h:361:12",
                    fieldName = NamePair {
                      nameC = Name "count",
                      nameHsIdent = HsIdentifier
                        "flexible_array_count"},
                    fieldComment = Just
                      (Comment
                        [
                          Paragraph [TextContent ""],
                          BlockCommand {
                            blockCommandName = "brief",
                            blockCommandArgs = [],
                            blockCommandParagraph = [
                              TextContent
                                "Number of elements"]}])},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "size_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "CSize"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "CSize"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
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
                              Enum
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
                              Bounded
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
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
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
                              Num
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
                              StaticSize
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
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "count",
                  commentLocation = Just
                    "doxygen_docs.h:361:12",
                  commentHeader = Just
                    "doxygen_docs.h",
                  commentChildren = [
                    Paragraph
                      [
                        TextContent
                          "Number of elements"]]}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "doxygen_docs.h:360:8",
                declId = NamePair {
                  nameC = Name "flexible_array",
                  nameHsIdent = HsIdentifier
                    "Flexible_array"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "doxygen_docs.h",
                declComment = Just
                  (Comment
                    [
                      Paragraph [TextContent ""],
                      BlockCommand {
                        blockCommandName = "brief",
                        blockCommandArgs = [],
                        blockCommandParagraph = [
                          TextContent
                            "Function with flexible array member"]},
                      Paragraph [TextContent ""],
                      ParamCommand {
                        paramCommandName = "count",
                        paramCommandIndex = Nothing,
                        paramCommandDirection = Just
                          CXCommentParamPassDirection_In,
                        paramCommandIsDirectionExplicit =
                        False,
                        paramCommandContent = [
                          Paragraph
                            [
                              TextContent
                                "Number of elements",
                              TextContent ""]]},
                      BlockCommand {
                        blockCommandName = "return",
                        blockCommandArgs = [],
                        blockCommandParagraph = [
                          TextContent
                            "Allocated structure"]}])},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName
                      "@NsConstr"
                      "Flexible_array"),
                  structSizeof = 8,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "doxygen_docs.h:361:12",
                        fieldName = NamePair {
                          nameC = Name "count",
                          nameHsIdent = HsIdentifier
                            "flexible_array_count"},
                        fieldComment = Just
                          (Comment
                            [
                              Paragraph [TextContent ""],
                              BlockCommand {
                                blockCommandName = "brief",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent
                                    "Number of elements"]}])},
                      structFieldType = TypeExtBinding
                        ResolvedExtBinding {
                          extCName = QualName {
                            qualNameName = Name "size_t",
                            qualNameKind =
                            NameKindOrdinary},
                          extHsRef = ExtHsRef {
                            extHsRefModule = HsModuleName
                              "HsBindgen.Runtime.Prelude",
                            extHsRefIdentifier =
                            HsIdentifier "CSize"},
                          extHsSpec = TypeSpec {
                            typeSpecModule = Just
                              (HsModuleName
                                "HsBindgen.Runtime.Prelude"),
                            typeSpecIdentifier = Just
                              (HsIdentifier "CSize"),
                            typeSpecInstances = Map.fromList
                              [
                                _×_
                                  Eq
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
                                  Enum
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
                                  Bounded
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
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bits
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
                                  Num
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
                                  StaticSize
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
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]}},
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Just
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "doxygen_docs.h:362:9",
                        fieldName = NamePair {
                          nameC = Name "data",
                          nameHsIdent = HsIdentifier
                            "flexible_array_data"},
                        fieldComment = Just
                          (Comment
                            [
                              Paragraph [TextContent ""],
                              BlockCommand {
                                blockCommandName = "brief",
                                blockCommandArgs = [],
                                blockCommandParagraph = [
                                  TextContent
                                    "Flexible array member"]}])},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 64,
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
              commentOrigin = Just
                "flexible_array",
              commentLocation = Just
                "doxygen_docs.h:360:8",
              commentHeader = Just
                "doxygen_docs.h",
              commentChildren = [
                Paragraph
                  [
                    TextContent
                      "Function with flexible array member"],
                DefinitionList {
                  definitionListTerm = Bold
                    [
                      Monospace [TextContent "count"],
                      Emph [TextContent "(input)"]],
                  definitionListContent = [
                    Paragraph
                      [
                        TextContent
                          "Number of elements"]]},
                Paragraph
                  [
                    Bold [TextContent "returns:"],
                    TextContent
                      "Allocated structure"]]}}
        (HsPrimType HsPrimCInt)
        8,
      defineInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "complex_function",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "config"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Config_t")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "config",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = [
                DefinitionList {
                  definitionListTerm = Bold
                    [
                      Monospace
                        [TextContent "config"],
                      Emph [TextContent "(input)"]],
                  definitionListContent = [
                    Paragraph
                      [
                        TextContent
                          "Configuration structure (see",
                        Identifier "Config_t",
                        TextContent ")"]]}]}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "data'"),
          functionParameterType = HsPtr
            (HsExtBinding
              ExtHsRef {
                extHsRefModule = HsModuleName
                  "HsBindgen.Runtime.Prelude",
                extHsRefIdentifier =
                HsIdentifier "Word8"}
              TypeSpec {
                typeSpecModule = Just
                  (HsModuleName
                    "HsBindgen.Runtime.Prelude"),
                typeSpecIdentifier = Just
                  (HsIdentifier "Word8"),
                typeSpecInstances = Map.fromList
                  [
                    _×_
                      Eq
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
                      Enum
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
                      Bounded
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
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bits
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
                      Num
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
                      StaticSize
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
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]}),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "data'",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "size"),
          functionParameterType =
          HsExtBinding
            ExtHsRef {
              extHsRefModule = HsModuleName
                "HsBindgen.Runtime.Prelude",
              extHsRefIdentifier =
              HsIdentifier "CSize"}
            TypeSpec {
              typeSpecModule = Just
                (HsModuleName
                  "HsBindgen.Runtime.Prelude"),
              typeSpecIdentifier = Just
                (HsIdentifier "CSize"),
              typeSpecInstances = Map.fromList
                [
                  _×_
                    Eq
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
                    Enum
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
                    Bounded
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
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bits
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
                    Num
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
                    StaticSize
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
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]},
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "size",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = [
                DefinitionList {
                  definitionListTerm = Bold
                    [
                      Monospace [TextContent "size"],
                      Emph [TextContent "(input)"]],
                  definitionListContent = [
                    Paragraph
                      [
                        TextContent
                          "Size of input data"]]}]}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Status_code_t"))),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_a9d86cb54c7498fe",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "status_code_t hs_bindgen_test_doxygen_docs_a9d86cb54c7498fe (config_t *arg1, uint8_t const *arg2, size_t arg3) { return complex_function(arg1, arg2, arg3); }",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "config",
                  nameHsIdent = HsIdentifier
                    "config"})
              (TypePointer
                (TypeTypedef
                  (TypedefSquashed
                    (Name "config_t")
                    (TypeStruct
                      NamePair {
                        nameC = Name "config_t",
                        nameHsIdent = HsIdentifier
                          "Config_t"}
                      (NameOriginGenerated
                        (AnonId
                          "doxygen_docs.h:232:9")))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "data",
                  nameHsIdent = HsIdentifier
                    "data'"})
              (TypePointer
                (TypeConst
                  (TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint8_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word8"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word8"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
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
                              Enum
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
                              Bounded
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
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
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
                              Num
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
                              StaticSize
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
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}}))),
            _×_
              (Just
                NamePair {
                  nameC = Name "size",
                  nameHsIdent = HsIdentifier
                    "size"})
              (TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "size_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "CSize"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "CSize"),
                    typeSpecInstances = Map.fromList
                      [
                        _×_
                          Eq
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
                          Enum
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
                          Bounded
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
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bits
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
                          Num
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
                          StaticSize
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
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}})],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeTypedef
            (TypedefSquashed
              (Name "status_code_t")
              (TypeEnum
                NamePair {
                  nameC = Name "status_code_t",
                  nameHsIdent = HsIdentifier
                    "Status_code_t"}
                (NameOriginGenerated
                  (AnonId
                    "doxygen_docs.h:258:9"))))},
      foreignImportComment =
      Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "complex_function",
          commentLocation = Just
            "doxygen_docs.h:423:15",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren =
          [
            Paragraph
              [
                TextContent
                  "Function with complex documentation"],
            Paragraph
              [
                TextContent
                  "This function demonstrates multiple documentation features:"],
            Paragraph
              [
                Bold
                  [TextContent "Description:"]],
            Paragraph
              [
                TextContent
                  "Performs complex data processing with multiple steps."],
            Paragraph
              [
                Bold
                  [TextContent "Algorithm:"]],
            ListItem {
              listItemType = NumberedList 10,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Validate input parameters"]]},
            ListItem {
              listItemType = NumberedList 200,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Allocate temporary buffers"]]},
            ListItem {
              listItemType = NumberedList
                3000,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Process data in chunks"]]},
            ListItem {
              listItemType = NumberedList
                41235,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Clean up resources"]]},
            Paragraph
              [
                Bold
                  [TextContent "Algorithm2:"]],
            ListItem {
              listItemType = BulletList,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Validate input parameters"]]},
            ListItem {
              listItemType = BulletList,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Allocate temporary buffers"]]},
            ListItem {
              listItemType = BulletList,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Process data in chunks"]]},
            ListItem {
              listItemType = BulletList,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Clean up resources"]]},
            Paragraph
              [Bold [TextContent "Example:"]],
            CodeBlock
              [
                "config_t cfg = {",
                ".id = 1,",
                ".name = \"test\",",
                ".flags = 0,",
                ".callback = my_callback,",
                ".user_data = NULL",
                "};",
                "",
                "status_code_t result = complex_function(&cfg, data, size);",
                "if (result != STATUS_OK) {",
                "handle_error(result);",
                "}"],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace
                    [TextContent "config"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Configuration structure (see",
                    Identifier "Config_t",
                    TextContent ")"]]},
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "data"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Input data buffer"]]},
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "size"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Size of input data"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent
                  "Status code indicating success or failure"],
            Paragraph
              [
                Bold
                  [TextContent "pre condition:"],
                TextContent
                  "config must not be NULL"],
            Paragraph
              [
                Bold
                  [TextContent "pre condition:"],
                TextContent
                  "data must not be NULL if size > 0"],
            Paragraph
              [
                Bold
                  [TextContent "post condition:"],
                TextContent
                  "Output data is written to config->user_data"],
            Paragraph
              [
                Bold
                  [Emph [TextContent "WARNING:"]],
                TextContent
                  "May return NULL if memory allocation fails"],
            Paragraph
              [
                Bold
                  [Emph [TextContent "WARNING:"]],
                TextContent
                  "Sets errno to EINVAL if parameters are invalid"]]},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_doxygen_docs_b7ae5186dd939781",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Config_t")))
              (HsFun
                (HsPtr
                  (HsExtBinding
                    ExtHsRef {
                      extHsRefModule = HsModuleName
                        "HsBindgen.Runtime.Prelude",
                      extHsRefIdentifier =
                      HsIdentifier "Word8"}
                    TypeSpec {
                      typeSpecModule = Just
                        (HsModuleName
                          "HsBindgen.Runtime.Prelude"),
                      typeSpecIdentifier = Just
                        (HsIdentifier "Word8"),
                      typeSpecInstances = Map.fromList
                        [
                          _×_
                            Eq
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
                            Enum
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
                            Bounded
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
                            Show
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bits
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
                            Num
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
                            StaticSize
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
                            WriteRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Storable
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = [
                                  ]})]}))
                (HsFun
                  (HsExtBinding
                    ExtHsRef {
                      extHsRefModule = HsModuleName
                        "HsBindgen.Runtime.Prelude",
                      extHsRefIdentifier =
                      HsIdentifier "CSize"}
                    TypeSpec {
                      typeSpecModule = Just
                        (HsModuleName
                          "HsBindgen.Runtime.Prelude"),
                      typeSpecIdentifier = Just
                        (HsIdentifier "CSize"),
                      typeSpecInstances = Map.fromList
                        [
                          _×_
                            Eq
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
                            Enum
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
                            Bounded
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
                            Show
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bits
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
                            Num
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
                            StaticSize
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
                            WriteRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Storable
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = [
                                  ]})]})
                  (HsIO
                    (HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Status_code_t")))))))),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_b7ae5186dd939781",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_complex_function_ptr */ __attribute__ ((const)) status_code_t (*hs_bindgen_test_doxygen_docs_b7ae5186dd939781 (void)) (config_t *arg1, uint8_t const *arg2, size_t arg3) { return &complex_function; } ",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeTypedef
                (TypedefSquashed
                  (Name "config_t")
                  (TypeStruct
                    NamePair {
                      nameC = Name "config_t",
                      nameHsIdent = HsIdentifier
                        "Config_t"}
                    (NameOriginGenerated
                      (AnonId
                        "doxygen_docs.h:232:9"))))),
            TypePointer
              (TypeConst
                (TypeExtBinding
                  ResolvedExtBinding {
                    extCName = QualName {
                      qualNameName = Name "uint8_t",
                      qualNameKind =
                      NameKindOrdinary},
                    extHsRef = ExtHsRef {
                      extHsRefModule = HsModuleName
                        "HsBindgen.Runtime.Prelude",
                      extHsRefIdentifier =
                      HsIdentifier "Word8"},
                    extHsSpec = TypeSpec {
                      typeSpecModule = Just
                        (HsModuleName
                          "HsBindgen.Runtime.Prelude"),
                      typeSpecIdentifier = Just
                        (HsIdentifier "Word8"),
                      typeSpecInstances = Map.fromList
                        [
                          _×_
                            Eq
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
                            Enum
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
                            Bounded
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
                            Show
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bits
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
                            Num
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
                            StaticSize
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
                            WriteRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Storable
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = [
                                  ]})]}})),
            TypeExtBinding
              ResolvedExtBinding {
                extCName = QualName {
                  qualNameName = Name "size_t",
                  qualNameKind =
                  NameKindOrdinary},
                extHsRef = ExtHsRef {
                  extHsRefModule = HsModuleName
                    "HsBindgen.Runtime.Prelude",
                  extHsRefIdentifier =
                  HsIdentifier "CSize"},
                extHsSpec = TypeSpec {
                  typeSpecModule = Just
                    (HsModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (HsIdentifier "CSize"),
                  typeSpecInstances = Map.fromList
                    [
                      _×_
                        Eq
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
                        Enum
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
                        Bounded
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
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bits
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
                        Num
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
                        StaticSize
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
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]}}]
          (TypeTypedef
            (TypedefSquashed
              (Name "status_code_t")
              (TypeEnum
                NamePair {
                  nameC = Name "status_code_t",
                  nameHsIdent = HsIdentifier
                    "Status_code_t"}
                (NameOriginGenerated
                  (AnonId
                    "doxygen_docs.h:258:9")))))),
      foreignImportComment =
      Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "complex_function",
          commentLocation = Just
            "doxygen_docs.h:423:15",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren =
          [
            Paragraph
              [
                TextContent
                  "Function with complex documentation"],
            Paragraph
              [
                TextContent
                  "This function demonstrates multiple documentation features:"],
            Paragraph
              [
                Bold
                  [TextContent "Description:"]],
            Paragraph
              [
                TextContent
                  "Performs complex data processing with multiple steps."],
            Paragraph
              [
                Bold
                  [TextContent "Algorithm:"]],
            ListItem {
              listItemType = NumberedList 10,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Validate input parameters"]]},
            ListItem {
              listItemType = NumberedList 200,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Allocate temporary buffers"]]},
            ListItem {
              listItemType = NumberedList
                3000,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Process data in chunks"]]},
            ListItem {
              listItemType = NumberedList
                41235,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Clean up resources"]]},
            Paragraph
              [
                Bold
                  [TextContent "Algorithm2:"]],
            ListItem {
              listItemType = BulletList,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Validate input parameters"]]},
            ListItem {
              listItemType = BulletList,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Allocate temporary buffers"]]},
            ListItem {
              listItemType = BulletList,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Process data in chunks"]]},
            ListItem {
              listItemType = BulletList,
              listItemContent = [
                Paragraph
                  [
                    TextContent
                      "Clean up resources"]]},
            Paragraph
              [Bold [TextContent "Example:"]],
            CodeBlock
              [
                "config_t cfg = {",
                ".id = 1,",
                ".name = \"test\",",
                ".flags = 0,",
                ".callback = my_callback,",
                ".user_data = NULL",
                "};",
                "",
                "status_code_t result = complex_function(&cfg, data, size);",
                "if (result != STATUS_OK) {",
                "handle_error(result);",
                "}"],
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace
                    [TextContent "config"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Configuration structure (see",
                    Identifier "Config_t",
                    TextContent ")"]]},
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "data"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Input data buffer"]]},
            DefinitionList {
              definitionListTerm = Bold
                [
                  Monospace [TextContent "size"],
                  Emph [TextContent "(input)"]],
              definitionListContent = [
                Paragraph
                  [
                    TextContent
                      "Size of input data"]]},
            Paragraph
              [
                Bold [TextContent "returns:"],
                TextContent
                  "Status code indicating success or failure"],
            Paragraph
              [
                Bold
                  [TextContent "pre condition:"],
                TextContent
                  "config must not be NULL"],
            Paragraph
              [
                Bold
                  [TextContent "pre condition:"],
                TextContent
                  "data must not be NULL if size > 0"],
            Paragraph
              [
                Bold
                  [TextContent "post condition:"],
                TextContent
                  "Output data is written to config->user_data"],
            Paragraph
              [
                Bold
                  [Emph [TextContent "WARNING:"]],
                TextContent
                  "May return NULL if memory allocation fails"],
            Paragraph
              [
                Bold
                  [Emph [TextContent "WARNING:"]],
                TextContent
                  "Sets errno to EINVAL if parameters are invalid"]]},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hash",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCChar),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "s",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_71214e4420f53a0e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_doxygen_docs_71214e4420f53a0e (char *arg1) { return hash(arg1); }",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = HsIdentifier "s"})
              (TypePointer
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))))],
          functionAttrs =
          FunctionAttributes
            CPureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "hash",
          commentLocation = Just
            "doxygen_docs.h:427:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = [
            Paragraph
              [
                TextContent "Marked",
                Monospace
                  [
                    Bold
                      [
                        TextContent
                          "attribute((pure))"]]]]},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_doxygen_docs_241bedb74b8016f3",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr (HsPrimType HsPrimCChar))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_241bedb74b8016f3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_hash_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_doxygen_docs_241bedb74b8016f3 (void)) (char *arg1) { return &hash; } ",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))))]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "hash",
          commentLocation = Just
            "doxygen_docs.h:427:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "square",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsPrimType HsPrimCInt),
      foreignImportOrigName =
      "hs_bindgen_test_doxygen_docs_8effe939268709e4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_doxygen_docs_8effe939268709e4 (signed int arg1) { return square(arg1); }",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            HaskellPureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "square",
          commentLocation = Just
            "doxygen_docs.h:429:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_doxygen_docs_631c7b52d4d4fe3a",
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
      "hs_bindgen_test_doxygen_docs_631c7b52d4d4fe3a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_square_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_doxygen_docs_631c7b52d4d4fe3a (void)) (signed int arg1) { return &square; } ",
          capiWrapperImport =
          "doxygen_docs.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed)]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "square",
          commentLocation = Just
            "doxygen_docs.h:429:5",
          commentHeader = Just
            "doxygen_docs.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
