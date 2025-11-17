[
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Myint",
      newtypeConstr = Name
        "@NsConstr"
        "Myint",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Myint",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:1:13",
          declId = NamePair {
            nameC = Name "myint",
            nameHsIdent = Identifier
              "Myint"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Myint",
              newtypeField = Name
                "@NsVar"
                "un_Myint"},
            typedefType = TypePrim
              (PrimIntegral PrimInt Signed)},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
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
          commentOrigin = Just "myint",
          commentLocation = Just
            "typedefs.h:1:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Myint",
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
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Myint"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_Myint",
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
            (Name "@NsTypeConstr" "Myint"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_Myint",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCInt,
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Intptr",
      newtypeConstr = Name
        "@NsConstr"
        "Intptr",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Intptr",
        fieldType = HsPtr
          (HsPrimType HsPrimCInt),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:2:15",
          declId = NamePair {
            nameC = Name "intptr",
            nameHsIdent = Identifier
              "Intptr"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Intptr",
              newtypeField = Name
                "@NsVar"
                "un_Intptr"},
            typedefType = TypePointer
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "intptr",
          commentLocation = Just
            "typedefs.h:2:15",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Intptr",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Intptr",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Intptr",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Intptr",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Intptr"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_Intptr",
          hasFieldInstanceFieldType =
          HsPtr (HsPrimType HsPrimCInt),
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
            (Name "@NsTypeConstr" "Intptr"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_Intptr",
          hasCFieldInstanceCFieldType =
          HsPtr (HsPrimType HsPrimCInt),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
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
          declLoc = "typedefs.h:5:13",
          declId = NamePair {
            nameC = Name "int2int",
            nameHsIdent = Identifier
              "Int2int"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
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
            "typedefs.h:5:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
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
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "FunctionPointer_Function_Deref",
      newtypeConstr = Name
        "@NsConstr"
        "FunctionPointer_Function_Deref",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_FunctionPointer_Function_Deref",
        fieldType = HsIO
          (HsPrimType HsPrimUnit),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:8:16",
          declId = NamePair {
            nameC = Name
              "FunctionPointer_Function_Deref",
            nameHsIdent = Identifier
              "FunctionPointer_Function_Deref"},
          declOrigin = NameOriginGenerated
            (AnonId "typedefs.h:8:16"),
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
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
                          nameC = Name
                            "FunctionPointer_Function",
                          nameHsIdent = Identifier
                            "FunctionPointer_Function"})]])},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "FunctionPointer_Function_Deref",
              newtypeField = Name
                "@NsVar"
                "un_FunctionPointer_Function_Deref"},
            typedefType = TypeFun
              []
              TypeVoid},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [],
      newtypeComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Auxiliary type used by",
              Identifier
                "FunctionPointer_Function"],
          commentOrigin = Nothing,
          commentLocation = Just
            "typedefs.h:8:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "toFunctionPointer_Function_Deref",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "FunctionPointer_Function_Deref"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "FunctionPointer_Function_Deref")))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = ToFunPtr
        (TypeFun [] TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fromFunctionPointer_Function_Deref",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "FunctionPointer_Function_Deref")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsTypRef
          (Name
            "@NsTypeConstr"
            "FunctionPointer_Function_Deref")),
      foreignImportOrigName =
      "dynamic",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = FromFunPtr
        (TypeFun [] TypeVoid),
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
              "FunctionPointer_Function_Deref"),
          toFunPtrInstanceBody = Name
            "@NsVar"
            "toFunctionPointer_Function_Deref"},
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
              "FunctionPointer_Function_Deref"),
          fromFunPtrInstanceBody = Name
            "@NsVar"
            "fromFunctionPointer_Function_Deref"},
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
              "FunctionPointer_Function_Deref"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_FunctionPointer_Function_Deref",
          hasFieldInstanceFieldType = HsIO
            (HsPrimType HsPrimUnit),
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
              "FunctionPointer_Function_Deref"),
          hasCFieldInstanceFieldName =
          Name
            "@NsVar"
            "un_FunctionPointer_Function_Deref",
          hasCFieldInstanceCFieldType =
          HsIO (HsPrimType HsPrimUnit),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "FunctionPointer_Function",
      newtypeConstr = Name
        "@NsConstr"
        "FunctionPointer_Function",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_FunctionPointer_Function",
        fieldType = HsFunPtr
          (HsTypRef
            (Name
              "@NsTypeConstr"
              "FunctionPointer_Function_Deref")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:8:16",
          declId = NamePair {
            nameC = Name
              "FunctionPointer_Function",
            nameHsIdent = Identifier
              "FunctionPointer_Function"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "FunctionPointer_Function",
              newtypeField = Name
                "@NsVar"
                "un_FunctionPointer_Function"},
            typedefType = TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name
                      "FunctionPointer_Function_Deref",
                    nameHsIdent = Identifier
                      "FunctionPointer_Function_Deref"}
                  (TypeFun [] TypeVoid)))},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "FunctionPointer_Function",
          commentLocation = Just
            "typedefs.h:8:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "FunctionPointer_Function",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "FunctionPointer_Function",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "FunctionPointer_Function",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "FunctionPointer_Function",
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
              "FunctionPointer_Function"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_FunctionPointer_Function",
          hasFieldInstanceFieldType =
          HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "FunctionPointer_Function_Deref")),
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
              "FunctionPointer_Function"),
          hasCFieldInstanceFieldName =
          Name
            "@NsVar"
            "un_FunctionPointer_Function",
          hasCFieldInstanceCFieldType =
          HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "FunctionPointer_Function_Deref")),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "NonFunctionPointer_Function",
      newtypeConstr = Name
        "@NsConstr"
        "NonFunctionPointer_Function",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_NonFunctionPointer_Function",
        fieldType = HsFun
          (HsPrimType HsPrimCInt)
          (HsIO (HsPrimType HsPrimCInt)),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:9:14",
          declId = NamePair {
            nameC = Name
              "NonFunctionPointer_Function",
            nameHsIdent = Identifier
              "NonFunctionPointer_Function"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "NonFunctionPointer_Function",
              newtypeField = Name
                "@NsVar"
                "un_NonFunctionPointer_Function"},
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
          commentOrigin = Just
            "NonFunctionPointer_Function",
          commentLocation = Just
            "typedefs.h:9:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "toNonFunctionPointer_Function",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "NonFunctionPointer_Function"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "NonFunctionPointer_Function")))),
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
        "fromNonFunctionPointer_Function",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "NonFunctionPointer_Function")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsTypRef
          (Name
            "@NsTypeConstr"
            "NonFunctionPointer_Function")),
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
              "NonFunctionPointer_Function"),
          toFunPtrInstanceBody = Name
            "@NsVar"
            "toNonFunctionPointer_Function"},
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
              "NonFunctionPointer_Function"),
          fromFunPtrInstanceBody = Name
            "@NsVar"
            "fromNonFunctionPointer_Function"},
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
              "NonFunctionPointer_Function"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_NonFunctionPointer_Function",
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
              "NonFunctionPointer_Function"),
          hasCFieldInstanceFieldName =
          Name
            "@NsVar"
            "un_NonFunctionPointer_Function",
          hasCFieldInstanceCFieldType =
          HsFun
            (HsPrimType HsPrimCInt)
            (HsIO (HsPrimType HsPrimCInt)),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "F1_Deref",
      newtypeConstr = Name
        "@NsConstr"
        "F1_Deref",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_F1_Deref",
        fieldType = HsIO
          (HsPrimType HsPrimUnit),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:11:16",
          declId = NamePair {
            nameC = Name "f1_Deref",
            nameHsIdent = Identifier
              "F1_Deref"},
          declOrigin = NameOriginGenerated
            (AnonId "typedefs.h:11:16"),
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
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
                          nameC = Name "f1",
                          nameHsIdent = Identifier
                            "F1"})]])},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "F1_Deref",
              newtypeField = Name
                "@NsVar"
                "un_F1_Deref"},
            typedefType = TypeFun
              []
              TypeVoid},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [],
      newtypeComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Auxiliary type used by",
              Identifier "F1"],
          commentOrigin = Nothing,
          commentLocation = Just
            "typedefs.h:11:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "toF1_Deref",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "F1_Deref"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "F1_Deref")))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = ToFunPtr
        (TypeFun [] TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fromF1_Deref",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "F1_Deref")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsTypRef
          (Name
            "@NsTypeConstr"
            "F1_Deref")),
      foreignImportOrigName =
      "dynamic",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = FromFunPtr
        (TypeFun [] TypeVoid),
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
              "F1_Deref"),
          toFunPtrInstanceBody = Name
            "@NsVar"
            "toF1_Deref"},
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
              "F1_Deref"),
          fromFunPtrInstanceBody = Name
            "@NsVar"
            "fromF1_Deref"},
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
              "F1_Deref"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_F1_Deref",
          hasFieldInstanceFieldType = HsIO
            (HsPrimType HsPrimUnit),
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
              "F1_Deref"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_F1_Deref",
          hasCFieldInstanceCFieldType =
          HsIO (HsPrimType HsPrimUnit),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "F1",
      newtypeConstr = Name
        "@NsConstr"
        "F1",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_F1",
        fieldType = HsFunPtr
          (HsTypRef
            (Name
              "@NsTypeConstr"
              "F1_Deref")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:11:16",
          declId = NamePair {
            nameC = Name "f1",
            nameHsIdent = Identifier "F1"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "F1",
              newtypeField = Name
                "@NsVar"
                "un_F1"},
            typedefType = TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "f1_Deref",
                    nameHsIdent = Identifier
                      "F1_Deref"}
                  (TypeFun [] TypeVoid)))},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "f1",
          commentLocation = Just
            "typedefs.h:11:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "F1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "F1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "F1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "F1",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "F1"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_F1",
          hasFieldInstanceFieldType =
          HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "F1_Deref")),
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
            (Name "@NsTypeConstr" "F1"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_F1",
          hasCFieldInstanceCFieldType =
          HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "F1_Deref")),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "G1",
      newtypeConstr = Name
        "@NsConstr"
        "G1",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_G1",
        fieldType = HsIO
          (HsPrimType HsPrimUnit),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:13:14",
          declId = NamePair {
            nameC = Name "g1",
            nameHsIdent = Identifier "G1"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "G1",
              newtypeField = Name
                "@NsVar"
                "un_G1"},
            typedefType = TypeFun
              []
              TypeVoid},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "g1",
          commentLocation = Just
            "typedefs.h:13:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "toG1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "G1"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsTypRef
              (Name "@NsTypeConstr" "G1")))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = ToFunPtr
        (TypeFun [] TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fromG1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsTypRef
              (Name "@NsTypeConstr" "G1")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsTypRef
          (Name "@NsTypeConstr" "G1")),
      foreignImportOrigName =
      "dynamic",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = FromFunPtr
        (TypeFun [] TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceToFunPtr
        ToFunPtrInstance {
          toFunPtrInstanceType = HsTypRef
            (Name "@NsTypeConstr" "G1"),
          toFunPtrInstanceBody = Name
            "@NsVar"
            "toG1"},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceFromFunPtr
        FromFunPtrInstance {
          fromFunPtrInstanceType =
          HsTypRef
            (Name "@NsTypeConstr" "G1"),
          fromFunPtrInstanceBody = Name
            "@NsVar"
            "fromG1"},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "G1"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_G1",
          hasFieldInstanceFieldType = HsIO
            (HsPrimType HsPrimUnit),
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
            (Name "@NsTypeConstr" "G1"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_G1",
          hasCFieldInstanceCFieldType =
          HsIO (HsPrimType HsPrimUnit),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "G2",
      newtypeConstr = Name
        "@NsConstr"
        "G2",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_G2",
        fieldType = HsFunPtr
          (HsTypRef
            (Name "@NsTypeConstr" "G1")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:14:14",
          declId = NamePair {
            nameC = Name "g2",
            nameHsIdent = Identifier "G2"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "G2",
              newtypeField = Name
                "@NsVar"
                "un_G2"},
            typedefType = TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "g1",
                    nameHsIdent = Identifier "G1"}
                  (TypeFun [] TypeVoid)))},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "g2",
          commentLocation = Just
            "typedefs.h:14:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "G2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "G2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "G2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "G2",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "G2"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_G2",
          hasFieldInstanceFieldType =
          HsFunPtr
            (HsTypRef
              (Name "@NsTypeConstr" "G1")),
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
            (Name "@NsTypeConstr" "G2"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_G2",
          hasCFieldInstanceCFieldType =
          HsFunPtr
            (HsTypRef
              (Name "@NsTypeConstr" "G1")),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "H1",
      newtypeConstr = Name
        "@NsConstr"
        "H1",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_H1",
        fieldType = HsIO
          (HsPrimType HsPrimUnit),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:16:14",
          declId = NamePair {
            nameC = Name "h1",
            nameHsIdent = Identifier "H1"},
          declOrigin = NameOriginInSource,
          declAliases = [Name "h2"],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "H1",
              newtypeField = Name
                "@NsVar"
                "un_H1"},
            typedefType = TypeFun
              []
              TypeVoid},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "h1",
          commentLocation = Just
            "typedefs.h:16:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "toH1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "H1"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsTypRef
              (Name "@NsTypeConstr" "H1")))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = ToFunPtr
        (TypeFun [] TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fromH1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsTypRef
              (Name "@NsTypeConstr" "H1")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsTypRef
          (Name "@NsTypeConstr" "H1")),
      foreignImportOrigName =
      "dynamic",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = FromFunPtr
        (TypeFun [] TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceToFunPtr
        ToFunPtrInstance {
          toFunPtrInstanceType = HsTypRef
            (Name "@NsTypeConstr" "H1"),
          toFunPtrInstanceBody = Name
            "@NsVar"
            "toH1"},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceFromFunPtr
        FromFunPtrInstance {
          fromFunPtrInstanceType =
          HsTypRef
            (Name "@NsTypeConstr" "H1"),
          fromFunPtrInstanceBody = Name
            "@NsVar"
            "fromH1"},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "H1"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_H1",
          hasFieldInstanceFieldType = HsIO
            (HsPrimType HsPrimUnit),
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
            (Name "@NsTypeConstr" "H1"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_H1",
          hasCFieldInstanceCFieldType =
          HsIO (HsPrimType HsPrimUnit),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "H2",
      newtypeConstr = Name
        "@NsConstr"
        "H2",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_H2",
        fieldType = HsTypRef
          (Name "@NsTypeConstr" "H1"),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:17:12",
          declId = NamePair {
            nameC = Name "h2",
            nameHsIdent = Identifier "H2"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "H2",
              newtypeField = Name
                "@NsVar"
                "un_H2"},
            typedefType = TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "h1",
                  nameHsIdent = Identifier "H1"}
                (TypeFun [] TypeVoid))},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "h2",
          commentLocation = Just
            "typedefs.h:17:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "H2"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_H2",
          hasFieldInstanceFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "H1"),
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
            (Name "@NsTypeConstr" "H2"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_H2",
          hasCFieldInstanceCFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "H1"),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "H3",
      newtypeConstr = Name
        "@NsConstr"
        "H3",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_H3",
        fieldType = HsFunPtr
          (HsTypRef
            (Name "@NsTypeConstr" "H2")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:18:14",
          declId = NamePair {
            nameC = Name "h3",
            nameHsIdent = Identifier "H3"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "H3",
              newtypeField = Name
                "@NsVar"
                "un_H3"},
            typedefType = TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "h2",
                    nameHsIdent = Identifier "H2"}
                  (TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "h1",
                        nameHsIdent = Identifier "H1"}
                      (TypeFun [] TypeVoid)))))},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "h3",
          commentLocation = Just
            "typedefs.h:18:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "H3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "H3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "H3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "H3",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "H3"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_H3",
          hasFieldInstanceFieldType =
          HsFunPtr
            (HsTypRef
              (Name "@NsTypeConstr" "H2")),
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
            (Name "@NsTypeConstr" "H3"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_H3",
          hasCFieldInstanceCFieldType =
          HsFunPtr
            (HsTypRef
              (Name "@NsTypeConstr" "H2")),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing}]
