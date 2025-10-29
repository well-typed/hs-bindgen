[
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "FileOpenedNotification_Deref",
      newtypeConstr = Name
        "@NsConstr"
        "FileOpenedNotification_Deref",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_FileOpenedNotification_Deref",
        fieldType = HsIO
          (HsPrimType HsPrimUnit),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "callbacks.h:10:16",
          declId = NamePair {
            nameC = Name
              "FileOpenedNotification_Deref",
            nameHsIdent = Identifier
              "FileOpenedNotification_Deref"},
          declOrigin = NameOriginGenerated
            (AnonId "callbacks.h:10:16"),
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
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
                            "FileOpenedNotification",
                          nameHsIdent = Identifier
                            "FileOpenedNotification"})]])},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "FileOpenedNotification_Deref",
              newtypeField = Name
                "@NsVar"
                "un_FileOpenedNotification_Deref"},
            typedefType = TypeFun
              []
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
              Identifier
                "FileOpenedNotification"],
          commentOrigin = Nothing,
          commentLocation = Just
            "callbacks.h:10:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "toFileOpenedNotification_Deref",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "FileOpenedNotification_Deref"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "FileOpenedNotification_Deref")))),
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
        "fromFileOpenedNotification_Deref",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "FileOpenedNotification_Deref")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsTypRef
          (Name
            "@NsTypeConstr"
            "FileOpenedNotification_Deref")),
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
              "FileOpenedNotification_Deref"),
          toFunPtrInstanceBody = Name
            "@NsVar"
            "toFileOpenedNotification_Deref"},
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
              "FileOpenedNotification_Deref"),
          fromFunPtrInstanceBody = Name
            "@NsVar"
            "fromFileOpenedNotification_Deref"},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "FileOpenedNotification",
      newtypeConstr = Name
        "@NsConstr"
        "FileOpenedNotification",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_FileOpenedNotification",
        fieldType = HsFunPtr
          (HsTypRef
            (Name
              "@NsTypeConstr"
              "FileOpenedNotification_Deref")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "callbacks.h:10:16",
          declId = NamePair {
            nameC = Name
              "FileOpenedNotification",
            nameHsIdent = Identifier
              "FileOpenedNotification"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "FileOpenedNotification",
              newtypeField = Name
                "@NsVar"
                "un_FileOpenedNotification"},
            typedefType = TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name
                      "FileOpenedNotification_Deref",
                    nameHsIdent = Identifier
                      "FileOpenedNotification_Deref"}
                  (TypeFun [] TypeVoid)))},
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
            "FileOpenedNotification",
          commentLocation = Just
            "callbacks.h:10:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "FileOpenedNotification",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "FileOpenedNotification",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "FileOpenedNotification",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "FileOpenedNotification",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "ProgressUpdate_Deref",
      newtypeConstr = Name
        "@NsConstr"
        "ProgressUpdate_Deref",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_ProgressUpdate_Deref",
        fieldType = HsFun
          (HsPrimType HsPrimCInt)
          (HsIO (HsPrimType HsPrimUnit)),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "callbacks.h:11:16",
          declId = NamePair {
            nameC = Name
              "ProgressUpdate_Deref",
            nameHsIdent = Identifier
              "ProgressUpdate_Deref"},
          declOrigin = NameOriginGenerated
            (AnonId "callbacks.h:11:16"),
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
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
                          nameC = Name "ProgressUpdate",
                          nameHsIdent = Identifier
                            "ProgressUpdate"})]])},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "ProgressUpdate_Deref",
              newtypeField = Name
                "@NsVar"
                "un_ProgressUpdate_Deref"},
            typedefType = TypeFun
              [
                TypePrim
                  (PrimIntegral PrimInt Signed)]
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
              Identifier "ProgressUpdate"],
          commentOrigin = Nothing,
          commentLocation = Just
            "callbacks.h:11:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "toProgressUpdate_Deref",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "ProgressUpdate_Deref"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "ProgressUpdate_Deref")))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = ToFunPtr
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed)]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fromProgressUpdate_Deref",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "ProgressUpdate_Deref")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsTypRef
          (Name
            "@NsTypeConstr"
            "ProgressUpdate_Deref")),
      foreignImportOrigName =
      "dynamic",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = FromFunPtr
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed)]
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
              "ProgressUpdate_Deref"),
          toFunPtrInstanceBody = Name
            "@NsVar"
            "toProgressUpdate_Deref"},
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
              "ProgressUpdate_Deref"),
          fromFunPtrInstanceBody = Name
            "@NsVar"
            "fromProgressUpdate_Deref"},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "ProgressUpdate",
      newtypeConstr = Name
        "@NsConstr"
        "ProgressUpdate",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_ProgressUpdate",
        fieldType = HsFunPtr
          (HsTypRef
            (Name
              "@NsTypeConstr"
              "ProgressUpdate_Deref")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "callbacks.h:11:16",
          declId = NamePair {
            nameC = Name "ProgressUpdate",
            nameHsIdent = Identifier
              "ProgressUpdate"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "ProgressUpdate",
              newtypeField = Name
                "@NsVar"
                "un_ProgressUpdate"},
            typedefType = TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name
                      "ProgressUpdate_Deref",
                    nameHsIdent = Identifier
                      "ProgressUpdate_Deref"}
                  (TypeFun
                    [
                      TypePrim
                        (PrimIntegral PrimInt Signed)]
                    TypeVoid)))},
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
            "ProgressUpdate",
          commentLocation = Just
            "callbacks.h:11:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ProgressUpdate",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ProgressUpdate",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ProgressUpdate",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ProgressUpdate",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "DataValidator_Deref",
      newtypeConstr = Name
        "@NsConstr"
        "DataValidator_Deref",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_DataValidator_Deref",
        fieldType = HsFun
          (HsPrimType HsPrimCInt)
          (HsIO (HsPrimType HsPrimCInt)),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "callbacks.h:12:15",
          declId = NamePair {
            nameC = Name
              "DataValidator_Deref",
            nameHsIdent = Identifier
              "DataValidator_Deref"},
          declOrigin = NameOriginGenerated
            (AnonId "callbacks.h:12:15"),
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
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
                          nameC = Name "DataValidator",
                          nameHsIdent = Identifier
                            "DataValidator"})]])},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "DataValidator_Deref",
              newtypeField = Name
                "@NsVar"
                "un_DataValidator_Deref"},
            typedefType = TypeFun
              [
                TypePrim
                  (PrimIntegral PrimInt Signed)]
              (TypePrim
                (PrimIntegral PrimInt Signed))},
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
              Identifier "DataValidator"],
          commentOrigin = Nothing,
          commentLocation = Just
            "callbacks.h:12:15",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "toDataValidator_Deref",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "DataValidator_Deref"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "DataValidator_Deref")))),
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
        "fromDataValidator_Deref",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "DataValidator_Deref")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsTypRef
          (Name
            "@NsTypeConstr"
            "DataValidator_Deref")),
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
              "DataValidator_Deref"),
          toFunPtrInstanceBody = Name
            "@NsVar"
            "toDataValidator_Deref"},
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
              "DataValidator_Deref"),
          fromFunPtrInstanceBody = Name
            "@NsVar"
            "fromDataValidator_Deref"},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "DataValidator",
      newtypeConstr = Name
        "@NsConstr"
        "DataValidator",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_DataValidator",
        fieldType = HsFunPtr
          (HsTypRef
            (Name
              "@NsTypeConstr"
              "DataValidator_Deref")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "callbacks.h:12:15",
          declId = NamePair {
            nameC = Name "DataValidator",
            nameHsIdent = Identifier
              "DataValidator"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "DataValidator",
              newtypeField = Name
                "@NsVar"
                "un_DataValidator"},
            typedefType = TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name
                      "DataValidator_Deref",
                    nameHsIdent = Identifier
                      "DataValidator_Deref"}
                  (TypeFun
                    [
                      TypePrim
                        (PrimIntegral PrimInt Signed)]
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))},
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
            "DataValidator",
          commentLocation = Just
            "callbacks.h:12:15",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "DataValidator",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "DataValidator",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "DataValidator",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "DataValidator",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Measurement",
      structConstr = Name
        "@NsConstr"
        "Measurement",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "measurement_value",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "callbacks.h:22:10",
                fieldName = NamePair {
                  nameC = Name "value",
                  nameHsIdent = Identifier
                    "measurement_value"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "value",
              commentLocation = Just
                "callbacks.h:22:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["callbacks.h"],
                  headerInclude = "callbacks.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "measurement_timestamp",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "callbacks.h:23:10",
                fieldName = NamePair {
                  nameC = Name "timestamp",
                  nameHsIdent = Identifier
                    "measurement_timestamp"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "timestamp",
              commentLocation = Just
                "callbacks.h:23:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["callbacks.h"],
                  headerInclude = "callbacks.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "callbacks.h:21:8",
            declId = NamePair {
              nameC = Name "Measurement",
              nameHsIdent = Identifier
                "Measurement"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["callbacks.h"],
                headerInclude = "callbacks.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name
                  "@NsConstr"
                  "Measurement"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "callbacks.h:22:10",
                    fieldName = NamePair {
                      nameC = Name "value",
                      nameHsIdent = Identifier
                        "measurement_value"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "callbacks.h:23:10",
                    fieldName = NamePair {
                      nameC = Name "timestamp",
                      nameHsIdent = Identifier
                        "measurement_timestamp"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 64,
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
            "Measurement",
          commentLocation = Just
            "callbacks.h:21:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Measurement",
          structConstr = Name
            "@NsConstr"
            "Measurement",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "measurement_value",
              fieldType = HsPrimType
                HsPrimCDouble,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "callbacks.h:22:10",
                    fieldName = NamePair {
                      nameC = Name "value",
                      nameHsIdent = Identifier
                        "measurement_value"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "value",
                  commentLocation = Just
                    "callbacks.h:22:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["callbacks.h"],
                      headerInclude = "callbacks.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "measurement_timestamp",
              fieldType = HsPrimType
                HsPrimCDouble,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "callbacks.h:23:10",
                    fieldName = NamePair {
                      nameC = Name "timestamp",
                      nameHsIdent = Identifier
                        "measurement_timestamp"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just
                    "timestamp",
                  commentLocation = Just
                    "callbacks.h:23:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["callbacks.h"],
                      headerInclude = "callbacks.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "callbacks.h:21:8",
                declId = NamePair {
                  nameC = Name "Measurement",
                  nameHsIdent = Identifier
                    "Measurement"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["callbacks.h"],
                    headerInclude = "callbacks.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name
                      "@NsConstr"
                      "Measurement"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "callbacks.h:22:10",
                        fieldName = NamePair {
                          nameC = Name "value",
                          nameHsIdent = Identifier
                            "measurement_value"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimFloating PrimDouble),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "callbacks.h:23:10",
                        fieldName = NamePair {
                          nameC = Name "timestamp",
                          nameHsIdent = Identifier
                            "measurement_timestamp"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimFloating PrimDouble),
                      structFieldOffset = 64,
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
                "Measurement",
              commentLocation = Just
                "callbacks.h:21:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["callbacks.h"],
                  headerInclude = "callbacks.h"},
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
                    "Measurement",
                  structConstr = Name
                    "@NsConstr"
                    "Measurement",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "measurement_value",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "callbacks.h:22:10",
                            fieldName = NamePair {
                              nameC = Name "value",
                              nameHsIdent = Identifier
                                "measurement_value"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "value",
                          commentLocation = Just
                            "callbacks.h:22:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "measurement_timestamp",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "callbacks.h:23:10",
                            fieldName = NamePair {
                              nameC = Name "timestamp",
                              nameHsIdent = Identifier
                                "measurement_timestamp"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "timestamp",
                          commentLocation = Just
                            "callbacks.h:23:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "callbacks.h:21:8",
                        declId = NamePair {
                          nameC = Name "Measurement",
                          nameHsIdent = Identifier
                            "Measurement"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["callbacks.h"],
                            headerInclude = "callbacks.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Measurement"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "callbacks.h:22:10",
                                fieldName = NamePair {
                                  nameC = Name "value",
                                  nameHsIdent = Identifier
                                    "measurement_value"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "callbacks.h:23:10",
                                fieldName = NamePair {
                                  nameC = Name "timestamp",
                                  nameHsIdent = Identifier
                                    "measurement_timestamp"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 64,
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
                        "Measurement",
                      commentLocation = Just
                        "callbacks.h:21:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["callbacks.h"],
                          headerInclude = "callbacks.h"},
                      commentChildren = []}})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 8]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Measurement",
                  structConstr = Name
                    "@NsConstr"
                    "Measurement",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "measurement_value",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "callbacks.h:22:10",
                            fieldName = NamePair {
                              nameC = Name "value",
                              nameHsIdent = Identifier
                                "measurement_value"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "value",
                          commentLocation = Just
                            "callbacks.h:22:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "measurement_timestamp",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "callbacks.h:23:10",
                            fieldName = NamePair {
                              nameC = Name "timestamp",
                              nameHsIdent = Identifier
                                "measurement_timestamp"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "timestamp",
                          commentLocation = Just
                            "callbacks.h:23:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "callbacks.h:21:8",
                        declId = NamePair {
                          nameC = Name "Measurement",
                          nameHsIdent = Identifier
                            "Measurement"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["callbacks.h"],
                            headerInclude = "callbacks.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Measurement"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "callbacks.h:22:10",
                                fieldName = NamePair {
                                  nameC = Name "value",
                                  nameHsIdent = Identifier
                                    "measurement_value"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "callbacks.h:23:10",
                                fieldName = NamePair {
                                  nameC = Name "timestamp",
                                  nameHsIdent = Identifier
                                    "measurement_timestamp"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 64,
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
                        "Measurement",
                      commentLocation = Just
                        "callbacks.h:21:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["callbacks.h"],
                          headerInclude = "callbacks.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      8
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
        "Measurement",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Measurement",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "MeasurementReceived_Deref",
      newtypeConstr = Name
        "@NsConstr"
        "MeasurementReceived_Deref",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_MeasurementReceived_Deref",
        fieldType = HsFun
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Measurement")))
          (HsIO (HsPrimType HsPrimUnit)),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "callbacks.h:26:16",
          declId = NamePair {
            nameC = Name
              "MeasurementReceived_Deref",
            nameHsIdent = Identifier
              "MeasurementReceived_Deref"},
          declOrigin = NameOriginGenerated
            (AnonId "callbacks.h:26:16"),
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
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
                            "MeasurementReceived",
                          nameHsIdent = Identifier
                            "MeasurementReceived"})]])},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "MeasurementReceived_Deref",
              newtypeField = Name
                "@NsVar"
                "un_MeasurementReceived_Deref"},
            typedefType = TypeFun
              [
                TypePointer
                  (TypeStruct
                    NamePair {
                      nameC = Name "Measurement",
                      nameHsIdent = Identifier
                        "Measurement"}
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
              Identifier
                "MeasurementReceived"],
          commentOrigin = Nothing,
          commentLocation = Just
            "callbacks.h:26:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "toMeasurementReceived_Deref",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "MeasurementReceived_Deref"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "MeasurementReceived_Deref")))),
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
                  nameC = Name "Measurement",
                  nameHsIdent = Identifier
                    "Measurement"}
                NameOriginInSource)]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fromMeasurementReceived_Deref",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "MeasurementReceived_Deref")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsTypRef
          (Name
            "@NsTypeConstr"
            "MeasurementReceived_Deref")),
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
                  nameC = Name "Measurement",
                  nameHsIdent = Identifier
                    "Measurement"}
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
              "MeasurementReceived_Deref"),
          toFunPtrInstanceBody = Name
            "@NsVar"
            "toMeasurementReceived_Deref"},
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
              "MeasurementReceived_Deref"),
          fromFunPtrInstanceBody = Name
            "@NsVar"
            "fromMeasurementReceived_Deref"},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "MeasurementReceived",
      newtypeConstr = Name
        "@NsConstr"
        "MeasurementReceived",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_MeasurementReceived",
        fieldType = HsFunPtr
          (HsTypRef
            (Name
              "@NsTypeConstr"
              "MeasurementReceived_Deref")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "callbacks.h:26:16",
          declId = NamePair {
            nameC = Name
              "MeasurementReceived",
            nameHsIdent = Identifier
              "MeasurementReceived"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "MeasurementReceived",
              newtypeField = Name
                "@NsVar"
                "un_MeasurementReceived"},
            typedefType = TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name
                      "MeasurementReceived_Deref",
                    nameHsIdent = Identifier
                      "MeasurementReceived_Deref"}
                  (TypeFun
                    [
                      TypePointer
                        (TypeStruct
                          NamePair {
                            nameC = Name "Measurement",
                            nameHsIdent = Identifier
                              "Measurement"}
                          NameOriginInSource)]
                    TypeVoid)))},
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
            "MeasurementReceived",
          commentLocation = Just
            "callbacks.h:26:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MeasurementReceived",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MeasurementReceived",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MeasurementReceived",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MeasurementReceived",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "MeasurementReceived2_Deref",
      newtypeConstr = Name
        "@NsConstr"
        "MeasurementReceived2_Deref",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_MeasurementReceived2_Deref",
        fieldType = HsFun
          (HsTypRef
            (Name
              "@NsTypeConstr"
              "Measurement"))
          (HsIO (HsPrimType HsPrimUnit)),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "callbacks.h:29:16",
          declId = NamePair {
            nameC = Name
              "MeasurementReceived2_Deref",
            nameHsIdent = Identifier
              "MeasurementReceived2_Deref"},
          declOrigin = NameOriginGenerated
            (AnonId "callbacks.h:29:16"),
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
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
                            "MeasurementReceived2",
                          nameHsIdent = Identifier
                            "MeasurementReceived2"})]])},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "MeasurementReceived2_Deref",
              newtypeField = Name
                "@NsVar"
                "un_MeasurementReceived2_Deref"},
            typedefType = TypeFun
              [
                TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource]
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
              Identifier
                "MeasurementReceived2"],
          commentOrigin = Nothing,
          commentLocation = Just
            "callbacks.h:29:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []}},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "MeasurementReceived2",
      newtypeConstr = Name
        "@NsConstr"
        "MeasurementReceived2",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_MeasurementReceived2",
        fieldType = HsFunPtr
          (HsTypRef
            (Name
              "@NsTypeConstr"
              "MeasurementReceived2_Deref")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "callbacks.h:29:16",
          declId = NamePair {
            nameC = Name
              "MeasurementReceived2",
            nameHsIdent = Identifier
              "MeasurementReceived2"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "MeasurementReceived2",
              newtypeField = Name
                "@NsVar"
                "un_MeasurementReceived2"},
            typedefType = TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name
                      "MeasurementReceived2_Deref",
                    nameHsIdent = Identifier
                      "MeasurementReceived2_Deref"}
                  (TypeFun
                    [
                      TypeStruct
                        NamePair {
                          nameC = Name "Measurement",
                          nameHsIdent = Identifier
                            "Measurement"}
                        NameOriginInSource]
                    TypeVoid)))},
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
            "MeasurementReceived2",
          commentLocation = Just
            "callbacks.h:29:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MeasurementReceived2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MeasurementReceived2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MeasurementReceived2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MeasurementReceived2",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "SampleBufferFull_Deref",
      newtypeConstr = Name
        "@NsConstr"
        "SampleBufferFull_Deref",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_SampleBufferFull_Deref",
        fieldType = HsFun
          (HsConstArray
            10
            (HsPrimType HsPrimCInt))
          (HsIO (HsPrimType HsPrimUnit)),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "callbacks.h:32:16",
          declId = NamePair {
            nameC = Name
              "SampleBufferFull_Deref",
            nameHsIdent = Identifier
              "SampleBufferFull_Deref"},
          declOrigin = NameOriginGenerated
            (AnonId "callbacks.h:32:16"),
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
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
                          nameC = Name "SampleBufferFull",
                          nameHsIdent = Identifier
                            "SampleBufferFull"})]])},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "SampleBufferFull_Deref",
              newtypeField = Name
                "@NsVar"
                "un_SampleBufferFull_Deref"},
            typedefType = TypeFun
              [
                TypeConstArray
                  10
                  (TypePrim
                    (PrimIntegral PrimInt Signed))]
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
              Identifier "SampleBufferFull"],
          commentOrigin = Nothing,
          commentLocation = Just
            "callbacks.h:32:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []}},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "SampleBufferFull",
      newtypeConstr = Name
        "@NsConstr"
        "SampleBufferFull",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_SampleBufferFull",
        fieldType = HsFunPtr
          (HsTypRef
            (Name
              "@NsTypeConstr"
              "SampleBufferFull_Deref")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "callbacks.h:32:16",
          declId = NamePair {
            nameC = Name "SampleBufferFull",
            nameHsIdent = Identifier
              "SampleBufferFull"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "SampleBufferFull",
              newtypeField = Name
                "@NsVar"
                "un_SampleBufferFull"},
            typedefType = TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name
                      "SampleBufferFull_Deref",
                    nameHsIdent = Identifier
                      "SampleBufferFull_Deref"}
                  (TypeFun
                    [
                      TypeConstArray
                        10
                        (TypePrim
                          (PrimIntegral PrimInt Signed))]
                    TypeVoid)))},
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
            "SampleBufferFull",
          commentLocation = Just
            "callbacks.h:32:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "SampleBufferFull",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "SampleBufferFull",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "SampleBufferFull",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "SampleBufferFull",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "MeasurementHandler",
      structConstr = Name
        "@NsConstr"
        "MeasurementHandler",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "measurementHandler_onReceived",
          fieldType = HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsIO (HsPrimType HsPrimUnit))),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "callbacks.h:51:10",
                fieldName = NamePair {
                  nameC = Name "onReceived",
                  nameHsIdent = Identifier
                    "measurementHandler_onReceived"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeFun
                  [
                    TypePointer
                      (TypeStruct
                        NamePair {
                          nameC = Name "Measurement",
                          nameHsIdent = Identifier
                            "Measurement"}
                        NameOriginInSource)]
                  TypeVoid),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "onReceived",
              commentLocation = Just
                "callbacks.h:51:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["callbacks.h"],
                  headerInclude = "callbacks.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "measurementHandler_validate",
          fieldType = HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsIO (HsPrimType HsPrimCInt))),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "callbacks.h:52:9",
                fieldName = NamePair {
                  nameC = Name "validate",
                  nameHsIdent = Identifier
                    "measurementHandler_validate"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeFun
                  [
                    TypePointer
                      (TypeStruct
                        NamePair {
                          nameC = Name "Measurement",
                          nameHsIdent = Identifier
                            "Measurement"}
                        NameOriginInSource)]
                  (TypePrim
                    (PrimIntegral PrimInt Signed))),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "validate",
              commentLocation = Just
                "callbacks.h:52:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["callbacks.h"],
                  headerInclude = "callbacks.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "measurementHandler_onError",
          fieldType = HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimUnit))),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "callbacks.h:53:10",
                fieldName = NamePair {
                  nameC = Name "onError",
                  nameHsIdent = Identifier
                    "measurementHandler_onError"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeFun
                  [
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  TypeVoid),
              structFieldOffset = 128,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "onError",
              commentLocation = Just
                "callbacks.h:53:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["callbacks.h"],
                  headerInclude = "callbacks.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "callbacks.h:50:8",
            declId = NamePair {
              nameC = Name
                "MeasurementHandler",
              nameHsIdent = Identifier
                "MeasurementHandler"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["callbacks.h"],
                headerInclude = "callbacks.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name
                  "@NsConstr"
                  "MeasurementHandler"),
              structSizeof = 24,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "callbacks.h:51:10",
                    fieldName = NamePair {
                      nameC = Name "onReceived",
                      nameHsIdent = Identifier
                        "measurementHandler_onReceived"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeFun
                      [
                        TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = Name "Measurement",
                              nameHsIdent = Identifier
                                "Measurement"}
                            NameOriginInSource)]
                      TypeVoid),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "callbacks.h:52:9",
                    fieldName = NamePair {
                      nameC = Name "validate",
                      nameHsIdent = Identifier
                        "measurementHandler_validate"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeFun
                      [
                        TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = Name "Measurement",
                              nameHsIdent = Identifier
                                "Measurement"}
                            NameOriginInSource)]
                      (TypePrim
                        (PrimIntegral PrimInt Signed))),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "callbacks.h:53:10",
                    fieldName = NamePair {
                      nameC = Name "onError",
                      nameHsIdent = Identifier
                        "measurementHandler_onError"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeFun
                      [
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      TypeVoid),
                  structFieldOffset = 128,
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
            "MeasurementHandler",
          commentLocation = Just
            "callbacks.h:50:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "MeasurementHandler",
          structConstr = Name
            "@NsConstr"
            "MeasurementHandler",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "measurementHandler_onReceived",
              fieldType = HsFunPtr
                (HsFun
                  (HsPtr
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "Measurement")))
                  (HsIO (HsPrimType HsPrimUnit))),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "callbacks.h:51:10",
                    fieldName = NamePair {
                      nameC = Name "onReceived",
                      nameHsIdent = Identifier
                        "measurementHandler_onReceived"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeFun
                      [
                        TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = Name "Measurement",
                              nameHsIdent = Identifier
                                "Measurement"}
                            NameOriginInSource)]
                      TypeVoid),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just
                    "onReceived",
                  commentLocation = Just
                    "callbacks.h:51:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["callbacks.h"],
                      headerInclude = "callbacks.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "measurementHandler_validate",
              fieldType = HsFunPtr
                (HsFun
                  (HsPtr
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "Measurement")))
                  (HsIO (HsPrimType HsPrimCInt))),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "callbacks.h:52:9",
                    fieldName = NamePair {
                      nameC = Name "validate",
                      nameHsIdent = Identifier
                        "measurementHandler_validate"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeFun
                      [
                        TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = Name "Measurement",
                              nameHsIdent = Identifier
                                "Measurement"}
                            NameOriginInSource)]
                      (TypePrim
                        (PrimIntegral PrimInt Signed))),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "validate",
                  commentLocation = Just
                    "callbacks.h:52:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["callbacks.h"],
                      headerInclude = "callbacks.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "measurementHandler_onError",
              fieldType = HsFunPtr
                (HsFun
                  (HsPrimType HsPrimCInt)
                  (HsIO (HsPrimType HsPrimUnit))),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "callbacks.h:53:10",
                    fieldName = NamePair {
                      nameC = Name "onError",
                      nameHsIdent = Identifier
                        "measurementHandler_onError"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeFun
                      [
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      TypeVoid),
                  structFieldOffset = 128,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "onError",
                  commentLocation = Just
                    "callbacks.h:53:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["callbacks.h"],
                      headerInclude = "callbacks.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "callbacks.h:50:8",
                declId = NamePair {
                  nameC = Name
                    "MeasurementHandler",
                  nameHsIdent = Identifier
                    "MeasurementHandler"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["callbacks.h"],
                    headerInclude = "callbacks.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name
                      "@NsConstr"
                      "MeasurementHandler"),
                  structSizeof = 24,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "callbacks.h:51:10",
                        fieldName = NamePair {
                          nameC = Name "onReceived",
                          nameHsIdent = Identifier
                            "measurementHandler_onReceived"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeFun
                          [
                            TypePointer
                              (TypeStruct
                                NamePair {
                                  nameC = Name "Measurement",
                                  nameHsIdent = Identifier
                                    "Measurement"}
                                NameOriginInSource)]
                          TypeVoid),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "callbacks.h:52:9",
                        fieldName = NamePair {
                          nameC = Name "validate",
                          nameHsIdent = Identifier
                            "measurementHandler_validate"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeFun
                          [
                            TypePointer
                              (TypeStruct
                                NamePair {
                                  nameC = Name "Measurement",
                                  nameHsIdent = Identifier
                                    "Measurement"}
                                NameOriginInSource)]
                          (TypePrim
                            (PrimIntegral PrimInt Signed))),
                      structFieldOffset = 64,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "callbacks.h:53:10",
                        fieldName = NamePair {
                          nameC = Name "onError",
                          nameHsIdent = Identifier
                            "measurementHandler_onError"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeFun
                          [
                            TypePrim
                              (PrimIntegral PrimInt Signed)]
                          TypeVoid),
                      structFieldOffset = 128,
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
                "MeasurementHandler",
              commentLocation = Just
                "callbacks.h:50:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["callbacks.h"],
                  headerInclude = "callbacks.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 24,
          storableAlignment = 8,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "MeasurementHandler",
                  structConstr = Name
                    "@NsConstr"
                    "MeasurementHandler",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "measurementHandler_onReceived",
                      fieldType = HsFunPtr
                        (HsFun
                          (HsPtr
                            (HsTypRef
                              (Name
                                "@NsTypeConstr"
                                "Measurement")))
                          (HsIO (HsPrimType HsPrimUnit))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "callbacks.h:51:10",
                            fieldName = NamePair {
                              nameC = Name "onReceived",
                              nameHsIdent = Identifier
                                "measurementHandler_onReceived"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeFun
                              [
                                TypePointer
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "Measurement",
                                      nameHsIdent = Identifier
                                        "Measurement"}
                                    NameOriginInSource)]
                              TypeVoid),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "onReceived",
                          commentLocation = Just
                            "callbacks.h:51:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "measurementHandler_validate",
                      fieldType = HsFunPtr
                        (HsFun
                          (HsPtr
                            (HsTypRef
                              (Name
                                "@NsTypeConstr"
                                "Measurement")))
                          (HsIO (HsPrimType HsPrimCInt))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "callbacks.h:52:9",
                            fieldName = NamePair {
                              nameC = Name "validate",
                              nameHsIdent = Identifier
                                "measurementHandler_validate"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeFun
                              [
                                TypePointer
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "Measurement",
                                      nameHsIdent = Identifier
                                        "Measurement"}
                                    NameOriginInSource)]
                              (TypePrim
                                (PrimIntegral PrimInt Signed))),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "validate",
                          commentLocation = Just
                            "callbacks.h:52:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "measurementHandler_onError",
                      fieldType = HsFunPtr
                        (HsFun
                          (HsPrimType HsPrimCInt)
                          (HsIO (HsPrimType HsPrimUnit))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "callbacks.h:53:10",
                            fieldName = NamePair {
                              nameC = Name "onError",
                              nameHsIdent = Identifier
                                "measurementHandler_onError"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeFun
                              [
                                TypePrim
                                  (PrimIntegral PrimInt Signed)]
                              TypeVoid),
                          structFieldOffset = 128,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "onError",
                          commentLocation = Just
                            "callbacks.h:53:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "callbacks.h:50:8",
                        declId = NamePair {
                          nameC = Name
                            "MeasurementHandler",
                          nameHsIdent = Identifier
                            "MeasurementHandler"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["callbacks.h"],
                            headerInclude = "callbacks.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "MeasurementHandler"),
                          structSizeof = 24,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "callbacks.h:51:10",
                                fieldName = NamePair {
                                  nameC = Name "onReceived",
                                  nameHsIdent = Identifier
                                    "measurementHandler_onReceived"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeFun
                                  [
                                    TypePointer
                                      (TypeStruct
                                        NamePair {
                                          nameC = Name "Measurement",
                                          nameHsIdent = Identifier
                                            "Measurement"}
                                        NameOriginInSource)]
                                  TypeVoid),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "callbacks.h:52:9",
                                fieldName = NamePair {
                                  nameC = Name "validate",
                                  nameHsIdent = Identifier
                                    "measurementHandler_validate"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeFun
                                  [
                                    TypePointer
                                      (TypeStruct
                                        NamePair {
                                          nameC = Name "Measurement",
                                          nameHsIdent = Identifier
                                            "Measurement"}
                                        NameOriginInSource)]
                                  (TypePrim
                                    (PrimIntegral PrimInt Signed))),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "callbacks.h:53:10",
                                fieldName = NamePair {
                                  nameC = Name "onError",
                                  nameHsIdent = Identifier
                                    "measurementHandler_onError"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeFun
                                  [
                                    TypePrim
                                      (PrimIntegral PrimInt Signed)]
                                  TypeVoid),
                              structFieldOffset = 128,
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
                        "MeasurementHandler",
                      commentLocation = Just
                        "callbacks.h:50:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["callbacks.h"],
                          headerInclude = "callbacks.h"},
                      commentChildren = []}})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 8,
                PeekByteOff (Idx 0) 16]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "MeasurementHandler",
                  structConstr = Name
                    "@NsConstr"
                    "MeasurementHandler",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "measurementHandler_onReceived",
                      fieldType = HsFunPtr
                        (HsFun
                          (HsPtr
                            (HsTypRef
                              (Name
                                "@NsTypeConstr"
                                "Measurement")))
                          (HsIO (HsPrimType HsPrimUnit))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "callbacks.h:51:10",
                            fieldName = NamePair {
                              nameC = Name "onReceived",
                              nameHsIdent = Identifier
                                "measurementHandler_onReceived"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeFun
                              [
                                TypePointer
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "Measurement",
                                      nameHsIdent = Identifier
                                        "Measurement"}
                                    NameOriginInSource)]
                              TypeVoid),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "onReceived",
                          commentLocation = Just
                            "callbacks.h:51:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "measurementHandler_validate",
                      fieldType = HsFunPtr
                        (HsFun
                          (HsPtr
                            (HsTypRef
                              (Name
                                "@NsTypeConstr"
                                "Measurement")))
                          (HsIO (HsPrimType HsPrimCInt))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "callbacks.h:52:9",
                            fieldName = NamePair {
                              nameC = Name "validate",
                              nameHsIdent = Identifier
                                "measurementHandler_validate"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeFun
                              [
                                TypePointer
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "Measurement",
                                      nameHsIdent = Identifier
                                        "Measurement"}
                                    NameOriginInSource)]
                              (TypePrim
                                (PrimIntegral PrimInt Signed))),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "validate",
                          commentLocation = Just
                            "callbacks.h:52:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "measurementHandler_onError",
                      fieldType = HsFunPtr
                        (HsFun
                          (HsPrimType HsPrimCInt)
                          (HsIO (HsPrimType HsPrimUnit))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "callbacks.h:53:10",
                            fieldName = NamePair {
                              nameC = Name "onError",
                              nameHsIdent = Identifier
                                "measurementHandler_onError"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeFun
                              [
                                TypePrim
                                  (PrimIntegral PrimInt Signed)]
                              TypeVoid),
                          structFieldOffset = 128,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "onError",
                          commentLocation = Just
                            "callbacks.h:53:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "callbacks.h:50:8",
                        declId = NamePair {
                          nameC = Name
                            "MeasurementHandler",
                          nameHsIdent = Identifier
                            "MeasurementHandler"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["callbacks.h"],
                            headerInclude = "callbacks.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "MeasurementHandler"),
                          structSizeof = 24,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "callbacks.h:51:10",
                                fieldName = NamePair {
                                  nameC = Name "onReceived",
                                  nameHsIdent = Identifier
                                    "measurementHandler_onReceived"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeFun
                                  [
                                    TypePointer
                                      (TypeStruct
                                        NamePair {
                                          nameC = Name "Measurement",
                                          nameHsIdent = Identifier
                                            "Measurement"}
                                        NameOriginInSource)]
                                  TypeVoid),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "callbacks.h:52:9",
                                fieldName = NamePair {
                                  nameC = Name "validate",
                                  nameHsIdent = Identifier
                                    "measurementHandler_validate"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeFun
                                  [
                                    TypePointer
                                      (TypeStruct
                                        NamePair {
                                          nameC = Name "Measurement",
                                          nameHsIdent = Identifier
                                            "Measurement"}
                                        NameOriginInSource)]
                                  (TypePrim
                                    (PrimIntegral PrimInt Signed))),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "callbacks.h:53:10",
                                fieldName = NamePair {
                                  nameC = Name "onError",
                                  nameHsIdent = Identifier
                                    "measurementHandler_onError"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeFun
                                  [
                                    TypePrim
                                      (PrimIntegral PrimInt Signed)]
                                  TypeVoid),
                              structFieldOffset = 128,
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
                        "MeasurementHandler",
                      commentLocation = Just
                        "callbacks.h:50:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["callbacks.h"],
                          headerInclude = "callbacks.h"},
                      commentChildren = []}}
                (Add 3)
                (Seq
                  [
                    PokeByteOff (Idx 4) 0 (Idx 0),
                    PokeByteOff (Idx 4) 8 (Idx 1),
                    PokeByteOff
                      (Idx 4)
                      16
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
        "MeasurementHandler",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MeasurementHandler",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "DataPipeline",
      structConstr = Name
        "@NsConstr"
        "DataPipeline",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "dataPipeline_preProcess",
          fieldType = HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsFun
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "DataValidator"))
                (HsIO
                  (HsPrimType HsPrimUnit)))),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "callbacks.h:59:10",
                fieldName = NamePair {
                  nameC = Name "preProcess",
                  nameHsIdent = Identifier
                    "dataPipeline_preProcess"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeFun
                  [
                    TypePointer
                      (TypeStruct
                        NamePair {
                          nameC = Name "Measurement",
                          nameHsIdent = Identifier
                            "Measurement"}
                        NameOriginInSource),
                    TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "DataValidator",
                          nameHsIdent = Identifier
                            "DataValidator"}
                        (TypePointer
                          (TypeFun
                            [
                              TypePrim
                                (PrimIntegral PrimInt Signed)]
                            (TypePrim
                              (PrimIntegral
                                PrimInt
                                Signed)))))]
                  TypeVoid),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "preProcess",
              commentLocation = Just
                "callbacks.h:59:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["callbacks.h"],
                  headerInclude = "callbacks.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "dataPipeline_process",
          fieldType = HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsIO (HsPrimType HsPrimUnit))),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "callbacks.h:60:10",
                fieldName = NamePair {
                  nameC = Name "process",
                  nameHsIdent = Identifier
                    "dataPipeline_process"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeFun
                  [
                    TypePointer
                      (TypeStruct
                        NamePair {
                          nameC = Name "Measurement",
                          nameHsIdent = Identifier
                            "Measurement"}
                        NameOriginInSource)]
                  TypeVoid),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "process",
              commentLocation = Just
                "callbacks.h:60:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["callbacks.h"],
                  headerInclude = "callbacks.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "dataPipeline_postProcess",
          fieldType = HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsFun
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "ProgressUpdate"))
                (HsIO
                  (HsPrimType HsPrimUnit)))),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "callbacks.h:61:10",
                fieldName = NamePair {
                  nameC = Name "postProcess",
                  nameHsIdent = Identifier
                    "dataPipeline_postProcess"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeFun
                  [
                    TypePointer
                      (TypeStruct
                        NamePair {
                          nameC = Name "Measurement",
                          nameHsIdent = Identifier
                            "Measurement"}
                        NameOriginInSource),
                    TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "ProgressUpdate",
                          nameHsIdent = Identifier
                            "ProgressUpdate"}
                        (TypePointer
                          (TypeFun
                            [
                              TypePrim
                                (PrimIntegral PrimInt Signed)]
                            TypeVoid)))]
                  TypeVoid),
              structFieldOffset = 128,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "postProcess",
              commentLocation = Just
                "callbacks.h:61:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["callbacks.h"],
                  headerInclude = "callbacks.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "callbacks.h:58:8",
            declId = NamePair {
              nameC = Name "DataPipeline",
              nameHsIdent = Identifier
                "DataPipeline"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["callbacks.h"],
                headerInclude = "callbacks.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name
                  "@NsConstr"
                  "DataPipeline"),
              structSizeof = 24,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "callbacks.h:59:10",
                    fieldName = NamePair {
                      nameC = Name "preProcess",
                      nameHsIdent = Identifier
                        "dataPipeline_preProcess"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeFun
                      [
                        TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = Name "Measurement",
                              nameHsIdent = Identifier
                                "Measurement"}
                            NameOriginInSource),
                        TypeTypedef
                          (TypedefRegular
                            NamePair {
                              nameC = Name "DataValidator",
                              nameHsIdent = Identifier
                                "DataValidator"}
                            (TypePointer
                              (TypeFun
                                [
                                  TypePrim
                                    (PrimIntegral PrimInt Signed)]
                                (TypePrim
                                  (PrimIntegral
                                    PrimInt
                                    Signed)))))]
                      TypeVoid),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "callbacks.h:60:10",
                    fieldName = NamePair {
                      nameC = Name "process",
                      nameHsIdent = Identifier
                        "dataPipeline_process"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeFun
                      [
                        TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = Name "Measurement",
                              nameHsIdent = Identifier
                                "Measurement"}
                            NameOriginInSource)]
                      TypeVoid),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "callbacks.h:61:10",
                    fieldName = NamePair {
                      nameC = Name "postProcess",
                      nameHsIdent = Identifier
                        "dataPipeline_postProcess"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeFun
                      [
                        TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = Name "Measurement",
                              nameHsIdent = Identifier
                                "Measurement"}
                            NameOriginInSource),
                        TypeTypedef
                          (TypedefRegular
                            NamePair {
                              nameC = Name "ProgressUpdate",
                              nameHsIdent = Identifier
                                "ProgressUpdate"}
                            (TypePointer
                              (TypeFun
                                [
                                  TypePrim
                                    (PrimIntegral PrimInt Signed)]
                                TypeVoid)))]
                      TypeVoid),
                  structFieldOffset = 128,
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
            "DataPipeline",
          commentLocation = Just
            "callbacks.h:58:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "DataPipeline",
          structConstr = Name
            "@NsConstr"
            "DataPipeline",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "dataPipeline_preProcess",
              fieldType = HsFunPtr
                (HsFun
                  (HsPtr
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "Measurement")))
                  (HsFun
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "DataValidator"))
                    (HsIO
                      (HsPrimType HsPrimUnit)))),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "callbacks.h:59:10",
                    fieldName = NamePair {
                      nameC = Name "preProcess",
                      nameHsIdent = Identifier
                        "dataPipeline_preProcess"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeFun
                      [
                        TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = Name "Measurement",
                              nameHsIdent = Identifier
                                "Measurement"}
                            NameOriginInSource),
                        TypeTypedef
                          (TypedefRegular
                            NamePair {
                              nameC = Name "DataValidator",
                              nameHsIdent = Identifier
                                "DataValidator"}
                            (TypePointer
                              (TypeFun
                                [
                                  TypePrim
                                    (PrimIntegral PrimInt Signed)]
                                (TypePrim
                                  (PrimIntegral
                                    PrimInt
                                    Signed)))))]
                      TypeVoid),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just
                    "preProcess",
                  commentLocation = Just
                    "callbacks.h:59:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["callbacks.h"],
                      headerInclude = "callbacks.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "dataPipeline_process",
              fieldType = HsFunPtr
                (HsFun
                  (HsPtr
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "Measurement")))
                  (HsIO (HsPrimType HsPrimUnit))),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "callbacks.h:60:10",
                    fieldName = NamePair {
                      nameC = Name "process",
                      nameHsIdent = Identifier
                        "dataPipeline_process"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeFun
                      [
                        TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = Name "Measurement",
                              nameHsIdent = Identifier
                                "Measurement"}
                            NameOriginInSource)]
                      TypeVoid),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "process",
                  commentLocation = Just
                    "callbacks.h:60:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["callbacks.h"],
                      headerInclude = "callbacks.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "dataPipeline_postProcess",
              fieldType = HsFunPtr
                (HsFun
                  (HsPtr
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "Measurement")))
                  (HsFun
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "ProgressUpdate"))
                    (HsIO
                      (HsPrimType HsPrimUnit)))),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "callbacks.h:61:10",
                    fieldName = NamePair {
                      nameC = Name "postProcess",
                      nameHsIdent = Identifier
                        "dataPipeline_postProcess"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeFun
                      [
                        TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = Name "Measurement",
                              nameHsIdent = Identifier
                                "Measurement"}
                            NameOriginInSource),
                        TypeTypedef
                          (TypedefRegular
                            NamePair {
                              nameC = Name "ProgressUpdate",
                              nameHsIdent = Identifier
                                "ProgressUpdate"}
                            (TypePointer
                              (TypeFun
                                [
                                  TypePrim
                                    (PrimIntegral PrimInt Signed)]
                                TypeVoid)))]
                      TypeVoid),
                  structFieldOffset = 128,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just
                    "postProcess",
                  commentLocation = Just
                    "callbacks.h:61:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["callbacks.h"],
                      headerInclude = "callbacks.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "callbacks.h:58:8",
                declId = NamePair {
                  nameC = Name "DataPipeline",
                  nameHsIdent = Identifier
                    "DataPipeline"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["callbacks.h"],
                    headerInclude = "callbacks.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name
                      "@NsConstr"
                      "DataPipeline"),
                  structSizeof = 24,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "callbacks.h:59:10",
                        fieldName = NamePair {
                          nameC = Name "preProcess",
                          nameHsIdent = Identifier
                            "dataPipeline_preProcess"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeFun
                          [
                            TypePointer
                              (TypeStruct
                                NamePair {
                                  nameC = Name "Measurement",
                                  nameHsIdent = Identifier
                                    "Measurement"}
                                NameOriginInSource),
                            TypeTypedef
                              (TypedefRegular
                                NamePair {
                                  nameC = Name "DataValidator",
                                  nameHsIdent = Identifier
                                    "DataValidator"}
                                (TypePointer
                                  (TypeFun
                                    [
                                      TypePrim
                                        (PrimIntegral PrimInt Signed)]
                                    (TypePrim
                                      (PrimIntegral
                                        PrimInt
                                        Signed)))))]
                          TypeVoid),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "callbacks.h:60:10",
                        fieldName = NamePair {
                          nameC = Name "process",
                          nameHsIdent = Identifier
                            "dataPipeline_process"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeFun
                          [
                            TypePointer
                              (TypeStruct
                                NamePair {
                                  nameC = Name "Measurement",
                                  nameHsIdent = Identifier
                                    "Measurement"}
                                NameOriginInSource)]
                          TypeVoid),
                      structFieldOffset = 64,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "callbacks.h:61:10",
                        fieldName = NamePair {
                          nameC = Name "postProcess",
                          nameHsIdent = Identifier
                            "dataPipeline_postProcess"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeFun
                          [
                            TypePointer
                              (TypeStruct
                                NamePair {
                                  nameC = Name "Measurement",
                                  nameHsIdent = Identifier
                                    "Measurement"}
                                NameOriginInSource),
                            TypeTypedef
                              (TypedefRegular
                                NamePair {
                                  nameC = Name "ProgressUpdate",
                                  nameHsIdent = Identifier
                                    "ProgressUpdate"}
                                (TypePointer
                                  (TypeFun
                                    [
                                      TypePrim
                                        (PrimIntegral PrimInt Signed)]
                                    TypeVoid)))]
                          TypeVoid),
                      structFieldOffset = 128,
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
                "DataPipeline",
              commentLocation = Just
                "callbacks.h:58:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["callbacks.h"],
                  headerInclude = "callbacks.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 24,
          storableAlignment = 8,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "DataPipeline",
                  structConstr = Name
                    "@NsConstr"
                    "DataPipeline",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dataPipeline_preProcess",
                      fieldType = HsFunPtr
                        (HsFun
                          (HsPtr
                            (HsTypRef
                              (Name
                                "@NsTypeConstr"
                                "Measurement")))
                          (HsFun
                            (HsTypRef
                              (Name
                                "@NsTypeConstr"
                                "DataValidator"))
                            (HsIO
                              (HsPrimType HsPrimUnit)))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "callbacks.h:59:10",
                            fieldName = NamePair {
                              nameC = Name "preProcess",
                              nameHsIdent = Identifier
                                "dataPipeline_preProcess"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeFun
                              [
                                TypePointer
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "Measurement",
                                      nameHsIdent = Identifier
                                        "Measurement"}
                                    NameOriginInSource),
                                TypeTypedef
                                  (TypedefRegular
                                    NamePair {
                                      nameC = Name "DataValidator",
                                      nameHsIdent = Identifier
                                        "DataValidator"}
                                    (TypePointer
                                      (TypeFun
                                        [
                                          TypePrim
                                            (PrimIntegral PrimInt Signed)]
                                        (TypePrim
                                          (PrimIntegral
                                            PrimInt
                                            Signed)))))]
                              TypeVoid),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "preProcess",
                          commentLocation = Just
                            "callbacks.h:59:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dataPipeline_process",
                      fieldType = HsFunPtr
                        (HsFun
                          (HsPtr
                            (HsTypRef
                              (Name
                                "@NsTypeConstr"
                                "Measurement")))
                          (HsIO (HsPrimType HsPrimUnit))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "callbacks.h:60:10",
                            fieldName = NamePair {
                              nameC = Name "process",
                              nameHsIdent = Identifier
                                "dataPipeline_process"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeFun
                              [
                                TypePointer
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "Measurement",
                                      nameHsIdent = Identifier
                                        "Measurement"}
                                    NameOriginInSource)]
                              TypeVoid),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "process",
                          commentLocation = Just
                            "callbacks.h:60:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dataPipeline_postProcess",
                      fieldType = HsFunPtr
                        (HsFun
                          (HsPtr
                            (HsTypRef
                              (Name
                                "@NsTypeConstr"
                                "Measurement")))
                          (HsFun
                            (HsTypRef
                              (Name
                                "@NsTypeConstr"
                                "ProgressUpdate"))
                            (HsIO
                              (HsPrimType HsPrimUnit)))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "callbacks.h:61:10",
                            fieldName = NamePair {
                              nameC = Name "postProcess",
                              nameHsIdent = Identifier
                                "dataPipeline_postProcess"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeFun
                              [
                                TypePointer
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "Measurement",
                                      nameHsIdent = Identifier
                                        "Measurement"}
                                    NameOriginInSource),
                                TypeTypedef
                                  (TypedefRegular
                                    NamePair {
                                      nameC = Name "ProgressUpdate",
                                      nameHsIdent = Identifier
                                        "ProgressUpdate"}
                                    (TypePointer
                                      (TypeFun
                                        [
                                          TypePrim
                                            (PrimIntegral PrimInt Signed)]
                                        TypeVoid)))]
                              TypeVoid),
                          structFieldOffset = 128,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "postProcess",
                          commentLocation = Just
                            "callbacks.h:61:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "callbacks.h:58:8",
                        declId = NamePair {
                          nameC = Name "DataPipeline",
                          nameHsIdent = Identifier
                            "DataPipeline"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["callbacks.h"],
                            headerInclude = "callbacks.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "DataPipeline"),
                          structSizeof = 24,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "callbacks.h:59:10",
                                fieldName = NamePair {
                                  nameC = Name "preProcess",
                                  nameHsIdent = Identifier
                                    "dataPipeline_preProcess"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeFun
                                  [
                                    TypePointer
                                      (TypeStruct
                                        NamePair {
                                          nameC = Name "Measurement",
                                          nameHsIdent = Identifier
                                            "Measurement"}
                                        NameOriginInSource),
                                    TypeTypedef
                                      (TypedefRegular
                                        NamePair {
                                          nameC = Name "DataValidator",
                                          nameHsIdent = Identifier
                                            "DataValidator"}
                                        (TypePointer
                                          (TypeFun
                                            [
                                              TypePrim
                                                (PrimIntegral PrimInt Signed)]
                                            (TypePrim
                                              (PrimIntegral
                                                PrimInt
                                                Signed)))))]
                                  TypeVoid),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "callbacks.h:60:10",
                                fieldName = NamePair {
                                  nameC = Name "process",
                                  nameHsIdent = Identifier
                                    "dataPipeline_process"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeFun
                                  [
                                    TypePointer
                                      (TypeStruct
                                        NamePair {
                                          nameC = Name "Measurement",
                                          nameHsIdent = Identifier
                                            "Measurement"}
                                        NameOriginInSource)]
                                  TypeVoid),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "callbacks.h:61:10",
                                fieldName = NamePair {
                                  nameC = Name "postProcess",
                                  nameHsIdent = Identifier
                                    "dataPipeline_postProcess"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeFun
                                  [
                                    TypePointer
                                      (TypeStruct
                                        NamePair {
                                          nameC = Name "Measurement",
                                          nameHsIdent = Identifier
                                            "Measurement"}
                                        NameOriginInSource),
                                    TypeTypedef
                                      (TypedefRegular
                                        NamePair {
                                          nameC = Name "ProgressUpdate",
                                          nameHsIdent = Identifier
                                            "ProgressUpdate"}
                                        (TypePointer
                                          (TypeFun
                                            [
                                              TypePrim
                                                (PrimIntegral PrimInt Signed)]
                                            TypeVoid)))]
                                  TypeVoid),
                              structFieldOffset = 128,
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
                        "DataPipeline",
                      commentLocation = Just
                        "callbacks.h:58:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["callbacks.h"],
                          headerInclude = "callbacks.h"},
                      commentChildren = []}})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 8,
                PeekByteOff (Idx 0) 16]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "DataPipeline",
                  structConstr = Name
                    "@NsConstr"
                    "DataPipeline",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dataPipeline_preProcess",
                      fieldType = HsFunPtr
                        (HsFun
                          (HsPtr
                            (HsTypRef
                              (Name
                                "@NsTypeConstr"
                                "Measurement")))
                          (HsFun
                            (HsTypRef
                              (Name
                                "@NsTypeConstr"
                                "DataValidator"))
                            (HsIO
                              (HsPrimType HsPrimUnit)))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "callbacks.h:59:10",
                            fieldName = NamePair {
                              nameC = Name "preProcess",
                              nameHsIdent = Identifier
                                "dataPipeline_preProcess"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeFun
                              [
                                TypePointer
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "Measurement",
                                      nameHsIdent = Identifier
                                        "Measurement"}
                                    NameOriginInSource),
                                TypeTypedef
                                  (TypedefRegular
                                    NamePair {
                                      nameC = Name "DataValidator",
                                      nameHsIdent = Identifier
                                        "DataValidator"}
                                    (TypePointer
                                      (TypeFun
                                        [
                                          TypePrim
                                            (PrimIntegral PrimInt Signed)]
                                        (TypePrim
                                          (PrimIntegral
                                            PrimInt
                                            Signed)))))]
                              TypeVoid),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "preProcess",
                          commentLocation = Just
                            "callbacks.h:59:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dataPipeline_process",
                      fieldType = HsFunPtr
                        (HsFun
                          (HsPtr
                            (HsTypRef
                              (Name
                                "@NsTypeConstr"
                                "Measurement")))
                          (HsIO (HsPrimType HsPrimUnit))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "callbacks.h:60:10",
                            fieldName = NamePair {
                              nameC = Name "process",
                              nameHsIdent = Identifier
                                "dataPipeline_process"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeFun
                              [
                                TypePointer
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "Measurement",
                                      nameHsIdent = Identifier
                                        "Measurement"}
                                    NameOriginInSource)]
                              TypeVoid),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "process",
                          commentLocation = Just
                            "callbacks.h:60:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "dataPipeline_postProcess",
                      fieldType = HsFunPtr
                        (HsFun
                          (HsPtr
                            (HsTypRef
                              (Name
                                "@NsTypeConstr"
                                "Measurement")))
                          (HsFun
                            (HsTypRef
                              (Name
                                "@NsTypeConstr"
                                "ProgressUpdate"))
                            (HsIO
                              (HsPrimType HsPrimUnit)))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "callbacks.h:61:10",
                            fieldName = NamePair {
                              nameC = Name "postProcess",
                              nameHsIdent = Identifier
                                "dataPipeline_postProcess"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeFun
                              [
                                TypePointer
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "Measurement",
                                      nameHsIdent = Identifier
                                        "Measurement"}
                                    NameOriginInSource),
                                TypeTypedef
                                  (TypedefRegular
                                    NamePair {
                                      nameC = Name "ProgressUpdate",
                                      nameHsIdent = Identifier
                                        "ProgressUpdate"}
                                    (TypePointer
                                      (TypeFun
                                        [
                                          TypePrim
                                            (PrimIntegral PrimInt Signed)]
                                        TypeVoid)))]
                              TypeVoid),
                          structFieldOffset = 128,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "postProcess",
                          commentLocation = Just
                            "callbacks.h:61:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "callbacks.h:58:8",
                        declId = NamePair {
                          nameC = Name "DataPipeline",
                          nameHsIdent = Identifier
                            "DataPipeline"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["callbacks.h"],
                            headerInclude = "callbacks.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "DataPipeline"),
                          structSizeof = 24,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "callbacks.h:59:10",
                                fieldName = NamePair {
                                  nameC = Name "preProcess",
                                  nameHsIdent = Identifier
                                    "dataPipeline_preProcess"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeFun
                                  [
                                    TypePointer
                                      (TypeStruct
                                        NamePair {
                                          nameC = Name "Measurement",
                                          nameHsIdent = Identifier
                                            "Measurement"}
                                        NameOriginInSource),
                                    TypeTypedef
                                      (TypedefRegular
                                        NamePair {
                                          nameC = Name "DataValidator",
                                          nameHsIdent = Identifier
                                            "DataValidator"}
                                        (TypePointer
                                          (TypeFun
                                            [
                                              TypePrim
                                                (PrimIntegral PrimInt Signed)]
                                            (TypePrim
                                              (PrimIntegral
                                                PrimInt
                                                Signed)))))]
                                  TypeVoid),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "callbacks.h:60:10",
                                fieldName = NamePair {
                                  nameC = Name "process",
                                  nameHsIdent = Identifier
                                    "dataPipeline_process"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeFun
                                  [
                                    TypePointer
                                      (TypeStruct
                                        NamePair {
                                          nameC = Name "Measurement",
                                          nameHsIdent = Identifier
                                            "Measurement"}
                                        NameOriginInSource)]
                                  TypeVoid),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "callbacks.h:61:10",
                                fieldName = NamePair {
                                  nameC = Name "postProcess",
                                  nameHsIdent = Identifier
                                    "dataPipeline_postProcess"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeFun
                                  [
                                    TypePointer
                                      (TypeStruct
                                        NamePair {
                                          nameC = Name "Measurement",
                                          nameHsIdent = Identifier
                                            "Measurement"}
                                        NameOriginInSource),
                                    TypeTypedef
                                      (TypedefRegular
                                        NamePair {
                                          nameC = Name "ProgressUpdate",
                                          nameHsIdent = Identifier
                                            "ProgressUpdate"}
                                        (TypePointer
                                          (TypeFun
                                            [
                                              TypePrim
                                                (PrimIntegral PrimInt Signed)]
                                            TypeVoid)))]
                                  TypeVoid),
                              structFieldOffset = 128,
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
                        "DataPipeline",
                      commentLocation = Just
                        "callbacks.h:58:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["callbacks.h"],
                          headerInclude = "callbacks.h"},
                      commentChildren = []}}
                (Add 3)
                (Seq
                  [
                    PokeByteOff (Idx 4) 0 (Idx 0),
                    PokeByteOff (Idx 4) 8 (Idx 1),
                    PokeByteOff
                      (Idx 4)
                      16
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
        "DataPipeline",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "DataPipeline",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "ProcessorCallback",
      newtypeConstr = Name
        "@NsConstr"
        "ProcessorCallback",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_ProcessorCallback",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "callbacks.h:69:7",
          declId = NamePair {
            nameC = Name
              "ProcessorCallback",
            nameHsIdent = Identifier
              "ProcessorCallback"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          declComment = Nothing},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "ProcessorCallback",
              newtypeField = Name
                "@NsVar"
                "un_ProcessorCallback"},
            unionSizeof = 8,
            unionAlignment = 8,
            unionFields = [
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc = "callbacks.h:70:10",
                  fieldName = NamePair {
                    nameC = Name "simple",
                    nameHsIdent = Identifier
                      "processorCallback_simple"},
                  fieldComment = Nothing},
                unionFieldType = TypePointer
                  (TypeFun
                    [
                      TypePointer
                        (TypeStruct
                          NamePair {
                            nameC = Name "Measurement",
                            nameHsIdent = Identifier
                              "Measurement"}
                          NameOriginInSource)]
                    TypeVoid)},
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc = "callbacks.h:71:10",
                  fieldName = NamePair {
                    nameC = Name "withValidator",
                    nameHsIdent = Identifier
                      "processorCallback_withValidator"},
                  fieldComment = Nothing},
                unionFieldType = TypePointer
                  (TypeFun
                    [
                      TypePointer
                        (TypeStruct
                          NamePair {
                            nameC = Name "Measurement",
                            nameHsIdent = Identifier
                              "Measurement"}
                          NameOriginInSource),
                      TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "DataValidator",
                            nameHsIdent = Identifier
                              "DataValidator"}
                          (TypePointer
                            (TypeFun
                              [
                                TypePrim
                                  (PrimIntegral PrimInt Signed)]
                              (TypePrim
                                (PrimIntegral
                                  PrimInt
                                  Signed)))))]
                    TypeVoid)},
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc = "callbacks.h:72:10",
                  fieldName = NamePair {
                    nameC = Name "withProgress",
                    nameHsIdent = Identifier
                      "processorCallback_withProgress"},
                  fieldComment = Nothing},
                unionFieldType = TypePointer
                  (TypeFun
                    [
                      TypePointer
                        (TypeStruct
                          NamePair {
                            nameC = Name "Measurement",
                            nameHsIdent = Identifier
                              "Measurement"}
                          NameOriginInSource),
                      TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "ProgressUpdate",
                            nameHsIdent = Identifier
                              "ProgressUpdate"}
                          (TypePointer
                            (TypeFun
                              [
                                TypePrim
                                  (PrimIntegral PrimInt Signed)]
                              TypeVoid)))]
                    TypeVoid)}]},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ProcessorCallback",
          commentLocation = Just
            "callbacks.h:69:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveVia
        (HsSizedByteArray 8 8),
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ProcessorCallback",
      deriveInstanceComment =
      Nothing},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = Name
        "@NsVar"
        "get_processorCallback_simple",
      unionGetterType = HsFunPtr
        (HsFun
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Measurement")))
          (HsIO (HsPrimType HsPrimUnit))),
      unionGetterConstr = Name
        "@NsTypeConstr"
        "ProcessorCallback",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "simple",
          commentLocation = Just
            "callbacks.h:70:10",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_processorCallback_simple"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = Name
        "@NsVar"
        "set_processorCallback_simple",
      unionSetterType = HsFunPtr
        (HsFun
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Measurement")))
          (HsIO (HsPrimType HsPrimUnit))),
      unionSetterConstr = Name
        "@NsTypeConstr"
        "ProcessorCallback",
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
                  "get_processorCallback_simple"]]}},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = Name
        "@NsVar"
        "get_processorCallback_withValidator",
      unionGetterType = HsFunPtr
        (HsFun
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Measurement")))
          (HsFun
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "DataValidator"))
            (HsIO
              (HsPrimType HsPrimUnit)))),
      unionGetterConstr = Name
        "@NsTypeConstr"
        "ProcessorCallback",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "withValidator",
          commentLocation = Just
            "callbacks.h:71:10",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_processorCallback_withValidator"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = Name
        "@NsVar"
        "set_processorCallback_withValidator",
      unionSetterType = HsFunPtr
        (HsFun
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Measurement")))
          (HsFun
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "DataValidator"))
            (HsIO
              (HsPrimType HsPrimUnit)))),
      unionSetterConstr = Name
        "@NsTypeConstr"
        "ProcessorCallback",
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
                  "get_processorCallback_withValidator"]]}},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = Name
        "@NsVar"
        "get_processorCallback_withProgress",
      unionGetterType = HsFunPtr
        (HsFun
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Measurement")))
          (HsFun
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "ProgressUpdate"))
            (HsIO
              (HsPrimType HsPrimUnit)))),
      unionGetterConstr = Name
        "@NsTypeConstr"
        "ProcessorCallback",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "withProgress",
          commentLocation = Just
            "callbacks.h:72:10",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_processorCallback_withProgress"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = Name
        "@NsVar"
        "set_processorCallback_withProgress",
      unionSetterType = HsFunPtr
        (HsFun
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Measurement")))
          (HsFun
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "ProgressUpdate"))
            (HsIO
              (HsPrimType HsPrimUnit)))),
      unionSetterConstr = Name
        "@NsTypeConstr"
        "ProcessorCallback",
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
                  "get_processorCallback_withProgress"]]}},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Processor_mode",
      newtypeConstr = Name
        "@NsConstr"
        "Processor_mode",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Processor_mode",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "callbacks.h:76:3",
          declId = NamePair {
            nameC = Name "Processor_mode",
            nameHsIdent = Identifier
              "Processor_mode"},
          declOrigin = NameOriginGenerated
            (AnonId "callbacks.h:76:3"),
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          declComment = Nothing},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Processor_mode",
              newtypeField = Name
                "@NsVar"
                "un_Processor_mode"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "callbacks.h:76:10",
                  fieldName = NamePair {
                    nameC = Name "MODE_SIMPLE",
                    nameHsIdent = Identifier
                      "MODE_SIMPLE"},
                  fieldComment = Nothing},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "callbacks.h:76:23",
                  fieldName = NamePair {
                    nameC = Name "MODE_VALIDATED",
                    nameHsIdent = Identifier
                      "MODE_VALIDATED"},
                  fieldComment = Nothing},
                enumConstantValue = 1},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "callbacks.h:76:39",
                  fieldName = NamePair {
                    nameC = Name "MODE_PROGRESS",
                    nameHsIdent = Identifier
                      "MODE_PROGRESS"},
                  fieldComment = Nothing},
                enumConstantValue = 2}]},
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
            "callbacks.h:76:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Processor_mode",
          structConstr = Name
            "@NsConstr"
            "Processor_mode",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_Processor_mode",
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
                    "Processor_mode",
                  structConstr = Name
                    "@NsConstr"
                    "Processor_mode",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "un_Processor_mode",
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
                    "Processor_mode",
                  structConstr = Name
                    "@NsConstr"
                    "Processor_mode",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "un_Processor_mode",
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
        "Processor_mode",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Processor_mode",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Processor_mode",
          structConstr = Name
            "@NsConstr"
            "Processor_mode",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_Processor_mode",
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
              (NE.fromList ["MODE_SIMPLE"]),
            _×_
              1
              (NE.fromList
                ["MODE_VALIDATED"]),
            _×_
              2
              (NE.fromList
                ["MODE_PROGRESS"])])
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
            "Processor_mode",
          structConstr = Name
            "@NsConstr"
            "Processor_mode",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_Processor_mode",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (Name "@NsConstr" "MODE_SIMPLE")
        (Name
          "@NsConstr"
          "MODE_PROGRESS"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Processor_mode",
          structConstr = Name
            "@NsConstr"
            "Processor_mode",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_Processor_mode",
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
            "Processor_mode",
          structConstr = Name
            "@NsConstr"
            "Processor_mode",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_Processor_mode",
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
        "MODE_SIMPLE",
      patSynType = Name
        "@NsTypeConstr"
        "Processor_mode",
      patSynConstr = Name
        "@NsConstr"
        "Processor_mode",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "callbacks.h:76:10",
            fieldName = NamePair {
              nameC = Name "MODE_SIMPLE",
              nameHsIdent = Identifier
                "MODE_SIMPLE"},
            fieldComment = Nothing},
          enumConstantValue = 0},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "MODE_SIMPLE",
          commentLocation = Just
            "callbacks.h:76:10",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []}},
  DeclPatSyn
    PatSyn {
      patSynName = Name
        "@NsConstr"
        "MODE_VALIDATED",
      patSynType = Name
        "@NsTypeConstr"
        "Processor_mode",
      patSynConstr = Name
        "@NsConstr"
        "Processor_mode",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "callbacks.h:76:23",
            fieldName = NamePair {
              nameC = Name "MODE_VALIDATED",
              nameHsIdent = Identifier
                "MODE_VALIDATED"},
            fieldComment = Nothing},
          enumConstantValue = 1},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "MODE_VALIDATED",
          commentLocation = Just
            "callbacks.h:76:23",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []}},
  DeclPatSyn
    PatSyn {
      patSynName = Name
        "@NsConstr"
        "MODE_PROGRESS",
      patSynType = Name
        "@NsTypeConstr"
        "Processor_mode",
      patSynConstr = Name
        "@NsConstr"
        "Processor_mode",
      patSynValue = 2,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "callbacks.h:76:39",
            fieldName = NamePair {
              nameC = Name "MODE_PROGRESS",
              nameHsIdent = Identifier
                "MODE_PROGRESS"},
            fieldComment = Nothing},
          enumConstantValue = 2},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "MODE_PROGRESS",
          commentLocation = Just
            "callbacks.h:76:39",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []}},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Processor",
      structConstr = Name
        "@NsConstr"
        "Processor",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "processor_mode",
          fieldType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Processor_mode"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "callbacks.h:76:55",
                fieldName = NamePair {
                  nameC = Name "mode",
                  nameHsIdent = Identifier
                    "processor_mode"},
                fieldComment = Nothing},
              structFieldType = TypeEnum
                NamePair {
                  nameC = Name "Processor_mode",
                  nameHsIdent = Identifier
                    "Processor_mode"}
                (NameOriginGenerated
                  (AnonId "callbacks.h:76:3")),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "mode",
              commentLocation = Just
                "callbacks.h:76:55",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["callbacks.h"],
                  headerInclude = "callbacks.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "processor_callback",
          fieldType = HsTypRef
            (Name
              "@NsTypeConstr"
              "ProcessorCallback"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "callbacks.h:77:27",
                fieldName = NamePair {
                  nameC = Name "callback",
                  nameHsIdent = Identifier
                    "processor_callback"},
                fieldComment = Nothing},
              structFieldType = TypeUnion
                NamePair {
                  nameC = Name
                    "ProcessorCallback",
                  nameHsIdent = Identifier
                    "ProcessorCallback"}
                NameOriginInSource,
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "callback",
              commentLocation = Just
                "callbacks.h:77:27",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["callbacks.h"],
                  headerInclude = "callbacks.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "callbacks.h:75:8",
            declId = NamePair {
              nameC = Name "Processor",
              nameHsIdent = Identifier
                "Processor"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["callbacks.h"],
                headerInclude = "callbacks.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Processor"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "callbacks.h:76:55",
                    fieldName = NamePair {
                      nameC = Name "mode",
                      nameHsIdent = Identifier
                        "processor_mode"},
                    fieldComment = Nothing},
                  structFieldType = TypeEnum
                    NamePair {
                      nameC = Name "Processor_mode",
                      nameHsIdent = Identifier
                        "Processor_mode"}
                    (NameOriginGenerated
                      (AnonId "callbacks.h:76:3")),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "callbacks.h:77:27",
                    fieldName = NamePair {
                      nameC = Name "callback",
                      nameHsIdent = Identifier
                        "processor_callback"},
                    fieldComment = Nothing},
                  structFieldType = TypeUnion
                    NamePair {
                      nameC = Name
                        "ProcessorCallback",
                      nameHsIdent = Identifier
                        "ProcessorCallback"}
                    NameOriginInSource,
                  structFieldOffset = 64,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecModule = Nothing,
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
      structInstances = Set.fromList
        [Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "Processor",
          commentLocation = Just
            "callbacks.h:75:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Processor",
          structConstr = Name
            "@NsConstr"
            "Processor",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "processor_mode",
              fieldType = HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Processor_mode"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "callbacks.h:76:55",
                    fieldName = NamePair {
                      nameC = Name "mode",
                      nameHsIdent = Identifier
                        "processor_mode"},
                    fieldComment = Nothing},
                  structFieldType = TypeEnum
                    NamePair {
                      nameC = Name "Processor_mode",
                      nameHsIdent = Identifier
                        "Processor_mode"}
                    (NameOriginGenerated
                      (AnonId "callbacks.h:76:3")),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "mode",
                  commentLocation = Just
                    "callbacks.h:76:55",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["callbacks.h"],
                      headerInclude = "callbacks.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "processor_callback",
              fieldType = HsTypRef
                (Name
                  "@NsTypeConstr"
                  "ProcessorCallback"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "callbacks.h:77:27",
                    fieldName = NamePair {
                      nameC = Name "callback",
                      nameHsIdent = Identifier
                        "processor_callback"},
                    fieldComment = Nothing},
                  structFieldType = TypeUnion
                    NamePair {
                      nameC = Name
                        "ProcessorCallback",
                      nameHsIdent = Identifier
                        "ProcessorCallback"}
                    NameOriginInSource,
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "callback",
                  commentLocation = Just
                    "callbacks.h:77:27",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["callbacks.h"],
                      headerInclude = "callbacks.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "callbacks.h:75:8",
                declId = NamePair {
                  nameC = Name "Processor",
                  nameHsIdent = Identifier
                    "Processor"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["callbacks.h"],
                    headerInclude = "callbacks.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Processor"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "callbacks.h:76:55",
                        fieldName = NamePair {
                          nameC = Name "mode",
                          nameHsIdent = Identifier
                            "processor_mode"},
                        fieldComment = Nothing},
                      structFieldType = TypeEnum
                        NamePair {
                          nameC = Name "Processor_mode",
                          nameHsIdent = Identifier
                            "Processor_mode"}
                        (NameOriginGenerated
                          (AnonId "callbacks.h:76:3")),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "callbacks.h:77:27",
                        fieldName = NamePair {
                          nameC = Name "callback",
                          nameHsIdent = Identifier
                            "processor_callback"},
                        fieldComment = Nothing},
                      structFieldType = TypeUnion
                        NamePair {
                          nameC = Name
                            "ProcessorCallback",
                          nameHsIdent = Identifier
                            "ProcessorCallback"}
                        NameOriginInSource,
                      structFieldOffset = 64,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecModule = Nothing,
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
          structInstances = Set.fromList
            [Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "Processor",
              commentLocation = Just
                "callbacks.h:75:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["callbacks.h"],
                  headerInclude = "callbacks.h"},
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
                    "Processor",
                  structConstr = Name
                    "@NsConstr"
                    "Processor",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "processor_mode",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Processor_mode"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "callbacks.h:76:55",
                            fieldName = NamePair {
                              nameC = Name "mode",
                              nameHsIdent = Identifier
                                "processor_mode"},
                            fieldComment = Nothing},
                          structFieldType = TypeEnum
                            NamePair {
                              nameC = Name "Processor_mode",
                              nameHsIdent = Identifier
                                "Processor_mode"}
                            (NameOriginGenerated
                              (AnonId "callbacks.h:76:3")),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "mode",
                          commentLocation = Just
                            "callbacks.h:76:55",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "processor_callback",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "ProcessorCallback"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "callbacks.h:77:27",
                            fieldName = NamePair {
                              nameC = Name "callback",
                              nameHsIdent = Identifier
                                "processor_callback"},
                            fieldComment = Nothing},
                          structFieldType = TypeUnion
                            NamePair {
                              nameC = Name
                                "ProcessorCallback",
                              nameHsIdent = Identifier
                                "ProcessorCallback"}
                            NameOriginInSource,
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "callback",
                          commentLocation = Just
                            "callbacks.h:77:27",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "callbacks.h:75:8",
                        declId = NamePair {
                          nameC = Name "Processor",
                          nameHsIdent = Identifier
                            "Processor"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["callbacks.h"],
                            headerInclude = "callbacks.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Processor"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "callbacks.h:76:55",
                                fieldName = NamePair {
                                  nameC = Name "mode",
                                  nameHsIdent = Identifier
                                    "processor_mode"},
                                fieldComment = Nothing},
                              structFieldType = TypeEnum
                                NamePair {
                                  nameC = Name "Processor_mode",
                                  nameHsIdent = Identifier
                                    "Processor_mode"}
                                (NameOriginGenerated
                                  (AnonId "callbacks.h:76:3")),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "callbacks.h:77:27",
                                fieldName = NamePair {
                                  nameC = Name "callback",
                                  nameHsIdent = Identifier
                                    "processor_callback"},
                                fieldComment = Nothing},
                              structFieldType = TypeUnion
                                NamePair {
                                  nameC = Name
                                    "ProcessorCallback",
                                  nameHsIdent = Identifier
                                    "ProcessorCallback"}
                                NameOriginInSource,
                              structFieldOffset = 64,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "Processor",
                      commentLocation = Just
                        "callbacks.h:75:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["callbacks.h"],
                          headerInclude = "callbacks.h"},
                      commentChildren = []}})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 8]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Processor",
                  structConstr = Name
                    "@NsConstr"
                    "Processor",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "processor_mode",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Processor_mode"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "callbacks.h:76:55",
                            fieldName = NamePair {
                              nameC = Name "mode",
                              nameHsIdent = Identifier
                                "processor_mode"},
                            fieldComment = Nothing},
                          structFieldType = TypeEnum
                            NamePair {
                              nameC = Name "Processor_mode",
                              nameHsIdent = Identifier
                                "Processor_mode"}
                            (NameOriginGenerated
                              (AnonId "callbacks.h:76:3")),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "mode",
                          commentLocation = Just
                            "callbacks.h:76:55",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "processor_callback",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "ProcessorCallback"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "callbacks.h:77:27",
                            fieldName = NamePair {
                              nameC = Name "callback",
                              nameHsIdent = Identifier
                                "processor_callback"},
                            fieldComment = Nothing},
                          structFieldType = TypeUnion
                            NamePair {
                              nameC = Name
                                "ProcessorCallback",
                              nameHsIdent = Identifier
                                "ProcessorCallback"}
                            NameOriginInSource,
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "callback",
                          commentLocation = Just
                            "callbacks.h:77:27",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "callbacks.h:75:8",
                        declId = NamePair {
                          nameC = Name "Processor",
                          nameHsIdent = Identifier
                            "Processor"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["callbacks.h"],
                            headerInclude = "callbacks.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Processor"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "callbacks.h:76:55",
                                fieldName = NamePair {
                                  nameC = Name "mode",
                                  nameHsIdent = Identifier
                                    "processor_mode"},
                                fieldComment = Nothing},
                              structFieldType = TypeEnum
                                NamePair {
                                  nameC = Name "Processor_mode",
                                  nameHsIdent = Identifier
                                    "Processor_mode"}
                                (NameOriginGenerated
                                  (AnonId "callbacks.h:76:3")),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "callbacks.h:77:27",
                                fieldName = NamePair {
                                  nameC = Name "callback",
                                  nameHsIdent = Identifier
                                    "processor_callback"},
                                fieldComment = Nothing},
                              structFieldType = TypeUnion
                                NamePair {
                                  nameC = Name
                                    "ProcessorCallback",
                                  nameHsIdent = Identifier
                                    "ProcessorCallback"}
                                NameOriginInSource,
                              structFieldOffset = 64,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "Processor",
                      commentLocation = Just
                        "callbacks.h:75:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["callbacks.h"],
                          headerInclude = "callbacks.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      8
                      (Idx 1)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Foo",
      newtypeConstr = Name
        "@NsConstr"
        "Foo",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Foo",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "callbacks.h:94:13",
          declId = NamePair {
            nameC = Name "foo",
            nameHsIdent = Identifier "Foo"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Foo",
              newtypeField = Name
                "@NsVar"
                "un_Foo"},
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
          commentOrigin = Just "foo",
          commentLocation = Just
            "callbacks.h:94:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
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
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo",
      deriveInstanceComment =
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
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo",
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
        "Foo",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Foo2",
      newtypeConstr = Name
        "@NsConstr"
        "Foo2",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Foo2",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "callbacks.h:95:13",
          declId = NamePair {
            nameC = Name "foo2",
            nameHsIdent = Identifier
              "Foo2"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Foo2",
              newtypeField = Name
                "@NsVar"
                "un_Foo2"},
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
          commentOrigin = Just "foo2",
          commentLocation = Just
            "callbacks.h:95:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo2",
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
        "Foo2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo2",
      deriveInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "funPtr_7a7e54ab_to",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFun
            (HsTypRef
              (Name "@NsTypeConstr" "Foo"))
            (HsIO (HsPrimType HsPrimUnit)),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "Foo"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = ToFunPtr
        (TypePointer
          (TypeFun
            [
              TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "foo",
                    nameHsIdent = Identifier "Foo"}
                  (TypePrim
                    (PrimIntegral PrimInt Signed)))]
            TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "funPtr_7a7e54ab_from",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "Foo"))
              (HsIO (HsPrimType HsPrimUnit))),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsFun
          (HsTypRef
            (Name "@NsTypeConstr" "Foo"))
          (HsIO (HsPrimType HsPrimUnit))),
      foreignImportOrigName =
      "dynamic",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = FromFunPtr
        (TypePointer
          (TypeFun
            [
              TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "foo",
                    nameHsIdent = Identifier "Foo"}
                  (TypePrim
                    (PrimIntegral PrimInt Signed)))]
            TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceToFunPtr
        ToFunPtrInstance {
          toFunPtrInstanceType = HsFun
            (HsTypRef
              (Name "@NsTypeConstr" "Foo"))
            (HsIO (HsPrimType HsPrimUnit)),
          toFunPtrInstanceBody = Name
            "@NsVar"
            "funPtr_7a7e54ab_to"},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceFromFunPtr
        FromFunPtrInstance {
          fromFunPtrInstanceType = HsFun
            (HsTypRef
              (Name "@NsTypeConstr" "Foo"))
            (HsIO (HsPrimType HsPrimUnit)),
          fromFunPtrInstanceBody = Name
            "@NsVar"
            "funPtr_7a7e54ab_from"},
      defineInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "funPtr_527712e4_to",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsIO (HsPrimType HsPrimCInt)),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = ToFunPtr
        (TypePointer
          (TypeFun
            [
              TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource)]
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "funPtr_527712e4_from",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsIO (HsPrimType HsPrimCInt))),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsFun
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Measurement")))
          (HsIO (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "dynamic",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = FromFunPtr
        (TypePointer
          (TypeFun
            [
              TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource)]
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceToFunPtr
        ToFunPtrInstance {
          toFunPtrInstanceType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsIO (HsPrimType HsPrimCInt)),
          toFunPtrInstanceBody = Name
            "@NsVar"
            "funPtr_527712e4_to"},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceFromFunPtr
        FromFunPtrInstance {
          fromFunPtrInstanceType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsIO (HsPrimType HsPrimCInt)),
          fromFunPtrInstanceBody = Name
            "@NsVar"
            "funPtr_527712e4_from"},
      defineInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "funPtr_2cc53fd3_to",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsIO (HsPrimType HsPrimUnit)),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = ToFunPtr
        (TypePointer
          (TypeFun
            [
              TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource)]
            TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "funPtr_2cc53fd3_from",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsIO (HsPrimType HsPrimUnit))),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsFun
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Measurement")))
          (HsIO (HsPrimType HsPrimUnit))),
      foreignImportOrigName =
      "dynamic",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = FromFunPtr
        (TypePointer
          (TypeFun
            [
              TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource)]
            TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceToFunPtr
        ToFunPtrInstance {
          toFunPtrInstanceType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsIO (HsPrimType HsPrimUnit)),
          toFunPtrInstanceBody = Name
            "@NsVar"
            "funPtr_2cc53fd3_to"},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceFromFunPtr
        FromFunPtrInstance {
          fromFunPtrInstanceType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsIO (HsPrimType HsPrimUnit)),
          fromFunPtrInstanceBody = Name
            "@NsVar"
            "funPtr_2cc53fd3_from"},
      defineInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "funPtr_6f6352e1_to",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "DataValidator"))
              (HsIO (HsPrimType HsPrimUnit))),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsFun
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "DataValidator"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = ToFunPtr
        (TypePointer
          (TypeFun
            [
              TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource),
              TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "DataValidator",
                    nameHsIdent = Identifier
                      "DataValidator"}
                  (TypePointer
                    (TypeFun
                      [
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed)))))]
            TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "funPtr_6f6352e1_from",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsFun
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "DataValidator"))
                (HsIO
                  (HsPrimType HsPrimUnit)))),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsFun
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Measurement")))
          (HsFun
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "DataValidator"))
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "dynamic",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = FromFunPtr
        (TypePointer
          (TypeFun
            [
              TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource),
              TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "DataValidator",
                    nameHsIdent = Identifier
                      "DataValidator"}
                  (TypePointer
                    (TypeFun
                      [
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed)))))]
            TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceToFunPtr
        ToFunPtrInstance {
          toFunPtrInstanceType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "DataValidator"))
              (HsIO (HsPrimType HsPrimUnit))),
          toFunPtrInstanceBody = Name
            "@NsVar"
            "funPtr_6f6352e1_to"},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceFromFunPtr
        FromFunPtrInstance {
          fromFunPtrInstanceType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "DataValidator"))
              (HsIO (HsPrimType HsPrimUnit))),
          fromFunPtrInstanceBody = Name
            "@NsVar"
            "funPtr_6f6352e1_from"},
      defineInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "funPtr_065333fd_to",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "DataValidator"))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimUnit)))),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsFun
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "DataValidator"))
                (HsFun
                  (HsPrimType HsPrimCInt)
                  (HsIO
                    (HsPrimType HsPrimUnit))))))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = ToFunPtr
        (TypePointer
          (TypeFun
            [
              TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource),
              TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "DataValidator",
                    nameHsIdent = Identifier
                      "DataValidator"}
                  (TypePointer
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
            TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "funPtr_065333fd_from",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsFun
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "DataValidator"))
                (HsFun
                  (HsPrimType HsPrimCInt)
                  (HsIO
                    (HsPrimType HsPrimUnit))))),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsFun
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Measurement")))
          (HsFun
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "DataValidator"))
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "dynamic",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = FromFunPtr
        (TypePointer
          (TypeFun
            [
              TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource),
              TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "DataValidator",
                    nameHsIdent = Identifier
                      "DataValidator"}
                  (TypePointer
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
            TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceToFunPtr
        ToFunPtrInstance {
          toFunPtrInstanceType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "DataValidator"))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimUnit)))),
          toFunPtrInstanceBody = Name
            "@NsVar"
            "funPtr_065333fd_to"},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceFromFunPtr
        FromFunPtrInstance {
          fromFunPtrInstanceType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "DataValidator"))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimUnit)))),
          fromFunPtrInstanceBody = Name
            "@NsVar"
            "funPtr_065333fd_from"},
      defineInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "funPtr_ac6a854e_to",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "FileOpenedNotification"))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimUnit)))),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsFun
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "FileOpenedNotification"))
                (HsFun
                  (HsPrimType HsPrimCInt)
                  (HsIO
                    (HsPrimType HsPrimUnit))))))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = ToFunPtr
        (TypePointer
          (TypeFun
            [
              TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource),
              TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name
                      "FileOpenedNotification",
                    nameHsIdent = Identifier
                      "FileOpenedNotification"}
                  (TypePointer
                    (TypeFun [] TypeVoid))),
              TypePrim
                (PrimIntegral PrimInt Signed)]
            TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "funPtr_ac6a854e_from",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsFun
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "FileOpenedNotification"))
                (HsFun
                  (HsPrimType HsPrimCInt)
                  (HsIO
                    (HsPrimType HsPrimUnit))))),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsFun
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Measurement")))
          (HsFun
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "FileOpenedNotification"))
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "dynamic",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = FromFunPtr
        (TypePointer
          (TypeFun
            [
              TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource),
              TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name
                      "FileOpenedNotification",
                    nameHsIdent = Identifier
                      "FileOpenedNotification"}
                  (TypePointer
                    (TypeFun [] TypeVoid))),
              TypePrim
                (PrimIntegral PrimInt Signed)]
            TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceToFunPtr
        ToFunPtrInstance {
          toFunPtrInstanceType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "FileOpenedNotification"))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimUnit)))),
          toFunPtrInstanceBody = Name
            "@NsVar"
            "funPtr_ac6a854e_to"},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceFromFunPtr
        FromFunPtrInstance {
          fromFunPtrInstanceType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "FileOpenedNotification"))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimUnit)))),
          fromFunPtrInstanceBody = Name
            "@NsVar"
            "funPtr_ac6a854e_from"},
      defineInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "funPtr_5e6e3f6b_to",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "ProgressUpdate"))
              (HsIO (HsPrimType HsPrimUnit))),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsFun
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "ProgressUpdate"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = ToFunPtr
        (TypePointer
          (TypeFun
            [
              TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource),
              TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "ProgressUpdate",
                    nameHsIdent = Identifier
                      "ProgressUpdate"}
                  (TypePointer
                    (TypeFun
                      [
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      TypeVoid)))]
            TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "funPtr_5e6e3f6b_from",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsFun
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "ProgressUpdate"))
                (HsIO
                  (HsPrimType HsPrimUnit)))),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsFun
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Measurement")))
          (HsFun
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "ProgressUpdate"))
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "dynamic",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = FromFunPtr
        (TypePointer
          (TypeFun
            [
              TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource),
              TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "ProgressUpdate",
                    nameHsIdent = Identifier
                      "ProgressUpdate"}
                  (TypePointer
                    (TypeFun
                      [
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      TypeVoid)))]
            TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceToFunPtr
        ToFunPtrInstance {
          toFunPtrInstanceType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "ProgressUpdate"))
              (HsIO (HsPrimType HsPrimUnit))),
          toFunPtrInstanceBody = Name
            "@NsVar"
            "funPtr_5e6e3f6b_to"},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceFromFunPtr
        FromFunPtrInstance {
          fromFunPtrInstanceType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "ProgressUpdate"))
              (HsIO (HsPrimType HsPrimUnit))),
          fromFunPtrInstanceBody = Name
            "@NsVar"
            "funPtr_5e6e3f6b_from"},
      defineInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "funPtr_8b272628_to",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsFun
              (HsFunPtr
                (HsFun
                  (HsPrimType HsPrimCDouble)
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsIO
                      (HsPrimType HsPrimCDouble)))))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimUnit)))),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsFun
                (HsFunPtr
                  (HsFun
                    (HsPrimType HsPrimCDouble)
                    (HsFun
                      (HsPrimType HsPrimCInt)
                      (HsIO
                        (HsPrimType HsPrimCDouble)))))
                (HsFun
                  (HsPrimType HsPrimCInt)
                  (HsIO
                    (HsPrimType HsPrimUnit))))))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = ToFunPtr
        (TypePointer
          (TypeFun
            [
              TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource),
              TypePointer
                (TypeFun
                  [
                    TypePrim
                      (PrimFloating PrimDouble),
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  (TypePrim
                    (PrimFloating PrimDouble))),
              TypePrim
                (PrimIntegral PrimInt Signed)]
            TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "funPtr_8b272628_from",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsFun
                (HsFunPtr
                  (HsFun
                    (HsPrimType HsPrimCDouble)
                    (HsFun
                      (HsPrimType HsPrimCInt)
                      (HsIO
                        (HsPrimType HsPrimCDouble)))))
                (HsFun
                  (HsPrimType HsPrimCInt)
                  (HsIO
                    (HsPrimType HsPrimUnit))))),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsFun
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Measurement")))
          (HsFun
            (HsFunPtr
              (HsFun
                (HsPrimType HsPrimCDouble)
                (HsFun
                  (HsPrimType HsPrimCInt)
                  (HsIO
                    (HsPrimType HsPrimCDouble)))))
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "dynamic",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = FromFunPtr
        (TypePointer
          (TypeFun
            [
              TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource),
              TypePointer
                (TypeFun
                  [
                    TypePrim
                      (PrimFloating PrimDouble),
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  (TypePrim
                    (PrimFloating PrimDouble))),
              TypePrim
                (PrimIntegral PrimInt Signed)]
            TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceToFunPtr
        ToFunPtrInstance {
          toFunPtrInstanceType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsFun
              (HsFunPtr
                (HsFun
                  (HsPrimType HsPrimCDouble)
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsIO
                      (HsPrimType HsPrimCDouble)))))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimUnit)))),
          toFunPtrInstanceBody = Name
            "@NsVar"
            "funPtr_8b272628_to"},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceFromFunPtr
        FromFunPtrInstance {
          fromFunPtrInstanceType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsFun
              (HsFunPtr
                (HsFun
                  (HsPrimType HsPrimCDouble)
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsIO
                      (HsPrimType HsPrimCDouble)))))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimUnit)))),
          fromFunPtrInstanceBody = Name
            "@NsVar"
            "funPtr_8b272628_from"},
      defineInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "funPtr_30a248a4_to",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsFun
              (HsFunPtr
                (HsFun
                  (HsPtr
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "Measurement")))
                  (HsFun
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "DataValidator"))
                    (HsFun
                      (HsPrimType HsPrimCInt)
                      (HsIO
                        (HsPrimType HsPrimUnit))))))
              (HsFun
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "DataValidator"))
                (HsIO
                  (HsPrimType HsPrimUnit)))),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsFun
                (HsFunPtr
                  (HsFun
                    (HsPtr
                      (HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Measurement")))
                    (HsFun
                      (HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "DataValidator"))
                      (HsFun
                        (HsPrimType HsPrimCInt)
                        (HsIO
                          (HsPrimType HsPrimUnit))))))
                (HsFun
                  (HsTypRef
                    (Name
                      "@NsTypeConstr"
                      "DataValidator"))
                  (HsIO
                    (HsPrimType HsPrimUnit))))))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = ToFunPtr
        (TypePointer
          (TypeFun
            [
              TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource),
              TypePointer
                (TypeFun
                  [
                    TypePointer
                      (TypeStruct
                        NamePair {
                          nameC = Name "Measurement",
                          nameHsIdent = Identifier
                            "Measurement"}
                        NameOriginInSource),
                    TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "DataValidator",
                          nameHsIdent = Identifier
                            "DataValidator"}
                        (TypePointer
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
                  TypeVoid),
              TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "DataValidator",
                    nameHsIdent = Identifier
                      "DataValidator"}
                  (TypePointer
                    (TypeFun
                      [
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed)))))]
            TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "funPtr_30a248a4_from",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsFun
                (HsFunPtr
                  (HsFun
                    (HsPtr
                      (HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Measurement")))
                    (HsFun
                      (HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "DataValidator"))
                      (HsFun
                        (HsPrimType HsPrimCInt)
                        (HsIO
                          (HsPrimType HsPrimUnit))))))
                (HsFun
                  (HsTypRef
                    (Name
                      "@NsTypeConstr"
                      "DataValidator"))
                  (HsIO
                    (HsPrimType HsPrimUnit))))),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsFun
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Measurement")))
          (HsFun
            (HsFunPtr
              (HsFun
                (HsPtr
                  (HsTypRef
                    (Name
                      "@NsTypeConstr"
                      "Measurement")))
                (HsFun
                  (HsTypRef
                    (Name
                      "@NsTypeConstr"
                      "DataValidator"))
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsIO
                      (HsPrimType HsPrimUnit))))))
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "DataValidator"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "dynamic",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = FromFunPtr
        (TypePointer
          (TypeFun
            [
              TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource),
              TypePointer
                (TypeFun
                  [
                    TypePointer
                      (TypeStruct
                        NamePair {
                          nameC = Name "Measurement",
                          nameHsIdent = Identifier
                            "Measurement"}
                        NameOriginInSource),
                    TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "DataValidator",
                          nameHsIdent = Identifier
                            "DataValidator"}
                        (TypePointer
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
                  TypeVoid),
              TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "DataValidator",
                    nameHsIdent = Identifier
                      "DataValidator"}
                  (TypePointer
                    (TypeFun
                      [
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed)))))]
            TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceToFunPtr
        ToFunPtrInstance {
          toFunPtrInstanceType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsFun
              (HsFunPtr
                (HsFun
                  (HsPtr
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "Measurement")))
                  (HsFun
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "DataValidator"))
                    (HsFun
                      (HsPrimType HsPrimCInt)
                      (HsIO
                        (HsPrimType HsPrimUnit))))))
              (HsFun
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "DataValidator"))
                (HsIO
                  (HsPrimType HsPrimUnit)))),
          toFunPtrInstanceBody = Name
            "@NsVar"
            "funPtr_30a248a4_to"},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceFromFunPtr
        FromFunPtrInstance {
          fromFunPtrInstanceType = HsFun
            (HsPtr
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Measurement")))
            (HsFun
              (HsFunPtr
                (HsFun
                  (HsPtr
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "Measurement")))
                  (HsFun
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "DataValidator"))
                    (HsFun
                      (HsPrimType HsPrimCInt)
                      (HsIO
                        (HsPrimType HsPrimUnit))))))
              (HsFun
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "DataValidator"))
                (HsIO
                  (HsPrimType HsPrimUnit)))),
          fromFunPtrInstanceBody = Name
            "@NsVar"
            "funPtr_30a248a4_from"},
      defineInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "funPtr_a3803a24_to",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFun
            (HsTypRef
              (Name "@NsTypeConstr" "Foo2"))
            (HsIO (HsPrimType HsPrimUnit)),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "Foo2"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = ToFunPtr
        (TypePointer
          (TypeFun
            [
              TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "foo2",
                      nameHsIdent = Identifier "Foo2"}
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))]
            TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "funPtr_a3803a24_from",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "Foo2"))
              (HsIO (HsPrimType HsPrimUnit))),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsFun
          (HsTypRef
            (Name "@NsTypeConstr" "Foo2"))
          (HsIO (HsPrimType HsPrimUnit))),
      foreignImportOrigName =
      "dynamic",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin = FromFunPtr
        (TypePointer
          (TypeFun
            [
              TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "foo2",
                      nameHsIdent = Identifier "Foo2"}
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))]
            TypeVoid)),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceToFunPtr
        ToFunPtrInstance {
          toFunPtrInstanceType = HsFun
            (HsTypRef
              (Name "@NsTypeConstr" "Foo2"))
            (HsIO (HsPrimType HsPrimUnit)),
          toFunPtrInstanceBody = Name
            "@NsVar"
            "funPtr_a3803a24_to"},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceFromFunPtr
        FromFunPtrInstance {
          fromFunPtrInstanceType = HsFun
            (HsTypRef
              (Name "@NsTypeConstr" "Foo2"))
            (HsIO (HsPrimType HsPrimUnit)),
          fromFunPtrInstanceBody = Name
            "@NsVar"
            "funPtr_a3803a24_from"},
      defineInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "readFileWithProcessor",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "processLine"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimUnit))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "processLine",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "fileId"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "fileId",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_a0a59181c714c131",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_callbacks_a0a59181c714c131 (void (*arg1) (signed int arg1), signed int arg2) { return readFileWithProcessor(arg1, arg2); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "processLine",
                  nameHsIdent = Identifier
                    "processLine"})
              (TypePointer
                (TypeFun
                  [
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  TypeVoid)),
            _×_
              (Just
                NamePair {
                  nameC = Name "fileId",
                  nameHsIdent = Identifier
                    "fileId"})
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
            "readFileWithProcessor",
          commentLocation = Just
            "callbacks.h:4:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "watchTemperature",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "onTempChange"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimUnit))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "onTempChange",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "sensorId"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "sensorId",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_d59e6698796971ea",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_d59e6698796971ea (void (*arg1) (signed int arg1), signed int arg2) { watchTemperature(arg1, arg2); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "onTempChange",
                  nameHsIdent = Identifier
                    "onTempChange"})
              (TypePointer
                (TypeFun
                  [
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  TypeVoid)),
            _×_
              (Just
                NamePair {
                  nameC = Name "sensorId",
                  nameHsIdent = Identifier
                    "sensorId"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "watchTemperature",
          commentLocation = Just
            "callbacks.h:5:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "onFileOpened",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "notify"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "FileOpenedNotification"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "notify",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_c9fb8fdc3d0d3978",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_c9fb8fdc3d0d3978 (FileOpenedNotification arg1) { onFileOpened(arg1); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "notify",
                  nameHsIdent = Identifier
                    "notify"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name
                      "FileOpenedNotification",
                    nameHsIdent = Identifier
                      "FileOpenedNotification"}
                  (TypePointer
                    (TypeFun [] TypeVoid))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "onFileOpened",
          commentLocation = Just
            "callbacks.h:14:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "onProgressChanged",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "update"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "ProgressUpdate"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "update",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_7921ad1b219190e4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_7921ad1b219190e4 (ProgressUpdate arg1) { onProgressChanged(arg1); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "update",
                  nameHsIdent = Identifier
                    "update"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "ProgressUpdate",
                    nameHsIdent = Identifier
                      "ProgressUpdate"}
                  (TypePointer
                    (TypeFun
                      [
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      TypeVoid))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "onProgressChanged",
          commentLocation = Just
            "callbacks.h:15:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "validateInput",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "validator"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "DataValidator"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "validator",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "rawValue"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "rawValue",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_ae19d658f098584a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_callbacks_ae19d658f098584a (DataValidator arg1, signed int arg2) { return validateInput(arg1, arg2); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "validator",
                  nameHsIdent = Identifier
                    "validator"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "DataValidator",
                    nameHsIdent = Identifier
                      "DataValidator"}
                  (TypePointer
                    (TypeFun
                      [
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed)))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "rawValue",
                  nameHsIdent = Identifier
                    "rawValue"})
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
            "validateInput",
          commentLocation = Just
            "callbacks.h:16:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "onNewMeasurement",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "handler"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "MeasurementReceived"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "handler",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_d2fdffe85523b3ef",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_d2fdffe85523b3ef (MeasurementReceived arg1) { onNewMeasurement(arg1); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "handler",
                  nameHsIdent = Identifier
                    "handler"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name
                      "MeasurementReceived",
                    nameHsIdent = Identifier
                      "MeasurementReceived"}
                  (TypePointer
                    (TypeFun
                      [
                        TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = Name "Measurement",
                              nameHsIdent = Identifier
                                "Measurement"}
                            NameOriginInSource)]
                      TypeVoid))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "onNewMeasurement",
          commentLocation = Just
            "callbacks.h:27:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "onNewMeasurement2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "handler"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "MeasurementReceived2"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "handler",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_c5b555bbc07b808d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_c5b555bbc07b808d (MeasurementReceived2 arg1) { onNewMeasurement2(arg1); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "handler",
                  nameHsIdent = Identifier
                    "handler"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name
                      "MeasurementReceived2",
                    nameHsIdent = Identifier
                      "MeasurementReceived2"}
                  (TypePointer
                    (TypeFun
                      [
                        TypeStruct
                          NamePair {
                            nameC = Name "Measurement",
                            nameHsIdent = Identifier
                              "Measurement"}
                          NameOriginInSource]
                      TypeVoid))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "onNewMeasurement2",
          commentLocation = Just
            "callbacks.h:30:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "onBufferReady",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "handler"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "SampleBufferFull"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "handler",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_65927c77229ad893",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_65927c77229ad893 (SampleBufferFull arg1) { onBufferReady(arg1); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "handler",
                  nameHsIdent = Identifier
                    "handler"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "SampleBufferFull",
                    nameHsIdent = Identifier
                      "SampleBufferFull"}
                  (TypePointer
                    (TypeFun
                      [
                        TypeConstArray
                          10
                          (TypePrim
                            (PrimIntegral PrimInt Signed))]
                      TypeVoid))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "onBufferReady",
          commentLocation = Just
            "callbacks.h:33:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "transformMeasurement",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "data'"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Measurement")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "data'",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "transformer"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsFun
                (HsFunPtr
                  (HsFun
                    (HsPrimType HsPrimCDouble)
                    (HsFun
                      (HsPrimType HsPrimCInt)
                      (HsIO
                        (HsPrimType HsPrimCDouble)))))
                (HsFun
                  (HsPrimType HsPrimCInt)
                  (HsIO
                    (HsPrimType HsPrimUnit))))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "transformer",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_0b6a9249f49b986f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_0b6a9249f49b986f (struct Measurement *arg1, void (*arg2) (struct Measurement *arg1, double (*arg2) (double arg1, signed int arg2), signed int arg3)) { transformMeasurement(arg1, arg2); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "data",
                  nameHsIdent = Identifier
                    "data'"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "transformer",
                  nameHsIdent = Identifier
                    "transformer"})
              (TypePointer
                (TypeFun
                  [
                    TypePointer
                      (TypeStruct
                        NamePair {
                          nameC = Name "Measurement",
                          nameHsIdent = Identifier
                            "Measurement"}
                        NameOriginInSource),
                    TypePointer
                      (TypeFun
                        [
                          TypePrim
                            (PrimFloating PrimDouble),
                          TypePrim
                            (PrimIntegral PrimInt Signed)]
                        (TypePrim
                          (PrimFloating PrimDouble))),
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  TypeVoid))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "transformMeasurement",
          commentLocation = Just
            "callbacks.h:38:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "processWithCallbacks",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "handler"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsFun
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "FileOpenedNotification"))
                (HsFun
                  (HsPrimType HsPrimCInt)
                  (HsIO
                    (HsPrimType HsPrimUnit))))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "handler",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_2c3e0e84ae9cde51",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_2c3e0e84ae9cde51 (void (*arg1) (struct Measurement *arg1, FileOpenedNotification arg2, signed int arg3)) { processWithCallbacks(arg1); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "handler",
                  nameHsIdent = Identifier
                    "handler"})
              (TypePointer
                (TypeFun
                  [
                    TypePointer
                      (TypeStruct
                        NamePair {
                          nameC = Name "Measurement",
                          nameHsIdent = Identifier
                            "Measurement"}
                        NameOriginInSource),
                    TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name
                            "FileOpenedNotification",
                          nameHsIdent = Identifier
                            "FileOpenedNotification"}
                        (TypePointer
                          (TypeFun [] TypeVoid))),
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  TypeVoid))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "processWithCallbacks",
          commentLocation = Just
            "callbacks.h:43:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "registerHandler",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "handler"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "MeasurementHandler")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "handler",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_0b172585709f9d48",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_0b172585709f9d48 (struct MeasurementHandler *arg1) { registerHandler(arg1); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "handler",
                  nameHsIdent = Identifier
                    "handler"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name
                      "MeasurementHandler",
                    nameHsIdent = Identifier
                      "MeasurementHandler"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "registerHandler",
          commentLocation = Just
            "callbacks.h:56:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "executePipeline",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "data'"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Measurement")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "data'",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "pipeline"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "DataPipeline")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "pipeline",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_25a56dfc7b259e7d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_25a56dfc7b259e7d (struct Measurement *arg1, struct DataPipeline *arg2) { executePipeline(arg1, arg2); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "data",
                  nameHsIdent = Identifier
                    "data'"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "pipeline",
                  nameHsIdent = Identifier
                    "pipeline"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "DataPipeline",
                    nameHsIdent = Identifier
                      "DataPipeline"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "executePipeline",
          commentLocation = Just
            "callbacks.h:64:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "runProcessor",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "data'"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Measurement")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "data'",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "processor"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Processor")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "processor",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_5908d37641d70953",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_5908d37641d70953 (struct Measurement *arg1, struct Processor *arg2) { runProcessor(arg1, arg2); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "data",
                  nameHsIdent = Identifier
                    "data'"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "processor",
                  nameHsIdent = Identifier
                    "processor"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Processor",
                    nameHsIdent = Identifier
                      "Processor"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "runProcessor",
          commentLocation = Just
            "callbacks.h:80:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "processMeasurementWithValidation",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "data'"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Measurement")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "data'",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "processor"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsFun
                (HsFunPtr
                  (HsFun
                    (HsPtr
                      (HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Measurement")))
                    (HsFun
                      (HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "DataValidator"))
                      (HsFun
                        (HsPrimType HsPrimCInt)
                        (HsIO
                          (HsPrimType HsPrimUnit))))))
                (HsFun
                  (HsTypRef
                    (Name
                      "@NsTypeConstr"
                      "DataValidator"))
                  (HsIO
                    (HsPrimType HsPrimUnit))))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "processor",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_f3c99b4af7808e7f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_f3c99b4af7808e7f (struct Measurement *arg1, void (*arg2) (struct Measurement *arg1, void (*arg2) (struct Measurement *arg1, DataValidator arg2, signed int arg3), DataValidator arg3)) { processMeasurementWithValidation(arg1, arg2); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "data",
                  nameHsIdent = Identifier
                    "data'"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "processor",
                  nameHsIdent = Identifier
                    "processor"})
              (TypePointer
                (TypeFun
                  [
                    TypePointer
                      (TypeStruct
                        NamePair {
                          nameC = Name "Measurement",
                          nameHsIdent = Identifier
                            "Measurement"}
                        NameOriginInSource),
                    TypePointer
                      (TypeFun
                        [
                          TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "Measurement",
                                nameHsIdent = Identifier
                                  "Measurement"}
                              NameOriginInSource),
                          TypeTypedef
                            (TypedefRegular
                              NamePair {
                                nameC = Name "DataValidator",
                                nameHsIdent = Identifier
                                  "DataValidator"}
                              (TypePointer
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
                        TypeVoid),
                    TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "DataValidator",
                          nameHsIdent = Identifier
                            "DataValidator"}
                        (TypePointer
                          (TypeFun
                            [
                              TypePrim
                                (PrimIntegral PrimInt Signed)]
                            (TypePrim
                              (PrimIntegral
                                PrimInt
                                Signed)))))]
                  TypeVoid))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "processMeasurementWithValidation",
          commentLocation = Just
            "callbacks.h:85:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "f",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "callback"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "Foo"))
              (HsIO (HsPrimType HsPrimUnit))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "callback",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_fcce70013c76ce8b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_fcce70013c76ce8b (void (*arg1) (foo arg1)) { f(arg1); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "callback",
                  nameHsIdent = Identifier
                    "callback"})
              (TypePointer
                (TypeFun
                  [
                    TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "foo",
                          nameHsIdent = Identifier "Foo"}
                        (TypePrim
                          (PrimIntegral PrimInt Signed)))]
                  TypeVoid))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "f",
          commentLocation = Just
            "callbacks.h:96:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "f2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "handler"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "Foo2"))
              (HsIO (HsPrimType HsPrimUnit))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "handler",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_1d043de05a457e90",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_1d043de05a457e90 (void (*arg1) (foo2 const arg1)) { f2(arg1); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "handler",
                  nameHsIdent = Identifier
                    "handler"})
              (TypePointer
                (TypeFun
                  [
                    TypeQualified
                      TypeQualifierConst
                      (TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "foo2",
                            nameHsIdent = Identifier "Foo2"}
                          (TypePrim
                            (PrimIntegral
                              PrimInt
                              Signed))))]
                  TypeVoid))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "f2",
          commentLocation = Just
            "callbacks.h:97:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "readFileWithProcessor",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "processLine"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimUnit))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "processLine",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "fileId"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "fileId",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_f0d72410d79899b5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_callbacks_f0d72410d79899b5 (void (*arg1) (signed int arg1), signed int arg2) { return readFileWithProcessor(arg1, arg2); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "processLine",
                  nameHsIdent = Identifier
                    "processLine"})
              (TypePointer
                (TypeFun
                  [
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  TypeVoid)),
            _×_
              (Just
                NamePair {
                  nameC = Name "fileId",
                  nameHsIdent = Identifier
                    "fileId"})
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
            "readFileWithProcessor",
          commentLocation = Just
            "callbacks.h:4:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "watchTemperature",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "onTempChange"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimUnit))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "onTempChange",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "sensorId"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "sensorId",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_a445b9cacb08ed71",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_a445b9cacb08ed71 (void (*arg1) (signed int arg1), signed int arg2) { watchTemperature(arg1, arg2); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "onTempChange",
                  nameHsIdent = Identifier
                    "onTempChange"})
              (TypePointer
                (TypeFun
                  [
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  TypeVoid)),
            _×_
              (Just
                NamePair {
                  nameC = Name "sensorId",
                  nameHsIdent = Identifier
                    "sensorId"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "watchTemperature",
          commentLocation = Just
            "callbacks.h:5:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "onFileOpened",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "notify"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "FileOpenedNotification"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "notify",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_b71e59965bcc2316",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_b71e59965bcc2316 (FileOpenedNotification arg1) { onFileOpened(arg1); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "notify",
                  nameHsIdent = Identifier
                    "notify"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name
                      "FileOpenedNotification",
                    nameHsIdent = Identifier
                      "FileOpenedNotification"}
                  (TypePointer
                    (TypeFun [] TypeVoid))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "onFileOpened",
          commentLocation = Just
            "callbacks.h:14:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "onProgressChanged",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "update"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "ProgressUpdate"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "update",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_013e79fc3cd3b1b4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_013e79fc3cd3b1b4 (ProgressUpdate arg1) { onProgressChanged(arg1); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "update",
                  nameHsIdent = Identifier
                    "update"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "ProgressUpdate",
                    nameHsIdent = Identifier
                      "ProgressUpdate"}
                  (TypePointer
                    (TypeFun
                      [
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      TypeVoid))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "onProgressChanged",
          commentLocation = Just
            "callbacks.h:15:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "validateInput",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "validator"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "DataValidator"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "validator",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "rawValue"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "rawValue",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_697a7b01b3d64c58",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_callbacks_697a7b01b3d64c58 (DataValidator arg1, signed int arg2) { return validateInput(arg1, arg2); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "validator",
                  nameHsIdent = Identifier
                    "validator"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "DataValidator",
                    nameHsIdent = Identifier
                      "DataValidator"}
                  (TypePointer
                    (TypeFun
                      [
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed)))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "rawValue",
                  nameHsIdent = Identifier
                    "rawValue"})
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
            "validateInput",
          commentLocation = Just
            "callbacks.h:16:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "onNewMeasurement",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "handler"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "MeasurementReceived"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "handler",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_f291b861b36d5a90",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_f291b861b36d5a90 (MeasurementReceived arg1) { onNewMeasurement(arg1); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "handler",
                  nameHsIdent = Identifier
                    "handler"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name
                      "MeasurementReceived",
                    nameHsIdent = Identifier
                      "MeasurementReceived"}
                  (TypePointer
                    (TypeFun
                      [
                        TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = Name "Measurement",
                              nameHsIdent = Identifier
                                "Measurement"}
                            NameOriginInSource)]
                      TypeVoid))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "onNewMeasurement",
          commentLocation = Just
            "callbacks.h:27:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "onNewMeasurement2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "handler"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "MeasurementReceived2"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "handler",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_4f36523b7d965e44",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_4f36523b7d965e44 (MeasurementReceived2 arg1) { onNewMeasurement2(arg1); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "handler",
                  nameHsIdent = Identifier
                    "handler"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name
                      "MeasurementReceived2",
                    nameHsIdent = Identifier
                      "MeasurementReceived2"}
                  (TypePointer
                    (TypeFun
                      [
                        TypeStruct
                          NamePair {
                            nameC = Name "Measurement",
                            nameHsIdent = Identifier
                              "Measurement"}
                          NameOriginInSource]
                      TypeVoid))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "onNewMeasurement2",
          commentLocation = Just
            "callbacks.h:30:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "onBufferReady",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "handler"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "SampleBufferFull"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "handler",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_92d54aaf9e8a1c8e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_92d54aaf9e8a1c8e (SampleBufferFull arg1) { onBufferReady(arg1); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "handler",
                  nameHsIdent = Identifier
                    "handler"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "SampleBufferFull",
                    nameHsIdent = Identifier
                      "SampleBufferFull"}
                  (TypePointer
                    (TypeFun
                      [
                        TypeConstArray
                          10
                          (TypePrim
                            (PrimIntegral PrimInt Signed))]
                      TypeVoid))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "onBufferReady",
          commentLocation = Just
            "callbacks.h:33:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "transformMeasurement",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "data'"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Measurement")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "data'",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "transformer"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsFun
                (HsFunPtr
                  (HsFun
                    (HsPrimType HsPrimCDouble)
                    (HsFun
                      (HsPrimType HsPrimCInt)
                      (HsIO
                        (HsPrimType HsPrimCDouble)))))
                (HsFun
                  (HsPrimType HsPrimCInt)
                  (HsIO
                    (HsPrimType HsPrimUnit))))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "transformer",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_0e22183e51a42eab",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_0e22183e51a42eab (struct Measurement *arg1, void (*arg2) (struct Measurement *arg1, double (*arg2) (double arg1, signed int arg2), signed int arg3)) { transformMeasurement(arg1, arg2); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "data",
                  nameHsIdent = Identifier
                    "data'"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "transformer",
                  nameHsIdent = Identifier
                    "transformer"})
              (TypePointer
                (TypeFun
                  [
                    TypePointer
                      (TypeStruct
                        NamePair {
                          nameC = Name "Measurement",
                          nameHsIdent = Identifier
                            "Measurement"}
                        NameOriginInSource),
                    TypePointer
                      (TypeFun
                        [
                          TypePrim
                            (PrimFloating PrimDouble),
                          TypePrim
                            (PrimIntegral PrimInt Signed)]
                        (TypePrim
                          (PrimFloating PrimDouble))),
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  TypeVoid))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "transformMeasurement",
          commentLocation = Just
            "callbacks.h:38:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "processWithCallbacks",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "handler"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsFun
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "FileOpenedNotification"))
                (HsFun
                  (HsPrimType HsPrimCInt)
                  (HsIO
                    (HsPrimType HsPrimUnit))))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "handler",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_9b4727ea289ff135",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_9b4727ea289ff135 (void (*arg1) (struct Measurement *arg1, FileOpenedNotification arg2, signed int arg3)) { processWithCallbacks(arg1); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "handler",
                  nameHsIdent = Identifier
                    "handler"})
              (TypePointer
                (TypeFun
                  [
                    TypePointer
                      (TypeStruct
                        NamePair {
                          nameC = Name "Measurement",
                          nameHsIdent = Identifier
                            "Measurement"}
                        NameOriginInSource),
                    TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name
                            "FileOpenedNotification",
                          nameHsIdent = Identifier
                            "FileOpenedNotification"}
                        (TypePointer
                          (TypeFun [] TypeVoid))),
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  TypeVoid))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "processWithCallbacks",
          commentLocation = Just
            "callbacks.h:43:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "registerHandler",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "handler"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "MeasurementHandler")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "handler",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_aea76777f06b51ce",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_aea76777f06b51ce (struct MeasurementHandler *arg1) { registerHandler(arg1); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "handler",
                  nameHsIdent = Identifier
                    "handler"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name
                      "MeasurementHandler",
                    nameHsIdent = Identifier
                      "MeasurementHandler"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "registerHandler",
          commentLocation = Just
            "callbacks.h:56:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "executePipeline",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "data'"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Measurement")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "data'",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "pipeline"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "DataPipeline")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "pipeline",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_54fc81d3b44b84d5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_54fc81d3b44b84d5 (struct Measurement *arg1, struct DataPipeline *arg2) { executePipeline(arg1, arg2); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "data",
                  nameHsIdent = Identifier
                    "data'"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "pipeline",
                  nameHsIdent = Identifier
                    "pipeline"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "DataPipeline",
                    nameHsIdent = Identifier
                      "DataPipeline"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "executePipeline",
          commentLocation = Just
            "callbacks.h:64:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "runProcessor",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "data'"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Measurement")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "data'",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "processor"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Processor")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "processor",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_4bb32ee774218053",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_4bb32ee774218053 (struct Measurement *arg1, struct Processor *arg2) { runProcessor(arg1, arg2); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "data",
                  nameHsIdent = Identifier
                    "data'"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "processor",
                  nameHsIdent = Identifier
                    "processor"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Processor",
                    nameHsIdent = Identifier
                      "Processor"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "runProcessor",
          commentLocation = Just
            "callbacks.h:80:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "processMeasurementWithValidation",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "data'"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Measurement")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "data'",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "processor"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Measurement")))
              (HsFun
                (HsFunPtr
                  (HsFun
                    (HsPtr
                      (HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Measurement")))
                    (HsFun
                      (HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "DataValidator"))
                      (HsFun
                        (HsPrimType HsPrimCInt)
                        (HsIO
                          (HsPrimType HsPrimUnit))))))
                (HsFun
                  (HsTypRef
                    (Name
                      "@NsTypeConstr"
                      "DataValidator"))
                  (HsIO
                    (HsPrimType HsPrimUnit))))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "processor",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_f453b618c9ab0234",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_f453b618c9ab0234 (struct Measurement *arg1, void (*arg2) (struct Measurement *arg1, void (*arg2) (struct Measurement *arg1, DataValidator arg2, signed int arg3), DataValidator arg3)) { processMeasurementWithValidation(arg1, arg2); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "data",
                  nameHsIdent = Identifier
                    "data'"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "Measurement",
                    nameHsIdent = Identifier
                      "Measurement"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "processor",
                  nameHsIdent = Identifier
                    "processor"})
              (TypePointer
                (TypeFun
                  [
                    TypePointer
                      (TypeStruct
                        NamePair {
                          nameC = Name "Measurement",
                          nameHsIdent = Identifier
                            "Measurement"}
                        NameOriginInSource),
                    TypePointer
                      (TypeFun
                        [
                          TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "Measurement",
                                nameHsIdent = Identifier
                                  "Measurement"}
                              NameOriginInSource),
                          TypeTypedef
                            (TypedefRegular
                              NamePair {
                                nameC = Name "DataValidator",
                                nameHsIdent = Identifier
                                  "DataValidator"}
                              (TypePointer
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
                        TypeVoid),
                    TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "DataValidator",
                          nameHsIdent = Identifier
                            "DataValidator"}
                        (TypePointer
                          (TypeFun
                            [
                              TypePrim
                                (PrimIntegral PrimInt Signed)]
                            (TypePrim
                              (PrimIntegral
                                PrimInt
                                Signed)))))]
                  TypeVoid))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "processMeasurementWithValidation",
          commentLocation = Just
            "callbacks.h:85:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "f",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "callback"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "Foo"))
              (HsIO (HsPrimType HsPrimUnit))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "callback",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_b68b2cffacc97c1d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_b68b2cffacc97c1d (void (*arg1) (foo arg1)) { f(arg1); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "callback",
                  nameHsIdent = Identifier
                    "callback"})
              (TypePointer
                (TypeFun
                  [
                    TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "foo",
                          nameHsIdent = Identifier "Foo"}
                        (TypePrim
                          (PrimIntegral PrimInt Signed)))]
                  TypeVoid))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "f",
          commentLocation = Just
            "callbacks.h:96:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "f2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "handler"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "Foo2"))
              (HsIO (HsPrimType HsPrimUnit))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "handler",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_14361e995fb5684a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_14361e995fb5684a (void (*arg1) (foo2 const arg1)) { f2(arg1); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "handler",
                  nameHsIdent = Identifier
                    "handler"})
              (TypePointer
                (TypeFun
                  [
                    TypeQualified
                      TypeQualifierConst
                      (TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "foo2",
                            nameHsIdent = Identifier "Foo2"}
                          (TypePrim
                            (PrimIntegral
                              PrimInt
                              Signed))))]
                  TypeVoid))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "f2",
          commentLocation = Just
            "callbacks.h:97:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_callbacks_c4b06d89a94616dd",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsFunPtr
                (HsFun
                  (HsPrimType HsPrimCInt)
                  (HsIO (HsPrimType HsPrimUnit))))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_c4b06d89a94616dd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_readFileWithProcessor_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_callbacks_c4b06d89a94616dd (void)) (void (*arg1) (signed int arg1), signed int arg2) { return &readFileWithProcessor; } ",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed)]
                TypeVoid),
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
        "hs_bindgen_test_callbacks_22c54726df44b640",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsFunPtr
                (HsFun
                  (HsPrimType HsPrimCInt)
                  (HsIO (HsPrimType HsPrimUnit))))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_22c54726df44b640",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_watchTemperature_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_22c54726df44b640 (void)) (void (*arg1) (signed int arg1), signed int arg2) { return &watchTemperature; } ",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed)]
                TypeVoid),
            TypePrim
              (PrimIntegral PrimInt Signed)]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_callbacks_8167a5b82d621c9d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "FileOpenedNotification"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_8167a5b82d621c9d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_onFileOpened_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_8167a5b82d621c9d (void)) (FileOpenedNotification arg1) { return &onFileOpened; } ",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name
                    "FileOpenedNotification",
                  nameHsIdent = Identifier
                    "FileOpenedNotification"}
                (TypePointer
                  (TypeFun [] TypeVoid)))]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_callbacks_ef51ad75ce9862a3",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "ProgressUpdate"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_ef51ad75ce9862a3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_onProgressChanged_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_ef51ad75ce9862a3 (void)) (ProgressUpdate arg1) { return &onProgressChanged; } ",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "ProgressUpdate",
                  nameHsIdent = Identifier
                    "ProgressUpdate"}
                (TypePointer
                  (TypeFun
                    [
                      TypePrim
                        (PrimIntegral PrimInt Signed)]
                    TypeVoid)))]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_callbacks_9eaedb1b1c5b3fdb",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "DataValidator"))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_9eaedb1b1c5b3fdb",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_validateInput_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_callbacks_9eaedb1b1c5b3fdb (void)) (DataValidator arg1, signed int arg2) { return &validateInput; } ",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "DataValidator",
                  nameHsIdent = Identifier
                    "DataValidator"}
                (TypePointer
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
        "hs_bindgen_test_callbacks_f9f4f5ec3dd82431",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "MeasurementReceived"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_f9f4f5ec3dd82431",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_onNewMeasurement_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_f9f4f5ec3dd82431 (void)) (MeasurementReceived arg1) { return &onNewMeasurement; } ",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name
                    "MeasurementReceived",
                  nameHsIdent = Identifier
                    "MeasurementReceived"}
                (TypePointer
                  (TypeFun
                    [
                      TypePointer
                        (TypeStruct
                          NamePair {
                            nameC = Name "Measurement",
                            nameHsIdent = Identifier
                              "Measurement"}
                          NameOriginInSource)]
                    TypeVoid)))]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_callbacks_9c5afeda25ede1ce",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "MeasurementReceived2"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_9c5afeda25ede1ce",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_onNewMeasurement2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_9c5afeda25ede1ce (void)) (MeasurementReceived2 arg1) { return &onNewMeasurement2; } ",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name
                    "MeasurementReceived2",
                  nameHsIdent = Identifier
                    "MeasurementReceived2"}
                (TypePointer
                  (TypeFun
                    [
                      TypeStruct
                        NamePair {
                          nameC = Name "Measurement",
                          nameHsIdent = Identifier
                            "Measurement"}
                        NameOriginInSource]
                    TypeVoid)))]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_callbacks_8091188123328aa8",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "SampleBufferFull"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_8091188123328aa8",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_onBufferReady_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_8091188123328aa8 (void)) (SampleBufferFull arg1) { return &onBufferReady; } ",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "SampleBufferFull",
                  nameHsIdent = Identifier
                    "SampleBufferFull"}
                (TypePointer
                  (TypeFun
                    [
                      TypeConstArray
                        10
                        (TypePrim
                          (PrimIntegral PrimInt Signed))]
                    TypeVoid)))]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_callbacks_6c9fe4dae03a37fa",
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
                    "Measurement")))
              (HsFun
                (HsFunPtr
                  (HsFun
                    (HsPtr
                      (HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Measurement")))
                    (HsFun
                      (HsFunPtr
                        (HsFun
                          (HsPrimType HsPrimCDouble)
                          (HsFun
                            (HsPrimType HsPrimCInt)
                            (HsIO
                              (HsPrimType HsPrimCDouble)))))
                      (HsFun
                        (HsPrimType HsPrimCInt)
                        (HsIO
                          (HsPrimType HsPrimUnit))))))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_6c9fe4dae03a37fa",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_transformMeasurement_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_6c9fe4dae03a37fa (void)) (struct Measurement *arg1, void (*arg2) (struct Measurement *arg1, double (*arg2) (double arg1, signed int arg2), signed int arg3)) { return &transformMeasurement; } ",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name "Measurement",
                  nameHsIdent = Identifier
                    "Measurement"}
                NameOriginInSource),
            TypePointer
              (TypeFun
                [
                  TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "Measurement",
                        nameHsIdent = Identifier
                          "Measurement"}
                      NameOriginInSource),
                  TypePointer
                    (TypeFun
                      [
                        TypePrim
                          (PrimFloating PrimDouble),
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      (TypePrim
                        (PrimFloating PrimDouble))),
                  TypePrim
                    (PrimIntegral PrimInt Signed)]
                TypeVoid)]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_callbacks_2ee8d8889cd31fb7",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsFunPtr
                (HsFun
                  (HsPtr
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "Measurement")))
                  (HsFun
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "FileOpenedNotification"))
                    (HsFun
                      (HsPrimType HsPrimCInt)
                      (HsIO
                        (HsPrimType HsPrimUnit))))))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_2ee8d8889cd31fb7",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_processWithCallbacks_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_2ee8d8889cd31fb7 (void)) (void (*arg1) (struct Measurement *arg1, FileOpenedNotification arg2, signed int arg3)) { return &processWithCallbacks; } ",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeFun
                [
                  TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "Measurement",
                        nameHsIdent = Identifier
                          "Measurement"}
                      NameOriginInSource),
                  TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name
                          "FileOpenedNotification",
                        nameHsIdent = Identifier
                          "FileOpenedNotification"}
                      (TypePointer
                        (TypeFun [] TypeVoid))),
                  TypePrim
                    (PrimIntegral PrimInt Signed)]
                TypeVoid)]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_callbacks_5a70e34ecc71835b",
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
                    "MeasurementHandler")))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_5a70e34ecc71835b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_registerHandler_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_5a70e34ecc71835b (void)) (struct MeasurementHandler *arg1) { return &registerHandler; } ",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name
                    "MeasurementHandler",
                  nameHsIdent = Identifier
                    "MeasurementHandler"}
                NameOriginInSource)]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_callbacks_1a0881ba01e93710",
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
                    "Measurement")))
              (HsFun
                (HsPtr
                  (HsTypRef
                    (Name
                      "@NsTypeConstr"
                      "DataPipeline")))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_1a0881ba01e93710",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_executePipeline_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_1a0881ba01e93710 (void)) (struct Measurement *arg1, struct DataPipeline *arg2) { return &executePipeline; } ",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name "Measurement",
                  nameHsIdent = Identifier
                    "Measurement"}
                NameOriginInSource),
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name "DataPipeline",
                  nameHsIdent = Identifier
                    "DataPipeline"}
                NameOriginInSource)]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_callbacks_1e7d8fd6cb5a199f",
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
                    "Measurement")))
              (HsFun
                (HsPtr
                  (HsTypRef
                    (Name
                      "@NsTypeConstr"
                      "Processor")))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_1e7d8fd6cb5a199f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_runProcessor_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_1e7d8fd6cb5a199f (void)) (struct Measurement *arg1, struct Processor *arg2) { return &runProcessor; } ",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name "Measurement",
                  nameHsIdent = Identifier
                    "Measurement"}
                NameOriginInSource),
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name "Processor",
                  nameHsIdent = Identifier
                    "Processor"}
                NameOriginInSource)]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_callbacks_6621b1bf8ef7af3b",
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
                    "Measurement")))
              (HsFun
                (HsFunPtr
                  (HsFun
                    (HsPtr
                      (HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Measurement")))
                    (HsFun
                      (HsFunPtr
                        (HsFun
                          (HsPtr
                            (HsTypRef
                              (Name
                                "@NsTypeConstr"
                                "Measurement")))
                          (HsFun
                            (HsTypRef
                              (Name
                                "@NsTypeConstr"
                                "DataValidator"))
                            (HsFun
                              (HsPrimType HsPrimCInt)
                              (HsIO
                                (HsPrimType HsPrimUnit))))))
                      (HsFun
                        (HsTypRef
                          (Name
                            "@NsTypeConstr"
                            "DataValidator"))
                        (HsIO
                          (HsPrimType HsPrimUnit))))))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_6621b1bf8ef7af3b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_processMeasurementWithValidation_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_6621b1bf8ef7af3b (void)) (struct Measurement *arg1, void (*arg2) (struct Measurement *arg1, void (*arg2) (struct Measurement *arg1, DataValidator arg2, signed int arg3), DataValidator arg3)) { return &processMeasurementWithValidation; } ",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name "Measurement",
                  nameHsIdent = Identifier
                    "Measurement"}
                NameOriginInSource),
            TypePointer
              (TypeFun
                [
                  TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "Measurement",
                        nameHsIdent = Identifier
                          "Measurement"}
                      NameOriginInSource),
                  TypePointer
                    (TypeFun
                      [
                        TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = Name "Measurement",
                              nameHsIdent = Identifier
                                "Measurement"}
                            NameOriginInSource),
                        TypeTypedef
                          (TypedefRegular
                            NamePair {
                              nameC = Name "DataValidator",
                              nameHsIdent = Identifier
                                "DataValidator"}
                            (TypePointer
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
                      TypeVoid),
                  TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "DataValidator",
                        nameHsIdent = Identifier
                          "DataValidator"}
                      (TypePointer
                        (TypeFun
                          [
                            TypePrim
                              (PrimIntegral PrimInt Signed)]
                          (TypePrim
                            (PrimIntegral
                              PrimInt
                              Signed)))))]
                TypeVoid)]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_callbacks_c34fd33eedc1490d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsFunPtr
                (HsFun
                  (HsTypRef
                    (Name "@NsTypeConstr" "Foo"))
                  (HsIO (HsPrimType HsPrimUnit))))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_c34fd33eedc1490d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_f_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_c34fd33eedc1490d (void)) (void (*arg1) (foo arg1)) { return &f; } ",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeFun
                [
                  TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "foo",
                        nameHsIdent = Identifier "Foo"}
                      (TypePrim
                        (PrimIntegral PrimInt Signed)))]
                TypeVoid)]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_callbacks_490ca7e8c8282a69",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsFunPtr
                (HsFun
                  (HsTypRef
                    (Name "@NsTypeConstr" "Foo2"))
                  (HsIO (HsPrimType HsPrimUnit))))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_490ca7e8c8282a69",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_f2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_490ca7e8c8282a69 (void)) (void (*arg1) (foo2 const arg1)) { return &f2; } ",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeFun
                [
                  TypeQualified
                    TypeQualifierConst
                    (TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "foo2",
                          nameHsIdent = Identifier "Foo2"}
                        (TypePrim
                          (PrimIntegral
                            PrimInt
                            Signed))))]
                TypeVoid)]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
