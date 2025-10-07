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
          declLoc = "callbacks.h:6:16",
          declId = NamePair {
            nameC = Name
              "FileOpenedNotification_Deref",
            nameHsIdent = Identifier
              "FileOpenedNotification_Deref"},
          declOrigin = NameOriginGenerated
            (AnonId "callbacks.h:6:16"),
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
            "callbacks.h:6:16",
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
          declLoc = "callbacks.h:6:16",
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
                      "FileOpenedNotification_Deref"}))},
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
            "callbacks.h:6:16",
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
          declLoc = "callbacks.h:7:16",
          declId = NamePair {
            nameC = Name
              "ProgressUpdate_Deref",
            nameHsIdent = Identifier
              "ProgressUpdate_Deref"},
          declOrigin = NameOriginGenerated
            (AnonId "callbacks.h:7:16"),
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
            "callbacks.h:7:16",
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
          declLoc = "callbacks.h:7:16",
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
                      "ProgressUpdate_Deref"}))},
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
            "callbacks.h:7:16",
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
          declLoc = "callbacks.h:8:15",
          declId = NamePair {
            nameC = Name
              "DataValidator_Deref",
            nameHsIdent = Identifier
              "DataValidator_Deref"},
          declOrigin = NameOriginGenerated
            (AnonId "callbacks.h:8:15"),
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
            "callbacks.h:8:15",
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
          declLoc = "callbacks.h:8:15",
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
                      "DataValidator_Deref"}))},
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
            "callbacks.h:8:15",
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
                fieldLoc = "callbacks.h:16:10",
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
                "callbacks.h:16:10",
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
                fieldLoc = "callbacks.h:16:17",
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
                "callbacks.h:16:17",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["callbacks.h"],
                  headerInclude = "callbacks.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "callbacks.h:15:8",
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
                    fieldLoc = "callbacks.h:16:10",
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
                    fieldLoc = "callbacks.h:16:17",
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
            "callbacks.h:15:8",
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
                    fieldLoc = "callbacks.h:16:10",
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
                    "callbacks.h:16:10",
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
                    fieldLoc = "callbacks.h:16:17",
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
                    "callbacks.h:16:17",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["callbacks.h"],
                      headerInclude = "callbacks.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "callbacks.h:15:8",
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
                        fieldLoc = "callbacks.h:16:10",
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
                        fieldLoc = "callbacks.h:16:17",
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
                "callbacks.h:15:8",
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
                            fieldLoc = "callbacks.h:16:10",
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
                            "callbacks.h:16:10",
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
                            fieldLoc = "callbacks.h:16:17",
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
                            "callbacks.h:16:17",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "callbacks.h:15:8",
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
                                fieldLoc = "callbacks.h:16:10",
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
                                fieldLoc = "callbacks.h:16:17",
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
                        "callbacks.h:15:8",
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
                            fieldLoc = "callbacks.h:16:10",
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
                            "callbacks.h:16:10",
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
                            fieldLoc = "callbacks.h:16:17",
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
                            "callbacks.h:16:17",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "callbacks.h:15:8",
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
                                fieldLoc = "callbacks.h:16:10",
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
                                fieldLoc = "callbacks.h:16:17",
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
                        "callbacks.h:15:8",
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
          declLoc = "callbacks.h:19:16",
          declId = NamePair {
            nameC = Name
              "MeasurementReceived_Deref",
            nameHsIdent = Identifier
              "MeasurementReceived_Deref"},
          declOrigin = NameOriginGenerated
            (AnonId "callbacks.h:19:16"),
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
            "callbacks.h:19:16",
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
          declLoc = "callbacks.h:19:16",
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
                      "MeasurementReceived_Deref"}))},
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
            "callbacks.h:19:16",
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
          declLoc = "callbacks.h:22:16",
          declId = NamePair {
            nameC = Name
              "MeasurementReceived2_Deref",
            nameHsIdent = Identifier
              "MeasurementReceived2_Deref"},
          declOrigin = NameOriginGenerated
            (AnonId "callbacks.h:22:16"),
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
            "callbacks.h:22:16",
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
          declLoc = "callbacks.h:22:16",
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
                      "MeasurementReceived2_Deref"}))},
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
            "callbacks.h:22:16",
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
          declLoc = "callbacks.h:25:16",
          declId = NamePair {
            nameC = Name
              "SampleBufferFull_Deref",
            nameHsIdent = Identifier
              "SampleBufferFull_Deref"},
          declOrigin = NameOriginGenerated
            (AnonId "callbacks.h:25:16"),
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
            "callbacks.h:25:16",
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
          declLoc = "callbacks.h:25:16",
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
                      "SampleBufferFull_Deref"}))},
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
            "callbacks.h:25:16",
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
            "callbacks.h:2:5",
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
            "callbacks.h:3:6",
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
                      "FileOpenedNotification"}))],
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
            "callbacks.h:10:6",
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
                      "ProgressUpdate"}))],
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
            "callbacks.h:11:6",
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
                      "DataValidator"})),
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
            "callbacks.h:12:5",
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
                      "MeasurementReceived"}))],
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
            "callbacks.h:20:6",
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
                      "MeasurementReceived2"}))],
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
            "callbacks.h:23:6",
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
                      "SampleBufferFull"}))],
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
            "callbacks.h:26:6",
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
            "callbacks.h:2:5",
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
            "callbacks.h:3:6",
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
                      "FileOpenedNotification"}))],
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
            "callbacks.h:10:6",
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
                      "ProgressUpdate"}))],
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
            "callbacks.h:11:6",
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
                      "DataValidator"})),
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
            "callbacks.h:12:5",
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
                      "MeasurementReceived"}))],
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
            "callbacks.h:20:6",
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
                      "MeasurementReceived2"}))],
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
            "callbacks.h:23:6",
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
                      "SampleBufferFull"}))],
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
            "callbacks.h:26:6",
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
                    "FileOpenedNotification"})]
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
                    "ProgressUpdate"})]
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
                    "DataValidator"}),
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
                    "MeasurementReceived"})]
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
                    "MeasurementReceived2"})]
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
                    "SampleBufferFull"})]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
