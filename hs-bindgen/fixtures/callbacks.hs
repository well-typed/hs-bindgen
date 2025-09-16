[
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "readFileWithProcessor",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "processLine"),
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
              commentHeader = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "fileId"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "fileId",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_b5fe3bd1fd70072c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_callbacks_b5fe3bd1fd70072c (void (*arg1) (signed int arg1), signed int arg2) { return readFileWithProcessor(arg1, arg2); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "processLine",
                  nameHsIdent = HsIdentifier
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
                  nameHsIdent = HsIdentifier
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
          commentHeader = Just
            "callbacks.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_callbacks_9bff2165e3decc84",
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
      "hs_bindgen_test_callbacks_9bff2165e3decc84",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_readFileWithProcessor_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_callbacks_9bff2165e3decc84 (void)) (void (*arg1) (signed int arg1), signed int arg2) { return &readFileWithProcessor; } ",
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
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "readFileWithProcessor",
          commentLocation = Just
            "callbacks.h:2:5",
          commentHeader = Just
            "callbacks.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "watchTemperature",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName
              "@NsVar"
              "onTempChange"),
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
              commentHeader = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "sensorId"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "sensorId",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_e444e06c0c5d9e82",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_e444e06c0c5d9e82 (void (*arg1) (signed int arg1), signed int arg2) { watchTemperature(arg1, arg2); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "onTempChange",
                  nameHsIdent = HsIdentifier
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
                  nameHsIdent = HsIdentifier
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
          commentHeader = Just
            "callbacks.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_callbacks_c2f8e0ccab10771e",
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
      "hs_bindgen_test_callbacks_c2f8e0ccab10771e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_watchTemperature_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_c2f8e0ccab10771e (void)) (void (*arg1) (signed int arg1), signed int arg2) { return &watchTemperature; } ",
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
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "watchTemperature",
          commentLocation = Just
            "callbacks.h:3:6",
          commentHeader = Just
            "callbacks.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "FileOpenedNotification",
      newtypeConstr = HsName
        "@NsConstr"
        "FileOpenedNotification",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_FileOpenedNotification",
        fieldType = HsFunPtr
          (HsIO (HsPrimType HsPrimUnit)),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "callbacks.h:6:16",
          declId = NamePair {
            nameC = Name
              "FileOpenedNotification",
            nameHsIdent = HsIdentifier
              "FileOpenedNotification"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "callbacks.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "FileOpenedNotification",
              newtypeField = HsName
                "@NsVar"
                "un_FileOpenedNotification"},
            typedefType = TypePointer
              (TypeFun [] TypeVoid)},
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
            "FileOpenedNotification",
          commentLocation = Just
            "callbacks.h:6:16",
          commentHeader = Just
            "callbacks.h",
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "mkFileOpenedNotification",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsIO
            (HsPrimType HsPrimUnit),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "FileOpenedNotification"))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin =
      GeneratedWrapper,
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeader = Nothing,
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Convert Haskell function",
                TypeSignature
                  (HsIO (HsPrimType HsPrimUnit)),
                TextContent "to",
                Identifier
                  "FileOpenedNotification",
                TextContent
                  "(C function pointer typedef)."]]},
      foreignImportSafety = Safe},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "FileOpenedNotification",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "FileOpenedNotification",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "FileOpenedNotification",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "FileOpenedNotification",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "ProgressUpdate",
      newtypeConstr = HsName
        "@NsConstr"
        "ProgressUpdate",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_ProgressUpdate",
        fieldType = HsFunPtr
          (HsFun
            (HsPrimType HsPrimCInt)
            (HsIO (HsPrimType HsPrimUnit))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "callbacks.h:7:16",
          declId = NamePair {
            nameC = Name "ProgressUpdate",
            nameHsIdent = HsIdentifier
              "ProgressUpdate"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "callbacks.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "ProgressUpdate",
              newtypeField = HsName
                "@NsVar"
                "un_ProgressUpdate"},
            typedefType = TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed)]
                TypeVoid)},
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
            "ProgressUpdate",
          commentLocation = Just
            "callbacks.h:7:16",
          commentHeader = Just
            "callbacks.h",
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "mkProgressUpdate",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFun
            (HsPrimType HsPrimCInt)
            (HsIO (HsPrimType HsPrimUnit)),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "ProgressUpdate"))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin =
      GeneratedWrapper,
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeader = Nothing,
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Convert Haskell function",
                TypeSignature
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsIO (HsPrimType HsPrimUnit))),
                TextContent "to",
                Identifier "ProgressUpdate",
                TextContent
                  "(C function pointer typedef)."]]},
      foreignImportSafety = Safe},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "ProgressUpdate",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "ProgressUpdate",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "ProgressUpdate",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "ProgressUpdate",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "DataValidator",
      newtypeConstr = HsName
        "@NsConstr"
        "DataValidator",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_DataValidator",
        fieldType = HsFunPtr
          (HsFun
            (HsPrimType HsPrimCInt)
            (HsIO (HsPrimType HsPrimCInt))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "callbacks.h:8:15",
          declId = NamePair {
            nameC = Name "DataValidator",
            nameHsIdent = HsIdentifier
              "DataValidator"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "callbacks.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "DataValidator",
              newtypeField = HsName
                "@NsVar"
                "un_DataValidator"},
            typedefType = TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed)]
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
            "DataValidator",
          commentLocation = Just
            "callbacks.h:8:15",
          commentHeader = Just
            "callbacks.h",
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "mkDataValidator",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFun
            (HsPrimType HsPrimCInt)
            (HsIO (HsPrimType HsPrimCInt)),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "DataValidator"))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin =
      GeneratedWrapper,
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeader = Nothing,
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Convert Haskell function",
                TypeSignature
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsIO (HsPrimType HsPrimCInt))),
                TextContent "to",
                Identifier "DataValidator",
                TextContent
                  "(C function pointer typedef)."]]},
      foreignImportSafety = Safe},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "DataValidator",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "DataValidator",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "DataValidator",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "DataValidator",
      deriveInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "onFileOpened",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "notify"),
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "FileOpenedNotification"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "notify",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_bebfa5cb7ff1a841",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_bebfa5cb7ff1a841 (FileOpenedNotification arg1) { onFileOpened(arg1); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "notify",
                  nameHsIdent = HsIdentifier
                    "notify"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name
                      "FileOpenedNotification",
                    nameHsIdent = HsIdentifier
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
          commentHeader = Just
            "callbacks.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_callbacks_b0c6e0e75106190c",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "FileOpenedNotification"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_b0c6e0e75106190c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_onFileOpened_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_b0c6e0e75106190c (void)) (FileOpenedNotification arg1) { return &onFileOpened; } ",
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
                  nameHsIdent = HsIdentifier
                    "FileOpenedNotification"})]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "onFileOpened",
          commentLocation = Just
            "callbacks.h:10:6",
          commentHeader = Just
            "callbacks.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "onProgressChanged",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "update"),
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "ProgressUpdate"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "update",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_f8434b7a5c10a8de",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_f8434b7a5c10a8de (ProgressUpdate arg1) { onProgressChanged(arg1); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "update",
                  nameHsIdent = HsIdentifier
                    "update"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "ProgressUpdate",
                    nameHsIdent = HsIdentifier
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
          commentHeader = Just
            "callbacks.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_callbacks_d5b434b3faaf4465",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "ProgressUpdate"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_d5b434b3faaf4465",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_onProgressChanged_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_d5b434b3faaf4465 (void)) (ProgressUpdate arg1) { return &onProgressChanged; } ",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "ProgressUpdate",
                  nameHsIdent = HsIdentifier
                    "ProgressUpdate"})]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "onProgressChanged",
          commentLocation = Just
            "callbacks.h:11:6",
          commentHeader = Just
            "callbacks.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "validateInput",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "validator"),
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "DataValidator"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "validator",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "rawValue"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "rawValue",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_41dbe3a1204d2401",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_callbacks_41dbe3a1204d2401 (DataValidator arg1, signed int arg2) { return validateInput(arg1, arg2); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "validator",
                  nameHsIdent = HsIdentifier
                    "validator"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "DataValidator",
                    nameHsIdent = HsIdentifier
                      "DataValidator"})),
            _×_
              (Just
                NamePair {
                  nameC = Name "rawValue",
                  nameHsIdent = HsIdentifier
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
          commentHeader = Just
            "callbacks.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_callbacks_b753c64bc11604dd",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "DataValidator"))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_b753c64bc11604dd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_validateInput_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_callbacks_b753c64bc11604dd (void)) (DataValidator arg1, signed int arg2) { return &validateInput; } ",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "DataValidator",
                  nameHsIdent = HsIdentifier
                    "DataValidator"}),
            TypePrim
              (PrimIntegral PrimInt Signed)]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "validateInput",
          commentLocation = Just
            "callbacks.h:12:5",
          commentHeader = Just
            "callbacks.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Measurement",
      structConstr = HsName
        "@NsConstr"
        "Measurement",
      structFields = [
        Field {
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
              commentHeader = Just
                "callbacks.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
              commentHeader = Just
                "callbacks.h",
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "callbacks.h:15:8",
            declId = NamePair {
              nameC = Name "Measurement",
              nameHsIdent = HsIdentifier
                "Measurement"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "callbacks.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
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
                      nameHsIdent = HsIdentifier
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
                      nameHsIdent = HsIdentifier
                        "measurement_timestamp"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 64,
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
            "Measurement",
          commentLocation = Just
            "callbacks.h:15:8",
          commentHeader = Just
            "callbacks.h",
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Measurement",
          structConstr = HsName
            "@NsConstr"
            "Measurement",
          structFields = [
            Field {
              fieldName = HsName
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
                      nameHsIdent = HsIdentifier
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
                  commentHeader = Just
                    "callbacks.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
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
                      nameHsIdent = HsIdentifier
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
                  commentHeader = Just
                    "callbacks.h",
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "callbacks.h:15:8",
                declId = NamePair {
                  nameC = Name "Measurement",
                  nameHsIdent = HsIdentifier
                    "Measurement"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "callbacks.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName
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
                          nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
                            "measurement_timestamp"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimFloating PrimDouble),
                      structFieldOffset = 64,
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
                "Measurement",
              commentLocation = Just
                "callbacks.h:15:8",
              commentHeader = Just
                "callbacks.h",
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 16,
          storableAlignment = 8,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Measurement",
                  structConstr = HsName
                    "@NsConstr"
                    "Measurement",
                  structFields = [
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                          commentHeader = Just
                            "callbacks.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                          commentHeader = Just
                            "callbacks.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "callbacks.h:15:8",
                        declId = NamePair {
                          nameC = Name "Measurement",
                          nameHsIdent = HsIdentifier
                            "Measurement"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "callbacks.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
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
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
                                    "measurement_timestamp"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 64,
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
                        "Measurement",
                      commentLocation = Just
                        "callbacks.h:15:8",
                      commentHeader = Just
                        "callbacks.h",
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
                  structName = HsName
                    "@NsTypeConstr"
                    "Measurement",
                  structConstr = HsName
                    "@NsConstr"
                    "Measurement",
                  structFields = [
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                          commentHeader = Just
                            "callbacks.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                          commentHeader = Just
                            "callbacks.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "callbacks.h:15:8",
                        declId = NamePair {
                          nameC = Name "Measurement",
                          nameHsIdent = HsIdentifier
                            "Measurement"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "callbacks.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
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
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
                                    "measurement_timestamp"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 64,
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
                        "Measurement",
                      commentLocation = Just
                        "callbacks.h:15:8",
                      commentHeader = Just
                        "callbacks.h",
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Measurement",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Measurement",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "MeasurementReceived",
      newtypeConstr = HsName
        "@NsConstr"
        "MeasurementReceived",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_MeasurementReceived",
        fieldType = HsFunPtr
          (HsFun
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Measurement"))
            (HsIO (HsPrimType HsPrimUnit))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "callbacks.h:19:16",
          declId = NamePair {
            nameC = Name
              "MeasurementReceived",
            nameHsIdent = HsIdentifier
              "MeasurementReceived"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "callbacks.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "MeasurementReceived",
              newtypeField = HsName
                "@NsVar"
                "un_MeasurementReceived"},
            typedefType = TypePointer
              (TypeFun
                [
                  TypeStruct
                    NamePair {
                      nameC = Name "Measurement",
                      nameHsIdent = HsIdentifier
                        "Measurement"}
                    NameOriginInSource]
                TypeVoid)},
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
            "MeasurementReceived",
          commentLocation = Just
            "callbacks.h:19:16",
          commentHeader = Just
            "callbacks.h",
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "mkMeasurementReceived",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFun
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Measurement"))
            (HsIO (HsPrimType HsPrimUnit)),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "MeasurementReceived"))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin =
      GeneratedWrapper,
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeader = Nothing,
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Convert Haskell function",
                TypeSignature
                  (HsFun
                    (HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Measurement"))
                    (HsIO (HsPrimType HsPrimUnit))),
                TextContent "to",
                Identifier
                  "MeasurementReceived",
                TextContent
                  "(C function pointer typedef)."]]},
      foreignImportSafety = Safe},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MeasurementReceived",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MeasurementReceived",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MeasurementReceived",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MeasurementReceived",
      deriveInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "onNewMeasurement",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "handler"),
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "MeasurementReceived"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "handler",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_80ae1355e2538a88",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_80ae1355e2538a88 (MeasurementReceived arg1) { onNewMeasurement(arg1); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "handler",
                  nameHsIdent = HsIdentifier
                    "handler"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name
                      "MeasurementReceived",
                    nameHsIdent = HsIdentifier
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
          commentHeader = Just
            "callbacks.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_callbacks_5ede51ad7cee58ce",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "MeasurementReceived"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_5ede51ad7cee58ce",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_onNewMeasurement_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_5ede51ad7cee58ce (void)) (MeasurementReceived arg1) { return &onNewMeasurement; } ",
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
                  nameHsIdent = HsIdentifier
                    "MeasurementReceived"})]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "onNewMeasurement",
          commentLocation = Just
            "callbacks.h:20:6",
          commentHeader = Just
            "callbacks.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "SampleBufferFull",
      newtypeConstr = HsName
        "@NsConstr"
        "SampleBufferFull",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_SampleBufferFull",
        fieldType = HsFunPtr
          (HsFun
            (HsConstArray
              10
              (HsPrimType HsPrimCInt))
            (HsIO (HsPrimType HsPrimUnit))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "callbacks.h:23:16",
          declId = NamePair {
            nameC = Name "SampleBufferFull",
            nameHsIdent = HsIdentifier
              "SampleBufferFull"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "callbacks.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "SampleBufferFull",
              newtypeField = HsName
                "@NsVar"
                "un_SampleBufferFull"},
            typedefType = TypePointer
              (TypeFun
                [
                  TypeConstArray
                    10
                    (TypePrim
                      (PrimIntegral PrimInt Signed))]
                TypeVoid)},
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
            "SampleBufferFull",
          commentLocation = Just
            "callbacks.h:23:16",
          commentHeader = Just
            "callbacks.h",
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "mkSampleBufferFull",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFun
            (HsConstArray
              10
              (HsPrimType HsPrimCInt))
            (HsIO (HsPrimType HsPrimUnit)),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "SampleBufferFull"))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin =
      GeneratedWrapper,
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeader = Nothing,
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Convert Haskell function",
                TypeSignature
                  (HsFun
                    (HsConstArray
                      10
                      (HsPrimType HsPrimCInt))
                    (HsIO (HsPrimType HsPrimUnit))),
                TextContent "to",
                Identifier "SampleBufferFull",
                TextContent
                  "(C function pointer typedef)."]]},
      foreignImportSafety = Safe},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "SampleBufferFull",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "SampleBufferFull",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "SampleBufferFull",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "SampleBufferFull",
      deriveInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "onBufferReady",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "handler"),
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "SampleBufferFull"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "handler",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_c537c4b0f5f57239",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_callbacks_c537c4b0f5f57239 (SampleBufferFull arg1) { onBufferReady(arg1); }",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "handler",
                  nameHsIdent = HsIdentifier
                    "handler"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "SampleBufferFull",
                    nameHsIdent = HsIdentifier
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
            "callbacks.h:24:6",
          commentHeader = Just
            "callbacks.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_callbacks_514203bcc8a16ea6",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "SampleBufferFull"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_callbacks_514203bcc8a16ea6",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_onBufferReady_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_514203bcc8a16ea6 (void)) (SampleBufferFull arg1) { return &onBufferReady; } ",
          capiWrapperImport =
          "callbacks.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "SampleBufferFull",
                  nameHsIdent = HsIdentifier
                    "SampleBufferFull"})]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "onBufferReady",
          commentLocation = Just
            "callbacks.h:24:6",
          commentHeader = Just
            "callbacks.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "wrapFunPtr_CInt_to_Unit",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsFun
            (HsPrimType HsPrimCInt)
            (HsIO (HsPrimType HsPrimUnit)),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "wrapper",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsValue,
      foreignImportOrigin =
      GeneratedWrapper,
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeader = Nothing,
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Convert Haskell function",
                TypeSignature
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsIO (HsPrimType HsPrimUnit))),
                TextContent
                  "to C function pointer."]]},
      foreignImportSafety = Safe}]
