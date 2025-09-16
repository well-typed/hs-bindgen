[
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
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
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
          commentHeaderInfo = Nothing,
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
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
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
          commentHeaderInfo = Nothing,
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
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
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
          commentHeaderInfo = Nothing,
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["callbacks.h"],
                  headerInclude = "callbacks.h"},
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
              nameHsIdent = HsIdentifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["callbacks.h"],
                      headerInclude = "callbacks.h"},
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
                  nameHsIdent = HsIdentifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
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
                          nameHsIdent = HsIdentifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["callbacks.h"],
                              headerInclude = "callbacks.h"},
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
                          nameHsIdent = HsIdentifier
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
            (HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Measurement")))
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
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
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
                  TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "Measurement",
                        nameHsIdent = HsIdentifier
                          "Measurement"}
                      NameOriginInSource)]
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
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
            (HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Measurement")))
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
          commentHeaderInfo = Nothing,
          commentChildren = [
            Paragraph
              [
                TextContent
                  "Convert Haskell function",
                TypeSignature
                  (HsFun
                    (HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Measurement")))
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
              commentHeaderInfo = Nothing,
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
              commentHeaderInfo = Nothing,
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
              commentHeaderInfo = Nothing,
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
              commentHeaderInfo = Nothing,
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
              commentHeaderInfo = Nothing,
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
              commentHeaderInfo = Nothing,
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
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
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
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
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_callbacks_8167a5b82d621c9d",
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_callbacks_ef51ad75ce9862a3",
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_callbacks_9eaedb1b1c5b3fdb",
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_callbacks_f9f4f5ec3dd82431",
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["callbacks.h"],
              headerInclude = "callbacks.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
