[
  DeclInlineCInclude "array.h",
  DeclInlineC
    "__attribute__ ((const)) signed int (*get_arr0_ptr (void))[3] { return &arr0; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "arr0_ptr",
      foreignImportType = HsPtr
        (HsConstArray
          3
          (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "get_arr0_ptr",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeConstArray
          3
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Global, complete, not initialised"],
          commentOrigin = Just "arr0",
          commentChildren = []}},
  DeclInlineCInclude "array.h",
  DeclInlineC
    "__attribute__ ((const)) signed int (*get_arr1_ptr (void))[3] { return &arr1; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "arr1_ptr",
      foreignImportType = HsPtr
        (HsConstArray
          3
          (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "get_arr1_ptr",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeConstArray
          3
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Global, complete, initialised"],
          commentOrigin = Just "arr1",
          commentChildren = []}},
  DeclInlineCInclude "array.h",
  DeclInlineC
    "__attribute__ ((const)) signed int (*get_arr2_ptr (void))[3] { return &arr2; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "arr2_ptr",
      foreignImportType = HsPtr
        (HsConstArray
          3
          (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "get_arr2_ptr",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeConstArray
          3
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Global, extern, complete, not initialised"],
          commentOrigin = Just "arr2",
          commentChildren = []}},
  DeclInlineCInclude "array.h",
  DeclInlineC
    "__attribute__ ((const)) signed int (*get_arr3_ptr (void))[3] { return &arr3; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "arr3_ptr",
      foreignImportType = HsPtr
        (HsConstArray
          3
          (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "get_arr3_ptr",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeConstArray
          3
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Global, extern, complete, initialised"],
          commentOrigin = Just "arr3",
          commentChildren = []}},
  DeclInlineCInclude "array.h",
  DeclInlineC
    "__attribute__ ((const)) signed int (*get_arr6_ptr (void))[1] { return &arr6; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "arr6_ptr",
      foreignImportType = HsPtr
        (HsConstArray
          1
          (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "get_arr6_ptr",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeConstArray
          1
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Global, incomplete"],
          commentOrigin = Just "arr6",
          commentChildren = []}},
  DeclInlineCInclude "array.h",
  DeclInlineC
    "__attribute__ ((const)) signed int (*get_arr7_ptr (void))[] { return &arr7; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "arr7_ptr",
      foreignImportType = HsPtr
        (HsIncompleteArray
          (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "get_arr7_ptr",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeIncompleteArray
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Global, extern, incomplete"],
          commentOrigin = Just "arr7",
          commentChildren = []}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Triplet",
      newtypeConstr = HsName
        "@NsConstr"
        "Triplet",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Triplet",
        fieldType = HsConstArray
          3
          (HsPrimType HsPrimCInt),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "array.h:41:13",
          declId = NamePair {
            nameC = Name "triplet",
            nameHsIdent = HsIdentifier
              "Triplet"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "array.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Triplet",
              newtypeField = HsName
                "@NsVar"
                "un_Triplet"},
            typedefType = TypeConstArray
              3
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable],
      newtypeComment = Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Triplet",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Triplet",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Triplet",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "List",
      newtypeConstr = HsName
        "@NsConstr"
        "List",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_List",
        fieldType = HsIncompleteArray
          (HsPrimType HsPrimCInt),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "array.h:43:13",
          declId = NamePair {
            nameC = Name "list",
            nameHsIdent = HsIdentifier
              "List"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "array.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "List",
              newtypeField = HsName
                "@NsVar"
                "un_List"},
            typedefType =
            TypeIncompleteArray
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Show],
      newtypeComment = Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "List",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "List",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Matrix",
      newtypeConstr = HsName
        "@NsConstr"
        "Matrix",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Matrix",
        fieldType = HsConstArray
          4
          (HsConstArray
            3
            (HsPrimType HsPrimCInt)),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "array.h:45:13",
          declId = NamePair {
            nameC = Name "matrix",
            nameHsIdent = HsIdentifier
              "Matrix"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "array.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Matrix",
              newtypeField = HsName
                "@NsVar"
                "un_Matrix"},
            typedefType = TypeConstArray
              4
              (TypeConstArray
                3
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
        [Eq, Show, Storable],
      newtypeComment = Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Matrix",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Matrix",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Matrix",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Tripletlist",
      newtypeConstr = HsName
        "@NsConstr"
        "Tripletlist",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Tripletlist",
        fieldType = HsIncompleteArray
          (HsConstArray
            3
            (HsPrimType HsPrimCInt)),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "array.h:47:13",
          declId = NamePair {
            nameC = Name "tripletlist",
            nameHsIdent = HsIdentifier
              "Tripletlist"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "array.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Tripletlist",
              newtypeField = HsName
                "@NsVar"
                "un_Tripletlist"},
            typedefType =
            TypeIncompleteArray
              (TypeConstArray
                3
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
        [Eq, Show],
      newtypeComment = Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Tripletlist",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Tripletlist",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Example",
      structConstr = HsName
        "@NsConstr"
        "Example",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "example_triple",
          fieldType = HsConstArray
            3
            (HsPrimType HsPrimCInt),
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "array.h:50:9",
              structFieldName = NamePair {
                nameC = Name "triple",
                nameHsIdent = HsIdentifier
                  "example_triple"},
              structFieldType = TypeConstArray
                3
                (TypePrim
                  (PrimIntegral PrimInt Signed)),
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing},
        Field {
          fieldName = HsName
            "@NsVar"
            "example_sudoku",
          fieldType = HsConstArray
            3
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "array.h:51:9",
              structFieldName = NamePair {
                nameC = Name "sudoku",
                nameHsIdent = HsIdentifier
                  "example_sudoku"},
              structFieldType = TypeConstArray
                3
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral PrimInt Signed))),
              structFieldOffset = 96,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "array.h:49:8",
            declId = NamePair {
              nameC = Name "Example",
              nameHsIdent = HsIdentifier
                "Example"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "array.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Example"),
              structSizeof = 48,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc = "array.h:50:9",
                  structFieldName = NamePair {
                    nameC = Name "triple",
                    nameHsIdent = HsIdentifier
                      "example_triple"},
                  structFieldType = TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral PrimInt Signed)),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
                StructField {
                  structFieldLoc = "array.h:51:9",
                  structFieldName = NamePair {
                    nameC = Name "sudoku",
                    nameHsIdent = HsIdentifier
                      "example_sudoku"},
                  structFieldType = TypeConstArray
                    3
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral PrimInt Signed))),
                  structFieldOffset = 96,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            TypeSpec {
              typeSpecModule = Nothing,
              typeSpecIdentifier = Nothing,
              typeSpecInstances = Map.fromList
                []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Example",
          structConstr = HsName
            "@NsConstr"
            "Example",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "example_triple",
              fieldType = HsConstArray
                3
                (HsPrimType HsPrimCInt),
              fieldOrigin = StructField
                StructField {
                  structFieldLoc = "array.h:50:9",
                  structFieldName = NamePair {
                    nameC = Name "triple",
                    nameHsIdent = HsIdentifier
                      "example_triple"},
                  structFieldType = TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral PrimInt Signed)),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing},
            Field {
              fieldName = HsName
                "@NsVar"
                "example_sudoku",
              fieldType = HsConstArray
                3
                (HsConstArray
                  3
                  (HsPrimType HsPrimCInt)),
              fieldOrigin = StructField
                StructField {
                  structFieldLoc = "array.h:51:9",
                  structFieldName = NamePair {
                    nameC = Name "sudoku",
                    nameHsIdent = HsIdentifier
                      "example_sudoku"},
                  structFieldType = TypeConstArray
                    3
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral PrimInt Signed))),
                  structFieldOffset = 96,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "array.h:49:8",
                declId = NamePair {
                  nameC = Name "Example",
                  nameHsIdent = HsIdentifier
                    "Example"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "array.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Example"),
                  structSizeof = 48,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldLoc = "array.h:50:9",
                      structFieldName = NamePair {
                        nameC = Name "triple",
                        nameHsIdent = HsIdentifier
                          "example_triple"},
                      structFieldType = TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral PrimInt Signed)),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing},
                    StructField {
                      structFieldLoc = "array.h:51:9",
                      structFieldName = NamePair {
                        nameC = Name "sudoku",
                        nameHsIdent = HsIdentifier
                          "example_sudoku"},
                      structFieldType = TypeConstArray
                        3
                        (TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral PrimInt Signed))),
                      structFieldOffset = 96,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                TypeSpec {
                  typeSpecModule = Nothing,
                  typeSpecIdentifier = Nothing,
                  typeSpecInstances = Map.fromList
                    []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Nothing}
        StorableInstance {
          storableSizeOf = 48,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Example",
                  structConstr = HsName
                    "@NsConstr"
                    "Example",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_triple",
                      fieldType = HsConstArray
                        3
                        (HsPrimType HsPrimCInt),
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "array.h:50:9",
                          structFieldName = NamePair {
                            nameC = Name "triple",
                            nameHsIdent = HsIdentifier
                              "example_triple"},
                          structFieldType = TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral PrimInt Signed)),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_sudoku",
                      fieldType = HsConstArray
                        3
                        (HsConstArray
                          3
                          (HsPrimType HsPrimCInt)),
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "array.h:51:9",
                          structFieldName = NamePair {
                            nameC = Name "sudoku",
                            nameHsIdent = HsIdentifier
                              "example_sudoku"},
                          structFieldType = TypeConstArray
                            3
                            (TypeConstArray
                              3
                              (TypePrim
                                (PrimIntegral PrimInt Signed))),
                          structFieldOffset = 96,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "array.h:49:8",
                        declId = NamePair {
                          nameC = Name "Example",
                          nameHsIdent = HsIdentifier
                            "Example"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "array.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Example"),
                          structSizeof = 48,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc = "array.h:50:9",
                              structFieldName = NamePair {
                                nameC = Name "triple",
                                nameHsIdent = HsIdentifier
                                  "example_triple"},
                              structFieldType = TypeConstArray
                                3
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc = "array.h:51:9",
                              structFieldName = NamePair {
                                nameC = Name "sudoku",
                                nameHsIdent = HsIdentifier
                                  "example_sudoku"},
                              structFieldType = TypeConstArray
                                3
                                (TypeConstArray
                                  3
                                  (TypePrim
                                    (PrimIntegral PrimInt Signed))),
                              structFieldOffset = 96,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Nothing})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 12]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Example",
                  structConstr = HsName
                    "@NsConstr"
                    "Example",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_triple",
                      fieldType = HsConstArray
                        3
                        (HsPrimType HsPrimCInt),
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "array.h:50:9",
                          structFieldName = NamePair {
                            nameC = Name "triple",
                            nameHsIdent = HsIdentifier
                              "example_triple"},
                          structFieldType = TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral PrimInt Signed)),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_sudoku",
                      fieldType = HsConstArray
                        3
                        (HsConstArray
                          3
                          (HsPrimType HsPrimCInt)),
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "array.h:51:9",
                          structFieldName = NamePair {
                            nameC = Name "sudoku",
                            nameHsIdent = HsIdentifier
                              "example_sudoku"},
                          structFieldType = TypeConstArray
                            3
                            (TypeConstArray
                              3
                              (TypePrim
                                (PrimIntegral PrimInt Signed))),
                          structFieldOffset = 96,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "array.h:49:8",
                        declId = NamePair {
                          nameC = Name "Example",
                          nameHsIdent = HsIdentifier
                            "Example"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "array.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Example"),
                          structSizeof = 48,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc = "array.h:50:9",
                              structFieldName = NamePair {
                                nameC = Name "triple",
                                nameHsIdent = HsIdentifier
                                  "example_triple"},
                              structFieldType = TypeConstArray
                                3
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc = "array.h:51:9",
                              structFieldName = NamePair {
                                nameC = Name "sudoku",
                                nameHsIdent = HsIdentifier
                                  "example_sudoku"},
                              structFieldType = TypeConstArray
                                3
                                (TypeConstArray
                                  3
                                  (TypePrim
                                    (PrimIntegral PrimInt Signed))),
                              structFieldOffset = 96,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Nothing}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      12
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
        "Example",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Example",
      deriveInstanceComment =
      Nothing},
  DeclInlineCInclude "array.h",
  DeclInlineC
    "__attribute__ ((const)) signed int (*get_arr_1_ptr (void))[3] { return &arr_1; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "arr_1_ptr",
      foreignImportType = HsPtr
        (HsConstArray
          3
          (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "get_arr_1_ptr",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeConstArray
          3
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of known size"],
          commentOrigin = Just "arr_1",
          commentChildren = []}},
  DeclInlineCInclude "array.h",
  DeclInlineC
    "__attribute__ ((const)) triplet *get_arr_2_ptr (void) { return &arr_2; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "arr_2_ptr",
      foreignImportType = HsPtr
        (HsTypRef
          (HsName
            "@NsTypeConstr"
            "Triplet")),
      foreignImportOrigName =
      "get_arr_2_ptr",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeTypedef
          (TypedefRegular
            NamePair {
              nameC = Name "triplet",
              nameHsIdent = HsIdentifier
                "Triplet"})),
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of known size, typedef"],
          commentOrigin = Just "arr_2",
          commentChildren = []}},
  DeclInlineCInclude "array.h",
  DeclInlineC
    "__attribute__ ((const)) signed int (*get_arr_3_ptr (void))[] { return &arr_3; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "arr_3_ptr",
      foreignImportType = HsPtr
        (HsIncompleteArray
          (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "get_arr_3_ptr",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeIncompleteArray
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of unknown size"],
          commentOrigin = Just "arr_3",
          commentChildren = []}},
  DeclInlineCInclude "array.h",
  DeclInlineC
    "__attribute__ ((const)) list *get_arr_4_ptr (void) { return &arr_4; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "arr_4_ptr",
      foreignImportType = HsPtr
        (HsTypRef
          (HsName
            "@NsTypeConstr"
            "List")),
      foreignImportOrigName =
      "get_arr_4_ptr",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeTypedef
          (TypedefRegular
            NamePair {
              nameC = Name "list",
              nameHsIdent = HsIdentifier
                "List"})),
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of unknown size, typedef"],
          commentOrigin = Just "arr_4",
          commentChildren = []}},
  DeclInlineCInclude "array.h",
  DeclInlineC
    "__attribute__ ((const)) signed int (*get_arr_5_ptr (void))[4][3] { return &arr_5; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "arr_5_ptr",
      foreignImportType = HsPtr
        (HsConstArray
          4
          (HsConstArray
            3
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "get_arr_5_ptr",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeConstArray
          4
          (TypeConstArray
            3
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of known size"],
          commentOrigin = Just "arr_5",
          commentChildren = []}},
  DeclInlineCInclude "array.h",
  DeclInlineC
    "__attribute__ ((const)) matrix *get_arr_6_ptr (void) { return &arr_6; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "arr_6_ptr",
      foreignImportType = HsPtr
        (HsTypRef
          (HsName
            "@NsTypeConstr"
            "Matrix")),
      foreignImportOrigName =
      "get_arr_6_ptr",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeTypedef
          (TypedefRegular
            NamePair {
              nameC = Name "matrix",
              nameHsIdent = HsIdentifier
                "Matrix"})),
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of known size, typedef"],
          commentOrigin = Just "arr_6",
          commentChildren = []}},
  DeclInlineCInclude "array.h",
  DeclInlineC
    "__attribute__ ((const)) signed int (*get_arr_7_ptr (void))[][3] { return &arr_7; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "arr_7_ptr",
      foreignImportType = HsPtr
        (HsIncompleteArray
          (HsConstArray
            3
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "get_arr_7_ptr",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeIncompleteArray
          (TypeConstArray
            3
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of unknown size"],
          commentOrigin = Just "arr_7",
          commentChildren = []}},
  DeclInlineCInclude "array.h",
  DeclInlineC
    "__attribute__ ((const)) tripletlist *get_arr_8_ptr (void) { return &arr_8; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "arr_8_ptr",
      foreignImportType = HsPtr
        (HsTypRef
          (HsName
            "@NsTypeConstr"
            "Tripletlist")),
      foreignImportOrigName =
      "get_arr_8_ptr",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeTypedef
          (TypedefRegular
            NamePair {
              nameC = Name "tripletlist",
              nameHsIdent = HsIdentifier
                "Tripletlist"})),
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of unknown size, typedef"],
          commentOrigin = Just "arr_8",
          commentChildren = []}},
  DeclInlineCInclude "array.h",
  DeclInlineC
    "signed int test_internal_fun_1 (signed int arg1, signed int *arg2) { return fun_1(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fun_1_wrapper",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsFun
          (HsPtr (HsPrimType HsPrimCInt))
          (HsIO (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "test_internal_fun_1",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypeConstArray
              3
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of known size"],
          commentOrigin = Just
            "fun_1(int, int *)",
          commentChildren = []}},
  DeclSimple,
  DeclInlineCInclude "array.h",
  DeclInlineC
    "signed int test_internal_fun_2 (signed int *arg1) { return fun_2(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fun_2_wrapper",
      foreignImportType = HsFun
        (HsPtr (HsPrimType HsPrimCInt))
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "test_internal_fun_2",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "triplet",
                  nameHsIdent = HsIdentifier
                    "Triplet"})],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of known size, typedef"],
          commentOrigin = Just
            "fun_2(int *)",
          commentChildren = []}},
  DeclSimple,
  DeclInlineCInclude "array.h",
  DeclInlineC
    "signed int test_internal_fun_3 (signed int *arg1) { return fun_3(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fun_3_wrapper",
      foreignImportType = HsFun
        (HsPtr (HsPrimType HsPrimCInt))
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "test_internal_fun_3",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypeIncompleteArray
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of unknown size"],
          commentOrigin = Just
            "fun_3(int *)",
          commentChildren = []}},
  DeclSimple,
  DeclInlineCInclude "array.h",
  DeclInlineC
    "signed int test_internal_fun_4 (signed int *arg1) { return fun_4(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fun_4_wrapper",
      foreignImportType = HsFun
        (HsPtr (HsPrimType HsPrimCInt))
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "test_internal_fun_4",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "list",
                  nameHsIdent = HsIdentifier
                    "List"})],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of unknown size, typedef"],
          commentOrigin = Just
            "fun_4(int *)",
          commentChildren = []}},
  DeclSimple,
  DeclInlineCInclude "array.h",
  DeclInlineC
    "signed int test_internal_fun_5 (signed int (*arg1)[3]) { return fun_5(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fun_5_wrapper",
      foreignImportType = HsFun
        (HsPtr
          (HsConstArray
            3
            (HsPrimType HsPrimCInt)))
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "test_internal_fun_5",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypeConstArray
              4
              (TypeConstArray
                3
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of known size"],
          commentOrigin = Just
            "fun_5(int (*)[3])",
          commentChildren = []}},
  DeclSimple,
  DeclInlineCInclude "array.h",
  DeclInlineC
    "signed int test_internal_fun_6 (signed int (*arg1)[3]) { return fun_6(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fun_6_wrapper",
      foreignImportType = HsFun
        (HsPtr
          (HsConstArray
            3
            (HsPrimType HsPrimCInt)))
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "test_internal_fun_6",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "matrix",
                  nameHsIdent = HsIdentifier
                    "Matrix"})],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of known size, typedef"],
          commentOrigin = Just
            "fun_6(int (*)[3])",
          commentChildren = []}},
  DeclSimple,
  DeclInlineCInclude "array.h",
  DeclInlineC
    "signed int test_internal_fun_7 (signed int (*arg1)[3]) { return fun_7(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fun_7_wrapper",
      foreignImportType = HsFun
        (HsPtr
          (HsConstArray
            3
            (HsPrimType HsPrimCInt)))
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "test_internal_fun_7",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypeIncompleteArray
              (TypeConstArray
                3
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of unknown size"],
          commentOrigin = Just
            "fun_7(int (*)[3])",
          commentChildren = []}},
  DeclSimple,
  DeclInlineCInclude "array.h",
  DeclInlineC
    "signed int test_internal_fun_8 (signed int (*arg1)[3]) { return fun_8(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fun_8_wrapper",
      foreignImportType = HsFun
        (HsPtr
          (HsConstArray
            3
            (HsPrimType HsPrimCInt)))
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "test_internal_fun_8",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "tripletlist",
                  nameHsIdent = HsIdentifier
                    "Tripletlist"})],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of unknown size, typedef"],
          commentOrigin = Just
            "fun_8(int (*)[3])",
          commentChildren = []}},
  DeclSimple,
  DeclInlineCInclude "array.h",
  DeclInlineC
    "signed int (*test_internal_fun_9 (void))[3] { return fun_9(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fun_9",
      foreignImportType = HsIO
        (HsPtr
          (HsConstArray
            3
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "test_internal_fun_9",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeConstArray
              3
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed)))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of known size"],
          commentOrigin = Just "fun_9()",
          commentChildren = []}},
  DeclInlineCInclude "array.h",
  DeclInlineC
    "triplet *test_internal_fun_10 (void) { return fun_10(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fun_10",
      foreignImportType = HsIO
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Triplet"))),
      foreignImportOrigName =
      "test_internal_fun_10",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "triplet",
                  nameHsIdent = HsIdentifier
                    "Triplet"}))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of known size, typedef"],
          commentOrigin = Just "fun_10()",
          commentChildren = []}},
  DeclInlineCInclude "array.h",
  DeclInlineC
    "signed int (*test_internal_fun_11 (void))[] { return fun_11(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fun_11",
      foreignImportType = HsIO
        (HsPtr
          (HsIncompleteArray
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "test_internal_fun_11",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeIncompleteArray
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed)))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of unknown size"],
          commentOrigin = Just "fun_11()",
          commentChildren = []}},
  DeclInlineCInclude "array.h",
  DeclInlineC
    "list *test_internal_fun_12 (void) { return fun_12(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fun_12",
      foreignImportType = HsIO
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "List"))),
      foreignImportOrigName =
      "test_internal_fun_12",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "list",
                  nameHsIdent = HsIdentifier
                    "List"}))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of unknown size, typedef"],
          commentOrigin = Just "fun_12()",
          commentChildren = []}},
  DeclInlineCInclude "array.h",
  DeclInlineC
    "signed int (*test_internal_fun_13 (void))[4][3] { return fun_13(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fun_13",
      foreignImportType = HsIO
        (HsPtr
          (HsConstArray
            4
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "test_internal_fun_13",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeConstArray
              4
              (TypeConstArray
                3
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of known size"],
          commentOrigin = Just "fun_13()",
          commentChildren = []}},
  DeclInlineCInclude "array.h",
  DeclInlineC
    "matrix *test_internal_fun_14 (void) { return fun_14(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fun_14",
      foreignImportType = HsIO
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Matrix"))),
      foreignImportOrigName =
      "test_internal_fun_14",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "matrix",
                  nameHsIdent = HsIdentifier
                    "Matrix"}))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of known size, typedef"],
          commentOrigin = Just "fun_14()",
          commentChildren = []}},
  DeclInlineCInclude "array.h",
  DeclInlineC
    "signed int (*test_internal_fun_15 (void))[][3] { return fun_15(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fun_15",
      foreignImportType = HsIO
        (HsPtr
          (HsIncompleteArray
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "test_internal_fun_15",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeIncompleteArray
              (TypeConstArray
                3
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of unknown size"],
          commentOrigin = Just "fun_15()",
          commentChildren = []}},
  DeclInlineCInclude "array.h",
  DeclInlineC
    "tripletlist *test_internal_fun_16 (void) { return fun_16(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fun_16",
      foreignImportType = HsIO
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Tripletlist"))),
      foreignImportOrigName =
      "test_internal_fun_16",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "tripletlist",
                  nameHsIdent = HsIdentifier
                    "Tripletlist"}))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of unknown size, typedef"],
          commentOrigin = Just "fun_16()",
          commentChildren = []}}]
