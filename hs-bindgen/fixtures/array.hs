[
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Triplet",
      newtypeConstr = Name
        "@NsConstr"
        "Triplet",
      newtypeField = Field {
        fieldName = Name
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
            nameHsIdent = Identifier
              "Triplet"},
          declOrigin = NameOriginInSource,
          declAliases = [Name "sudoku"],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Triplet",
              newtypeField = Name
                "@NsVar"
                "un_Triplet"},
            typedefType = TypeConstArray
              3
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "triplet",
          commentLocation = Just
            "array.h:41:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Triplet",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Triplet",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Triplet",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "List",
      newtypeConstr = Name
        "@NsConstr"
        "List",
      newtypeField = Field {
        fieldName = Name
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
            nameHsIdent = Identifier
              "List"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "List",
              newtypeField = Name
                "@NsVar"
                "un_List"},
            typedefType =
            TypeIncompleteArray
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [Eq, Show],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "list",
          commentLocation = Just
            "array.h:43:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "List",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "List",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Matrix",
      newtypeConstr = Name
        "@NsConstr"
        "Matrix",
      newtypeField = Field {
        fieldName = Name
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
            nameHsIdent = Identifier
              "Matrix"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Matrix",
              newtypeField = Name
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
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "matrix",
          commentLocation = Just
            "array.h:45:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Matrix",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Matrix",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Matrix",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Tripletlist",
      newtypeConstr = Name
        "@NsConstr"
        "Tripletlist",
      newtypeField = Field {
        fieldName = Name
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
            nameHsIdent = Identifier
              "Tripletlist"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Tripletlist",
              newtypeField = Name
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
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [Eq, Show],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "tripletlist",
          commentLocation = Just
            "array.h:47:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Tripletlist",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Tripletlist",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Example",
      structConstr = Name
        "@NsConstr"
        "Example",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "example_triple",
          fieldType = HsConstArray
            3
            (HsPrimType HsPrimCInt),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "array.h:50:9",
                fieldName = NamePair {
                  nameC = Name "triple",
                  nameHsIdent = Identifier
                    "example_triple"},
                fieldComment = Nothing},
              structFieldType = TypeConstArray
                3
                (TypePrim
                  (PrimIntegral PrimInt Signed)),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "triple",
              commentLocation = Just
                "array.h:50:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["array.h"],
                  headerInclude = "array.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "example_sudoku",
          fieldType = HsConstArray
            3
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "array.h:51:9",
                fieldName = NamePair {
                  nameC = Name "sudoku",
                  nameHsIdent = Identifier
                    "example_sudoku"},
                fieldComment = Nothing},
              structFieldType = TypeConstArray
                3
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral PrimInt Signed))),
              structFieldOffset = 96,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "sudoku",
              commentLocation = Just
                "array.h:51:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["array.h"],
                  headerInclude = "array.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "array.h:49:8",
            declId = NamePair {
              nameC = Name "Example",
              nameHsIdent = Identifier
                "Example"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["array.h"],
                headerInclude = "array.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Example"),
              structSizeof = 48,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "array.h:50:9",
                    fieldName = NamePair {
                      nameC = Name "triple",
                      nameHsIdent = Identifier
                        "example_triple"},
                    fieldComment = Nothing},
                  structFieldType = TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral PrimInt Signed)),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "array.h:51:9",
                    fieldName = NamePair {
                      nameC = Name "sudoku",
                      nameHsIdent = Identifier
                        "example_sudoku"},
                    fieldComment = Nothing},
                  structFieldType = TypeConstArray
                    3
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral PrimInt Signed))),
                  structFieldOffset = 96,
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
          commentOrigin = Just "Example",
          commentLocation = Just
            "array.h:49:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Example",
          structConstr = Name
            "@NsConstr"
            "Example",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "example_triple",
              fieldType = HsConstArray
                3
                (HsPrimType HsPrimCInt),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "array.h:50:9",
                    fieldName = NamePair {
                      nameC = Name "triple",
                      nameHsIdent = Identifier
                        "example_triple"},
                    fieldComment = Nothing},
                  structFieldType = TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral PrimInt Signed)),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "triple",
                  commentLocation = Just
                    "array.h:50:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["array.h"],
                      headerInclude = "array.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "example_sudoku",
              fieldType = HsConstArray
                3
                (HsConstArray
                  3
                  (HsPrimType HsPrimCInt)),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "array.h:51:9",
                    fieldName = NamePair {
                      nameC = Name "sudoku",
                      nameHsIdent = Identifier
                        "example_sudoku"},
                    fieldComment = Nothing},
                  structFieldType = TypeConstArray
                    3
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral PrimInt Signed))),
                  structFieldOffset = 96,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "sudoku",
                  commentLocation = Just
                    "array.h:51:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["array.h"],
                      headerInclude = "array.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "array.h:49:8",
                declId = NamePair {
                  nameC = Name "Example",
                  nameHsIdent = Identifier
                    "Example"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["array.h"],
                    headerInclude = "array.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Example"),
                  structSizeof = 48,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "array.h:50:9",
                        fieldName = NamePair {
                          nameC = Name "triple",
                          nameHsIdent = Identifier
                            "example_triple"},
                        fieldComment = Nothing},
                      structFieldType = TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral PrimInt Signed)),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "array.h:51:9",
                        fieldName = NamePair {
                          nameC = Name "sudoku",
                          nameHsIdent = Identifier
                            "example_sudoku"},
                        fieldComment = Nothing},
                      structFieldType = TypeConstArray
                        3
                        (TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral PrimInt Signed))),
                      structFieldOffset = 96,
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
              commentOrigin = Just "Example",
              commentLocation = Just
                "array.h:49:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["array.h"],
                  headerInclude = "array.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 48,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Example",
                  structConstr = Name
                    "@NsConstr"
                    "Example",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "example_triple",
                      fieldType = HsConstArray
                        3
                        (HsPrimType HsPrimCInt),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "array.h:50:9",
                            fieldName = NamePair {
                              nameC = Name "triple",
                              nameHsIdent = Identifier
                                "example_triple"},
                            fieldComment = Nothing},
                          structFieldType = TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral PrimInt Signed)),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "triple",
                          commentLocation = Just
                            "array.h:50:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["array.h"],
                              headerInclude = "array.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "example_sudoku",
                      fieldType = HsConstArray
                        3
                        (HsConstArray
                          3
                          (HsPrimType HsPrimCInt)),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "array.h:51:9",
                            fieldName = NamePair {
                              nameC = Name "sudoku",
                              nameHsIdent = Identifier
                                "example_sudoku"},
                            fieldComment = Nothing},
                          structFieldType = TypeConstArray
                            3
                            (TypeConstArray
                              3
                              (TypePrim
                                (PrimIntegral PrimInt Signed))),
                          structFieldOffset = 96,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "sudoku",
                          commentLocation = Just
                            "array.h:51:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["array.h"],
                              headerInclude = "array.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "array.h:49:8",
                        declId = NamePair {
                          nameC = Name "Example",
                          nameHsIdent = Identifier
                            "Example"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["array.h"],
                            headerInclude = "array.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Example"),
                          structSizeof = 48,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "array.h:50:9",
                                fieldName = NamePair {
                                  nameC = Name "triple",
                                  nameHsIdent = Identifier
                                    "example_triple"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                3
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "array.h:51:9",
                                fieldName = NamePair {
                                  nameC = Name "sudoku",
                                  nameHsIdent = Identifier
                                    "example_sudoku"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                3
                                (TypeConstArray
                                  3
                                  (TypePrim
                                    (PrimIntegral PrimInt Signed))),
                              structFieldOffset = 96,
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
                      commentOrigin = Just "Example",
                      commentLocation = Just
                        "array.h:49:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["array.h"],
                          headerInclude = "array.h"},
                      commentChildren = []}})
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
                  structName = Name
                    "@NsTypeConstr"
                    "Example",
                  structConstr = Name
                    "@NsConstr"
                    "Example",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "example_triple",
                      fieldType = HsConstArray
                        3
                        (HsPrimType HsPrimCInt),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "array.h:50:9",
                            fieldName = NamePair {
                              nameC = Name "triple",
                              nameHsIdent = Identifier
                                "example_triple"},
                            fieldComment = Nothing},
                          structFieldType = TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral PrimInt Signed)),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "triple",
                          commentLocation = Just
                            "array.h:50:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["array.h"],
                              headerInclude = "array.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "example_sudoku",
                      fieldType = HsConstArray
                        3
                        (HsConstArray
                          3
                          (HsPrimType HsPrimCInt)),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "array.h:51:9",
                            fieldName = NamePair {
                              nameC = Name "sudoku",
                              nameHsIdent = Identifier
                                "example_sudoku"},
                            fieldComment = Nothing},
                          structFieldType = TypeConstArray
                            3
                            (TypeConstArray
                              3
                              (TypePrim
                                (PrimIntegral PrimInt Signed))),
                          structFieldOffset = 96,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "sudoku",
                          commentLocation = Just
                            "array.h:51:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["array.h"],
                              headerInclude = "array.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "array.h:49:8",
                        declId = NamePair {
                          nameC = Name "Example",
                          nameHsIdent = Identifier
                            "Example"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["array.h"],
                            headerInclude = "array.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Example"),
                          structSizeof = 48,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "array.h:50:9",
                                fieldName = NamePair {
                                  nameC = Name "triple",
                                  nameHsIdent = Identifier
                                    "example_triple"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                3
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "array.h:51:9",
                                fieldName = NamePair {
                                  nameC = Name "sudoku",
                                  nameHsIdent = Identifier
                                    "example_sudoku"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                3
                                (TypeConstArray
                                  3
                                  (TypePrim
                                    (PrimIntegral PrimInt Signed))),
                              structFieldOffset = 96,
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
                      commentOrigin = Just "Example",
                      commentLocation = Just
                        "array.h:49:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["array.h"],
                          headerInclude = "array.h"},
                      commentChildren = []}}
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
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Example",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Example",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Sudoku",
      newtypeConstr = Name
        "@NsConstr"
        "Sudoku",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Sudoku",
        fieldType = HsConstArray
          3
          (HsTypRef
            (Name
              "@NsTypeConstr"
              "Triplet")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "array.h:55:17",
          declId = NamePair {
            nameC = Name "sudoku",
            nameHsIdent = Identifier
              "Sudoku"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          declComment = Just
            (Comment
              [
                Paragraph
                  [
                    TextContent
                      "Typedef-in-typedef"]])},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Sudoku",
              newtypeField = Name
                "@NsVar"
                "un_Sudoku"},
            typedefType = TypeConstArray
              3
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "triplet",
                    nameHsIdent = Identifier
                      "Triplet"}
                  (TypeConstArray
                    3
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
        [Eq, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Typedef-in-typedef"],
          commentOrigin = Just "sudoku",
          commentLocation = Just
            "array.h:55:17",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Sudoku",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Sudoku",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Sudoku",
      deriveInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_1",
      foreignImportParameters = [
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
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xs",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_5d1be223fd040c3b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_5d1be223fd040c3b (signed int arg1, signed int *arg2) { return fun_1(arg1, arg2); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
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
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
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
                "Array of known size"],
          commentOrigin = Just "fun_1",
          commentLocation = Just
            "array.h:118:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xs",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_cabe35537b18e986",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_cabe35537b18e986 (signed int *arg1) { return fun_2(arg1); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "triplet",
                    nameHsIdent = Identifier
                      "Triplet"}
                  (TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))],
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
          commentOrigin = Just "fun_2",
          commentLocation = Just
            "array.h:121:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xs",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_4cdbf10236e78984",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_4cdbf10236e78984 (signed int *arg1) { return fun_3(arg1); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
              (TypeIncompleteArray
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
                "Array of unknown size"],
          commentOrigin = Just "fun_3",
          commentLocation = Just
            "array.h:124:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_4",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xs",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_e356c5ddb2608063",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_e356c5ddb2608063 (signed int *arg1) { return fun_4(arg1); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "list",
                    nameHsIdent = Identifier "List"}
                  (TypeIncompleteArray
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))],
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
          commentOrigin = Just "fun_4",
          commentLocation = Just
            "array.h:127:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_5",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_f5ccf2c8d2e60be5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_f5ccf2c8d2e60be5 (signed int (*arg1)[3]) { return fun_5(arg1); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeConstArray
                4
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
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
          commentOrigin = Just "fun_5",
          commentLocation = Just
            "array.h:130:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_6",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_2b3a983697999524",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_2b3a983697999524 (signed int (*arg1)[3]) { return fun_6(arg1); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "matrix",
                    nameHsIdent = Identifier
                      "Matrix"}
                  (TypeConstArray
                    4
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed))))))],
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
          commentOrigin = Just "fun_6",
          commentLocation = Just
            "array.h:133:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_7",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_72e9371a1b8b8907",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_72e9371a1b8b8907 (signed int (*arg1)[3]) { return fun_7(arg1); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeIncompleteArray
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
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
          commentOrigin = Just "fun_7",
          commentLocation = Just
            "array.h:136:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_8",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_62ad87463d9a75de",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_62ad87463d9a75de (signed int (*arg1)[3]) { return fun_8(arg1); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "tripletlist",
                    nameHsIdent = Identifier
                      "Tripletlist"}
                  (TypeIncompleteArray
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed))))))],
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
          commentOrigin = Just "fun_8",
          commentLocation = Just
            "array.h:139:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "isSolved",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_2280ecc4c152a73f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_2280ecc4c152a73f (triplet *arg1) { return isSolved(arg1); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "sudoku",
                    nameHsIdent = Identifier
                      "Sudoku"}
                  (TypeConstArray
                    3
                    (TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "triplet",
                          nameHsIdent = Identifier
                            "Triplet"}
                        (TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral
                              PrimInt
                              Signed))))))))],
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
                "Typedef-in-typedef"],
          commentOrigin = Just "isSolved",
          commentLocation = Just
            "array.h:142:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_1_const_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "ys"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_f1d120f83dc61db5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_f1d120f83dc61db5 (signed int arg1, signed int *arg2, signed int const *arg3) { return fun_1_const(arg1, arg2, arg3); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
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
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
              (TypeConstArray
                3
                (TypePrim
                  (PrimIntegral PrimInt Signed))),
            _×_
              (Just
                NamePair {
                  nameC = Name "ys",
                  nameHsIdent = Identifier "ys"})
              (TypeConstArray
                3
                (TypeQualified
                  TypeQualifierConst
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
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
                "Pointer-based API for",
              Identifier "fun_1_const"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "fun_1_const",
      functionDeclParameters = [
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
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xs",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "ys"),
          functionParameterType =
          HsConstArray
            3
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "ys",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimCInt),
      functionDeclBody =
      `ELam "x" (ELam "x" (ELam "x" (EApp (EApp (EGlobal ConstantArray_withPtr) (EBound 0)) (ELam "ptr" (EApp (EApp (EApp (EFree "fun_1_const_wrapper") (EBound 3)) (EBound 2)) (EBound 0))))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
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
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
              (TypeConstArray
                3
                (TypePrim
                  (PrimIntegral PrimInt Signed))),
            _×_
              (Just
                NamePair {
                  nameC = Name "ys",
                  nameHsIdent = Identifier "ys"})
              (TypeConstArray
                3
                (TypeQualified
                  TypeQualifierConst
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      functionDeclComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of known size"],
          commentOrigin = Just
            "fun_1_const",
          commentLocation = Just
            "array.h:149:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_2_const_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "ys"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_f15760e6f3596189",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_f15760e6f3596189 (signed int *arg1, signed int *arg2) { return fun_2_const(arg1, arg2); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "triplet",
                    nameHsIdent = Identifier
                      "Triplet"}
                  (TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "ys",
                  nameHsIdent = Identifier "ys"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "triplet",
                      nameHsIdent = Identifier
                        "Triplet"}
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed))))))],
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
                "Pointer-based API for",
              Identifier "fun_2_const"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "fun_2_const",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xs",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "ys"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Triplet"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "ys",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimCInt),
      functionDeclBody =
      `ELam "x" (ELam "x" (EApp (EApp (EGlobal ConstantArray_withPtr) (EBound 0)) (ELam "ptr" (EApp (EApp (EFree "fun_2_const_wrapper") (EBound 2)) (EBound 0)))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "triplet",
                    nameHsIdent = Identifier
                      "Triplet"}
                  (TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "ys",
                  nameHsIdent = Identifier "ys"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "triplet",
                      nameHsIdent = Identifier
                        "Triplet"}
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      functionDeclComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of known size, typedef"],
          commentOrigin = Just
            "fun_2_const",
          commentLocation = Just
            "array.h:152:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_3_const_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "ys"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_0ad99f041fc4f5ca",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_0ad99f041fc4f5ca (signed int *arg1, signed int const *arg2) { return fun_3_const(arg1, arg2); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
              (TypeIncompleteArray
                (TypePrim
                  (PrimIntegral PrimInt Signed))),
            _×_
              (Just
                NamePair {
                  nameC = Name "ys",
                  nameHsIdent = Identifier "ys"})
              (TypeIncompleteArray
                (TypeQualified
                  TypeQualifierConst
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
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
                "Pointer-based API for",
              Identifier "fun_3_const"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "fun_3_const",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xs",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "ys"),
          functionParameterType =
          HsIncompleteArray
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "ys",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimCInt),
      functionDeclBody =
      `ELam "x" (ELam "x" (EApp (EApp (EGlobal IncompleteArray_withPtr) (EBound 0)) (ELam "ptr" (EApp (EApp (EFree "fun_3_const_wrapper") (EBound 2)) (EBound 0)))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
              (TypeIncompleteArray
                (TypePrim
                  (PrimIntegral PrimInt Signed))),
            _×_
              (Just
                NamePair {
                  nameC = Name "ys",
                  nameHsIdent = Identifier "ys"})
              (TypeIncompleteArray
                (TypeQualified
                  TypeQualifierConst
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      functionDeclComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of unknown size"],
          commentOrigin = Just
            "fun_3_const",
          commentLocation = Just
            "array.h:155:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_4_const_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "ys"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_d61f2b8777e6ca19",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_d61f2b8777e6ca19 (signed int *arg1, signed int *arg2) { return fun_4_const(arg1, arg2); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "list",
                    nameHsIdent = Identifier "List"}
                  (TypeIncompleteArray
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "ys",
                  nameHsIdent = Identifier "ys"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "list",
                      nameHsIdent = Identifier "List"}
                    (TypeIncompleteArray
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed))))))],
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
                "Pointer-based API for",
              Identifier "fun_4_const"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "fun_4_const",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xs",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "ys"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "List"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "ys",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimCInt),
      functionDeclBody =
      `ELam "x" (ELam "x" (EApp (EApp (EGlobal IncompleteArray_withPtr) (EBound 0)) (ELam "ptr" (EApp (EApp (EFree "fun_4_const_wrapper") (EBound 2)) (EBound 0)))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "list",
                    nameHsIdent = Identifier "List"}
                  (TypeIncompleteArray
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "ys",
                  nameHsIdent = Identifier "ys"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "list",
                      nameHsIdent = Identifier "List"}
                    (TypeIncompleteArray
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      functionDeclComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of unknown size, typedef"],
          commentOrigin = Just
            "fun_4_const",
          commentLocation = Just
            "array.h:158:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_5_const_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "yss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_9e1f66e6a0369c45",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_9e1f66e6a0369c45 (signed int (*arg1)[3], signed int const (*arg2)[3]) { return fun_5_const(arg1, arg2); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeConstArray
                4
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))),
            _×_
              (Just
                NamePair {
                  nameC = Name "yss",
                  nameHsIdent = Identifier "yss"})
              (TypeConstArray
                4
                (TypeConstArray
                  3
                  (TypeQualified
                    TypeQualifierConst
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))],
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
                "Pointer-based API for",
              Identifier "fun_5_const"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "fun_5_const",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "yss"),
          functionParameterType =
          HsConstArray
            4
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "yss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimCInt),
      functionDeclBody =
      `ELam "x" (ELam "x" (EApp (EApp (EGlobal ConstantArray_withPtr) (EBound 0)) (ELam "ptr" (EApp (EApp (EFree "fun_5_const_wrapper") (EBound 2)) (EBound 0)))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeConstArray
                4
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))),
            _×_
              (Just
                NamePair {
                  nameC = Name "yss",
                  nameHsIdent = Identifier "yss"})
              (TypeConstArray
                4
                (TypeConstArray
                  3
                  (TypeQualified
                    TypeQualifierConst
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      functionDeclComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of known size"],
          commentOrigin = Just
            "fun_5_const",
          commentLocation = Just
            "array.h:161:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_6_const_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "yss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_5b4bd3c6cee83e61",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_5b4bd3c6cee83e61 (signed int (*arg1)[3], signed int (*arg2)[3]) { return fun_6_const(arg1, arg2); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "matrix",
                    nameHsIdent = Identifier
                      "Matrix"}
                  (TypeConstArray
                    4
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed)))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "yss",
                  nameHsIdent = Identifier "yss"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "matrix",
                      nameHsIdent = Identifier
                        "Matrix"}
                    (TypeConstArray
                      4
                      (TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral
                            PrimInt
                            Signed)))))))],
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
                "Pointer-based API for",
              Identifier "fun_6_const"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "fun_6_const",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "yss"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Matrix"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "yss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimCInt),
      functionDeclBody =
      `ELam "x" (ELam "x" (EApp (EApp (EGlobal ConstantArray_withPtr) (EBound 0)) (ELam "ptr" (EApp (EApp (EFree "fun_6_const_wrapper") (EBound 2)) (EBound 0)))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "matrix",
                    nameHsIdent = Identifier
                      "Matrix"}
                  (TypeConstArray
                    4
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed)))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "yss",
                  nameHsIdent = Identifier "yss"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "matrix",
                      nameHsIdent = Identifier
                        "Matrix"}
                    (TypeConstArray
                      4
                      (TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral
                            PrimInt
                            Signed)))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      functionDeclComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of known size, typedef"],
          commentOrigin = Just
            "fun_6_const",
          commentLocation = Just
            "array.h:164:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_7_const_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "yss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_b551069ce9e1f12e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_b551069ce9e1f12e (signed int (*arg1)[3], signed int const (*arg2)[3]) { return fun_7_const(arg1, arg2); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeIncompleteArray
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))),
            _×_
              (Just
                NamePair {
                  nameC = Name "yss",
                  nameHsIdent = Identifier "yss"})
              (TypeIncompleteArray
                (TypeConstArray
                  3
                  (TypeQualified
                    TypeQualifierConst
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))],
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
                "Pointer-based API for",
              Identifier "fun_7_const"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "fun_7_const",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "yss"),
          functionParameterType =
          HsIncompleteArray
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "yss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimCInt),
      functionDeclBody =
      `ELam "x" (ELam "x" (EApp (EApp (EGlobal IncompleteArray_withPtr) (EBound 0)) (ELam "ptr" (EApp (EApp (EFree "fun_7_const_wrapper") (EBound 2)) (EBound 0)))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeIncompleteArray
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))),
            _×_
              (Just
                NamePair {
                  nameC = Name "yss",
                  nameHsIdent = Identifier "yss"})
              (TypeIncompleteArray
                (TypeConstArray
                  3
                  (TypeQualified
                    TypeQualifierConst
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      functionDeclComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of unknown size"],
          commentOrigin = Just
            "fun_7_const",
          commentLocation = Just
            "array.h:167:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_8_const_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "yss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_4ac495707a95aa13",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_4ac495707a95aa13 (signed int (*arg1)[3], signed int (*arg2)[3]) { return fun_8_const(arg1, arg2); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "tripletlist",
                    nameHsIdent = Identifier
                      "Tripletlist"}
                  (TypeIncompleteArray
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed)))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "yss",
                  nameHsIdent = Identifier "yss"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "tripletlist",
                      nameHsIdent = Identifier
                        "Tripletlist"}
                    (TypeIncompleteArray
                      (TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral
                            PrimInt
                            Signed)))))))],
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
                "Pointer-based API for",
              Identifier "fun_8_const"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "fun_8_const",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "yss"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Tripletlist"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "yss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimCInt),
      functionDeclBody =
      `ELam "x" (ELam "x" (EApp (EApp (EGlobal IncompleteArray_withPtr) (EBound 0)) (ELam "ptr" (EApp (EApp (EFree "fun_8_const_wrapper") (EBound 2)) (EBound 0)))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "tripletlist",
                    nameHsIdent = Identifier
                      "Tripletlist"}
                  (TypeIncompleteArray
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed)))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "yss",
                  nameHsIdent = Identifier "yss"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "tripletlist",
                      nameHsIdent = Identifier
                        "Tripletlist"}
                    (TypeIncompleteArray
                      (TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral
                            PrimInt
                            Signed)))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      functionDeclComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of unknown size, typedef"],
          commentOrigin = Just
            "fun_8_const",
          commentLocation = Just
            "array.h:170:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "isSolved_const_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "yss"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_1bdcfcd7aca9a2f6",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_1bdcfcd7aca9a2f6 (triplet *arg1, triplet *arg2) { return isSolved_const(arg1, arg2); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "sudoku",
                    nameHsIdent = Identifier
                      "Sudoku"}
                  (TypeConstArray
                    3
                    (TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "triplet",
                          nameHsIdent = Identifier
                            "Triplet"}
                        (TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral
                              PrimInt
                              Signed)))))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "yss",
                  nameHsIdent = Identifier "yss"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "sudoku",
                      nameHsIdent = Identifier
                        "Sudoku"}
                    (TypeConstArray
                      3
                      (TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "triplet",
                            nameHsIdent = Identifier
                              "Triplet"}
                          (TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral
                                PrimInt
                                Signed)))))))))],
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
                "Pointer-based API for",
              Identifier "isSolved_const"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "isSolved_const",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "yss"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Sudoku"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "yss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimCInt),
      functionDeclBody =
      `ELam "x" (ELam "x" (EApp (EApp (EGlobal ConstantArray_withPtr) (EBound 0)) (ELam "ptr" (EApp (EApp (EFree "isSolved_const_wrapper") (EBound 2)) (EBound 0)))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "sudoku",
                    nameHsIdent = Identifier
                      "Sudoku"}
                  (TypeConstArray
                    3
                    (TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "triplet",
                          nameHsIdent = Identifier
                            "Triplet"}
                        (TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral
                              PrimInt
                              Signed)))))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "yss",
                  nameHsIdent = Identifier "yss"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "sudoku",
                      nameHsIdent = Identifier
                        "Sudoku"}
                    (TypeConstArray
                      3
                      (TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "triplet",
                            nameHsIdent = Identifier
                              "Triplet"}
                          (TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral
                                PrimInt
                                Signed)))))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      functionDeclComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Typedef-in-typedef"],
          commentOrigin = Just
            "isSolved_const",
          commentLocation = Just
            "array.h:173:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_9",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_array_d4c729a69c884fd4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int (*hs_bindgen_test_array_d4c729a69c884fd4 (void))[3] { return fun_9(); }",
          capiWrapperImport = "array.h"},
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
          commentOrigin = Just "fun_9",
          commentLocation = Just
            "array.h:185:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_10",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")))),
      foreignImportOrigName =
      "hs_bindgen_test_array_bb92dfded907271e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "triplet *hs_bindgen_test_array_bb92dfded907271e (void) { return fun_10(); }",
          capiWrapperImport = "array.h"},
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
                  nameHsIdent = Identifier
                    "Triplet"}
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of known size, typedef"],
          commentOrigin = Just "fun_10",
          commentLocation = Just
            "array.h:188:10",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_11",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsIncompleteArray
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_array_489aaaa59e992ddf",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int (*hs_bindgen_test_array_489aaaa59e992ddf (void))[] { return fun_11(); }",
          capiWrapperImport = "array.h"},
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
          commentOrigin = Just "fun_11",
          commentLocation = Just
            "array.h:191:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_12",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "List")))),
      foreignImportOrigName =
      "hs_bindgen_test_array_ee94c35f987d6c50",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "list *hs_bindgen_test_array_ee94c35f987d6c50 (void) { return fun_12(); }",
          capiWrapperImport = "array.h"},
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
                  nameHsIdent = Identifier "List"}
                (TypeIncompleteArray
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of unknown size, typedef"],
          commentOrigin = Just "fun_12",
          commentLocation = Just
            "array.h:194:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_13",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              4
              (HsConstArray
                3
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_ca2c7b60ce85a964",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int (*hs_bindgen_test_array_ca2c7b60ce85a964 (void))[4][3] { return fun_13(); }",
          capiWrapperImport = "array.h"},
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
          commentOrigin = Just "fun_13",
          commentLocation = Just
            "array.h:197:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_14",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Matrix")))),
      foreignImportOrigName =
      "hs_bindgen_test_array_ab2c533efdae8e41",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "matrix *hs_bindgen_test_array_ab2c533efdae8e41 (void) { return fun_14(); }",
          capiWrapperImport = "array.h"},
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
                  nameHsIdent = Identifier
                    "Matrix"}
                (TypeConstArray
                  4
                  (TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of known size, typedef"],
          commentOrigin = Just "fun_14",
          commentLocation = Just
            "array.h:200:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_15",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsIncompleteArray
              (HsConstArray
                3
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_019bdeb5db79cee1",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int (*hs_bindgen_test_array_019bdeb5db79cee1 (void))[][3] { return fun_15(); }",
          capiWrapperImport = "array.h"},
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
          commentOrigin = Just "fun_15",
          commentLocation = Just
            "array.h:203:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_16",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Tripletlist")))),
      foreignImportOrigName =
      "hs_bindgen_test_array_ca0e7c51654fef12",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "tripletlist *hs_bindgen_test_array_ca0e7c51654fef12 (void) { return fun_16(); }",
          capiWrapperImport = "array.h"},
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
                  nameHsIdent = Identifier
                    "Tripletlist"}
                (TypeIncompleteArray
                  (TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of unknown size, typedef"],
          commentOrigin = Just "fun_16",
          commentLocation = Just
            "array.h:206:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "solve",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Sudoku")))),
      foreignImportOrigName =
      "hs_bindgen_test_array_f6b66497ee1685b0",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "sudoku *hs_bindgen_test_array_f6b66497ee1685b0 (void) { return solve(); }",
          capiWrapperImport = "array.h"},
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
                  nameC = Name "sudoku",
                  nameHsIdent = Identifier
                    "Sudoku"}
                (TypeConstArray
                  3
                  (TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "triplet",
                        nameHsIdent = Identifier
                          "Triplet"}
                      (TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral
                            PrimInt
                            Signed))))))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Typedef-in-typedef"],
          commentOrigin = Just "solve",
          commentLocation = Just
            "array.h:209:10",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_1",
      foreignImportParameters = [
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
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xs",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_38d1e706888c6509",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_38d1e706888c6509 (signed int arg1, signed int *arg2) { return fun_1(arg1, arg2); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
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
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
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
                "Array of known size"],
          commentOrigin = Just "fun_1",
          commentLocation = Just
            "array.h:118:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xs",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_39ee469929b167e2",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_39ee469929b167e2 (signed int *arg1) { return fun_2(arg1); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "triplet",
                    nameHsIdent = Identifier
                      "Triplet"}
                  (TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))],
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
          commentOrigin = Just "fun_2",
          commentLocation = Just
            "array.h:121:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xs",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_2aa49d73d177f65b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_2aa49d73d177f65b (signed int *arg1) { return fun_3(arg1); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
              (TypeIncompleteArray
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
                "Array of unknown size"],
          commentOrigin = Just "fun_3",
          commentLocation = Just
            "array.h:124:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_4",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xs",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_c3b2941d43616704",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_c3b2941d43616704 (signed int *arg1) { return fun_4(arg1); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "list",
                    nameHsIdent = Identifier "List"}
                  (TypeIncompleteArray
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))],
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
          commentOrigin = Just "fun_4",
          commentLocation = Just
            "array.h:127:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_5",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_69ec2f59c3c40de4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_69ec2f59c3c40de4 (signed int (*arg1)[3]) { return fun_5(arg1); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeConstArray
                4
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
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
          commentOrigin = Just "fun_5",
          commentLocation = Just
            "array.h:130:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_6",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_a4600c666e12a07a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_a4600c666e12a07a (signed int (*arg1)[3]) { return fun_6(arg1); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "matrix",
                    nameHsIdent = Identifier
                      "Matrix"}
                  (TypeConstArray
                    4
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed))))))],
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
          commentOrigin = Just "fun_6",
          commentLocation = Just
            "array.h:133:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_7",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_b903c9d5ebf4f21f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_b903c9d5ebf4f21f (signed int (*arg1)[3]) { return fun_7(arg1); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeIncompleteArray
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
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
          commentOrigin = Just "fun_7",
          commentLocation = Just
            "array.h:136:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_8",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_88af789e5a205473",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_88af789e5a205473 (signed int (*arg1)[3]) { return fun_8(arg1); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "tripletlist",
                    nameHsIdent = Identifier
                      "Tripletlist"}
                  (TypeIncompleteArray
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed))))))],
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
          commentOrigin = Just "fun_8",
          commentLocation = Just
            "array.h:139:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "isSolved",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_617bd1cd5514ea45",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_617bd1cd5514ea45 (triplet *arg1) { return isSolved(arg1); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "sudoku",
                    nameHsIdent = Identifier
                      "Sudoku"}
                  (TypeConstArray
                    3
                    (TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "triplet",
                          nameHsIdent = Identifier
                            "Triplet"}
                        (TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral
                              PrimInt
                              Signed))))))))],
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
                "Typedef-in-typedef"],
          commentOrigin = Just "isSolved",
          commentLocation = Just
            "array.h:142:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_1_const_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "ys"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_e4b00d6936127c9c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_e4b00d6936127c9c (signed int arg1, signed int *arg2, signed int const *arg3) { return fun_1_const(arg1, arg2, arg3); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
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
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
              (TypeConstArray
                3
                (TypePrim
                  (PrimIntegral PrimInt Signed))),
            _×_
              (Just
                NamePair {
                  nameC = Name "ys",
                  nameHsIdent = Identifier "ys"})
              (TypeConstArray
                3
                (TypeQualified
                  TypeQualifierConst
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
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
                "Pointer-based API for",
              Identifier "fun_1_const"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "fun_1_const",
      functionDeclParameters = [
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
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xs",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "ys"),
          functionParameterType =
          HsConstArray
            3
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "ys",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimCInt),
      functionDeclBody =
      `ELam "x" (ELam "x" (ELam "x" (EApp (EApp (EGlobal ConstantArray_withPtr) (EBound 0)) (ELam "ptr" (EApp (EApp (EApp (EFree "fun_1_const_wrapper") (EBound 3)) (EBound 2)) (EBound 0))))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
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
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
              (TypeConstArray
                3
                (TypePrim
                  (PrimIntegral PrimInt Signed))),
            _×_
              (Just
                NamePair {
                  nameC = Name "ys",
                  nameHsIdent = Identifier "ys"})
              (TypeConstArray
                3
                (TypeQualified
                  TypeQualifierConst
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      functionDeclComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of known size"],
          commentOrigin = Just
            "fun_1_const",
          commentLocation = Just
            "array.h:149:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_2_const_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "ys"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_5fe603fc3c41a066",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_5fe603fc3c41a066 (signed int *arg1, signed int *arg2) { return fun_2_const(arg1, arg2); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "triplet",
                    nameHsIdent = Identifier
                      "Triplet"}
                  (TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "ys",
                  nameHsIdent = Identifier "ys"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "triplet",
                      nameHsIdent = Identifier
                        "Triplet"}
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed))))))],
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
                "Pointer-based API for",
              Identifier "fun_2_const"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "fun_2_const",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xs",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "ys"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Triplet"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "ys",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimCInt),
      functionDeclBody =
      `ELam "x" (ELam "x" (EApp (EApp (EGlobal ConstantArray_withPtr) (EBound 0)) (ELam "ptr" (EApp (EApp (EFree "fun_2_const_wrapper") (EBound 2)) (EBound 0)))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "triplet",
                    nameHsIdent = Identifier
                      "Triplet"}
                  (TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "ys",
                  nameHsIdent = Identifier "ys"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "triplet",
                      nameHsIdent = Identifier
                        "Triplet"}
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      functionDeclComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of known size, typedef"],
          commentOrigin = Just
            "fun_2_const",
          commentLocation = Just
            "array.h:152:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_3_const_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "ys"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_d7b0d574cbe650f8",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_d7b0d574cbe650f8 (signed int *arg1, signed int const *arg2) { return fun_3_const(arg1, arg2); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
              (TypeIncompleteArray
                (TypePrim
                  (PrimIntegral PrimInt Signed))),
            _×_
              (Just
                NamePair {
                  nameC = Name "ys",
                  nameHsIdent = Identifier "ys"})
              (TypeIncompleteArray
                (TypeQualified
                  TypeQualifierConst
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
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
                "Pointer-based API for",
              Identifier "fun_3_const"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "fun_3_const",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xs",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "ys"),
          functionParameterType =
          HsIncompleteArray
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "ys",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimCInt),
      functionDeclBody =
      `ELam "x" (ELam "x" (EApp (EApp (EGlobal IncompleteArray_withPtr) (EBound 0)) (ELam "ptr" (EApp (EApp (EFree "fun_3_const_wrapper") (EBound 2)) (EBound 0)))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
              (TypeIncompleteArray
                (TypePrim
                  (PrimIntegral PrimInt Signed))),
            _×_
              (Just
                NamePair {
                  nameC = Name "ys",
                  nameHsIdent = Identifier "ys"})
              (TypeIncompleteArray
                (TypeQualified
                  TypeQualifierConst
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      functionDeclComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of unknown size"],
          commentOrigin = Just
            "fun_3_const",
          commentLocation = Just
            "array.h:155:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_4_const_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "ys"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_a7499ca2f044e9ce",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_a7499ca2f044e9ce (signed int *arg1, signed int *arg2) { return fun_4_const(arg1, arg2); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "list",
                    nameHsIdent = Identifier "List"}
                  (TypeIncompleteArray
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "ys",
                  nameHsIdent = Identifier "ys"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "list",
                      nameHsIdent = Identifier "List"}
                    (TypeIncompleteArray
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed))))))],
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
                "Pointer-based API for",
              Identifier "fun_4_const"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "fun_4_const",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xs"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xs",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "ys"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "List"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "ys",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimCInt),
      functionDeclBody =
      `ELam "x" (ELam "x" (EApp (EApp (EGlobal IncompleteArray_withPtr) (EBound 0)) (ELam "ptr" (EApp (EApp (EFree "fun_4_const_wrapper") (EBound 2)) (EBound 0)))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xs",
                  nameHsIdent = Identifier "xs"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "list",
                    nameHsIdent = Identifier "List"}
                  (TypeIncompleteArray
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "ys",
                  nameHsIdent = Identifier "ys"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "list",
                      nameHsIdent = Identifier "List"}
                    (TypeIncompleteArray
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      functionDeclComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of unknown size, typedef"],
          commentOrigin = Just
            "fun_4_const",
          commentLocation = Just
            "array.h:158:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_5_const_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "yss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_24e12fc0372c2467",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_24e12fc0372c2467 (signed int (*arg1)[3], signed int const (*arg2)[3]) { return fun_5_const(arg1, arg2); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeConstArray
                4
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))),
            _×_
              (Just
                NamePair {
                  nameC = Name "yss",
                  nameHsIdent = Identifier "yss"})
              (TypeConstArray
                4
                (TypeConstArray
                  3
                  (TypeQualified
                    TypeQualifierConst
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))],
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
                "Pointer-based API for",
              Identifier "fun_5_const"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "fun_5_const",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "yss"),
          functionParameterType =
          HsConstArray
            4
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "yss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimCInt),
      functionDeclBody =
      `ELam "x" (ELam "x" (EApp (EApp (EGlobal ConstantArray_withPtr) (EBound 0)) (ELam "ptr" (EApp (EApp (EFree "fun_5_const_wrapper") (EBound 2)) (EBound 0)))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeConstArray
                4
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))),
            _×_
              (Just
                NamePair {
                  nameC = Name "yss",
                  nameHsIdent = Identifier "yss"})
              (TypeConstArray
                4
                (TypeConstArray
                  3
                  (TypeQualified
                    TypeQualifierConst
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      functionDeclComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of known size"],
          commentOrigin = Just
            "fun_5_const",
          commentLocation = Just
            "array.h:161:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_6_const_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "yss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_870ee33752c078df",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_870ee33752c078df (signed int (*arg1)[3], signed int (*arg2)[3]) { return fun_6_const(arg1, arg2); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "matrix",
                    nameHsIdent = Identifier
                      "Matrix"}
                  (TypeConstArray
                    4
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed)))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "yss",
                  nameHsIdent = Identifier "yss"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "matrix",
                      nameHsIdent = Identifier
                        "Matrix"}
                    (TypeConstArray
                      4
                      (TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral
                            PrimInt
                            Signed)))))))],
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
                "Pointer-based API for",
              Identifier "fun_6_const"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "fun_6_const",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "yss"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Matrix"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "yss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimCInt),
      functionDeclBody =
      `ELam "x" (ELam "x" (EApp (EApp (EGlobal ConstantArray_withPtr) (EBound 0)) (ELam "ptr" (EApp (EApp (EFree "fun_6_const_wrapper") (EBound 2)) (EBound 0)))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "matrix",
                    nameHsIdent = Identifier
                      "Matrix"}
                  (TypeConstArray
                    4
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed)))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "yss",
                  nameHsIdent = Identifier "yss"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "matrix",
                      nameHsIdent = Identifier
                        "Matrix"}
                    (TypeConstArray
                      4
                      (TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral
                            PrimInt
                            Signed)))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      functionDeclComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of known size, typedef"],
          commentOrigin = Just
            "fun_6_const",
          commentLocation = Just
            "array.h:164:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_7_const_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "yss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_18aa0941d0646906",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_18aa0941d0646906 (signed int (*arg1)[3], signed int const (*arg2)[3]) { return fun_7_const(arg1, arg2); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeIncompleteArray
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))),
            _×_
              (Just
                NamePair {
                  nameC = Name "yss",
                  nameHsIdent = Identifier "yss"})
              (TypeIncompleteArray
                (TypeConstArray
                  3
                  (TypeQualified
                    TypeQualifierConst
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))],
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
                "Pointer-based API for",
              Identifier "fun_7_const"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "fun_7_const",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "yss"),
          functionParameterType =
          HsIncompleteArray
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "yss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimCInt),
      functionDeclBody =
      `ELam "x" (ELam "x" (EApp (EApp (EGlobal IncompleteArray_withPtr) (EBound 0)) (ELam "ptr" (EApp (EApp (EFree "fun_7_const_wrapper") (EBound 2)) (EBound 0)))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeIncompleteArray
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))),
            _×_
              (Just
                NamePair {
                  nameC = Name "yss",
                  nameHsIdent = Identifier "yss"})
              (TypeIncompleteArray
                (TypeConstArray
                  3
                  (TypeQualified
                    TypeQualifierConst
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      functionDeclComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of unknown size"],
          commentOrigin = Just
            "fun_7_const",
          commentLocation = Just
            "array.h:167:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_8_const_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "yss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_94591138e958ffe1",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_94591138e958ffe1 (signed int (*arg1)[3], signed int (*arg2)[3]) { return fun_8_const(arg1, arg2); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "tripletlist",
                    nameHsIdent = Identifier
                      "Tripletlist"}
                  (TypeIncompleteArray
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed)))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "yss",
                  nameHsIdent = Identifier "yss"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "tripletlist",
                      nameHsIdent = Identifier
                        "Tripletlist"}
                    (TypeIncompleteArray
                      (TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral
                            PrimInt
                            Signed)))))))],
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
                "Pointer-based API for",
              Identifier "fun_8_const"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "fun_8_const",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "yss"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Tripletlist"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "yss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimCInt),
      functionDeclBody =
      `ELam "x" (ELam "x" (EApp (EApp (EGlobal IncompleteArray_withPtr) (EBound 0)) (ELam "ptr" (EApp (EApp (EFree "fun_8_const_wrapper") (EBound 2)) (EBound 0)))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "tripletlist",
                    nameHsIdent = Identifier
                      "Tripletlist"}
                  (TypeIncompleteArray
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed)))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "yss",
                  nameHsIdent = Identifier "yss"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "tripletlist",
                      nameHsIdent = Identifier
                        "Tripletlist"}
                    (TypeIncompleteArray
                      (TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral
                            PrimInt
                            Signed)))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      functionDeclComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of unknown size, typedef"],
          commentOrigin = Just
            "fun_8_const",
          commentLocation = Just
            "array.h:170:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "isSolved_const_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "yss"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_array_ec99b7cbbed57b25",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_array_ec99b7cbbed57b25 (triplet *arg1, triplet *arg2) { return isSolved_const(arg1, arg2); }",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "sudoku",
                    nameHsIdent = Identifier
                      "Sudoku"}
                  (TypeConstArray
                    3
                    (TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "triplet",
                          nameHsIdent = Identifier
                            "Triplet"}
                        (TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral
                              PrimInt
                              Signed)))))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "yss",
                  nameHsIdent = Identifier "yss"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "sudoku",
                      nameHsIdent = Identifier
                        "Sudoku"}
                    (TypeConstArray
                      3
                      (TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "triplet",
                            nameHsIdent = Identifier
                              "Triplet"}
                          (TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral
                                PrimInt
                                Signed)))))))))],
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
                "Pointer-based API for",
              Identifier "isSolved_const"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "isSolved_const",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "xss"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "xss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "yss"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Sudoku"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "yss",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimCInt),
      functionDeclBody =
      `ELam "x" (ELam "x" (EApp (EApp (EGlobal ConstantArray_withPtr) (EBound 0)) (ELam "ptr" (EApp (EApp (EFree "isSolved_const_wrapper") (EBound 2)) (EBound 0)))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "xss",
                  nameHsIdent = Identifier "xss"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "sudoku",
                    nameHsIdent = Identifier
                      "Sudoku"}
                  (TypeConstArray
                    3
                    (TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "triplet",
                          nameHsIdent = Identifier
                            "Triplet"}
                        (TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral
                              PrimInt
                              Signed)))))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "yss",
                  nameHsIdent = Identifier "yss"})
              (TypeQualified
                TypeQualifierConst
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "sudoku",
                      nameHsIdent = Identifier
                        "Sudoku"}
                    (TypeConstArray
                      3
                      (TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "triplet",
                            nameHsIdent = Identifier
                              "Triplet"}
                          (TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral
                                PrimInt
                                Signed)))))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      functionDeclComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Typedef-in-typedef"],
          commentOrigin = Just
            "isSolved_const",
          commentLocation = Just
            "array.h:173:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_9",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_array_49d4508b43473bd2",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int (*hs_bindgen_test_array_49d4508b43473bd2 (void))[3] { return fun_9(); }",
          capiWrapperImport = "array.h"},
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
          commentOrigin = Just "fun_9",
          commentLocation = Just
            "array.h:185:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_10",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")))),
      foreignImportOrigName =
      "hs_bindgen_test_array_d1763638472ee039",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "triplet *hs_bindgen_test_array_d1763638472ee039 (void) { return fun_10(); }",
          capiWrapperImport = "array.h"},
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
                  nameHsIdent = Identifier
                    "Triplet"}
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of known size, typedef"],
          commentOrigin = Just "fun_10",
          commentLocation = Just
            "array.h:188:10",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_11",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsIncompleteArray
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_array_293d2be6d282321b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int (*hs_bindgen_test_array_293d2be6d282321b (void))[] { return fun_11(); }",
          capiWrapperImport = "array.h"},
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
          commentOrigin = Just "fun_11",
          commentLocation = Just
            "array.h:191:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_12",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "List")))),
      foreignImportOrigName =
      "hs_bindgen_test_array_fe193d0e0c330960",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "list *hs_bindgen_test_array_fe193d0e0c330960 (void) { return fun_12(); }",
          capiWrapperImport = "array.h"},
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
                  nameHsIdent = Identifier "List"}
                (TypeIncompleteArray
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of unknown size, typedef"],
          commentOrigin = Just "fun_12",
          commentLocation = Just
            "array.h:194:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_13",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              4
              (HsConstArray
                3
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_f3df0067620bd691",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int (*hs_bindgen_test_array_f3df0067620bd691 (void))[4][3] { return fun_13(); }",
          capiWrapperImport = "array.h"},
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
          commentOrigin = Just "fun_13",
          commentLocation = Just
            "array.h:197:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_14",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Matrix")))),
      foreignImportOrigName =
      "hs_bindgen_test_array_9d75a740147af339",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "matrix *hs_bindgen_test_array_9d75a740147af339 (void) { return fun_14(); }",
          capiWrapperImport = "array.h"},
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
                  nameHsIdent = Identifier
                    "Matrix"}
                (TypeConstArray
                  4
                  (TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of known size, typedef"],
          commentOrigin = Just "fun_14",
          commentLocation = Just
            "array.h:200:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_15",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsIncompleteArray
              (HsConstArray
                3
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_d49e5e7f4ad3c830",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int (*hs_bindgen_test_array_d49e5e7f4ad3c830 (void))[][3] { return fun_15(); }",
          capiWrapperImport = "array.h"},
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
          commentOrigin = Just "fun_15",
          commentLocation = Just
            "array.h:203:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_16",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Tripletlist")))),
      foreignImportOrigName =
      "hs_bindgen_test_array_900726612f7787e4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "tripletlist *hs_bindgen_test_array_900726612f7787e4 (void) { return fun_16(); }",
          capiWrapperImport = "array.h"},
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
                  nameHsIdent = Identifier
                    "Tripletlist"}
                (TypeIncompleteArray
                  (TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of unknown size, typedef"],
          commentOrigin = Just "fun_16",
          commentLocation = Just
            "array.h:206:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "solve",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Sudoku")))),
      foreignImportOrigName =
      "hs_bindgen_test_array_ede6133d23ed3248",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "sudoku *hs_bindgen_test_array_ede6133d23ed3248 (void) { return solve(); }",
          capiWrapperImport = "array.h"},
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
                  nameC = Name "sudoku",
                  nameHsIdent = Identifier
                    "Sudoku"}
                (TypeConstArray
                  3
                  (TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "triplet",
                        nameHsIdent = Identifier
                          "Triplet"}
                      (TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral
                            PrimInt
                            Signed))))))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Typedef-in-typedef"],
          commentOrigin = Just "solve",
          commentLocation = Just
            "array.h:209:10",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_array_3ced2f3b2af806f8",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsFun
                (HsConstArray
                  3
                  (HsPrimType HsPrimCInt))
                (HsIO
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_3ced2f3b2af806f8",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_3ced2f3b2af806f8 (void)) (signed int arg1, signed int arg2[3]) { return &fun_1; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypeConstArray
              3
              (TypePrim
                (PrimIntegral PrimInt Signed))]
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
        "hs_bindgen_test_array_84966994a8d7df93",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Triplet"))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_84966994a8d7df93",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_2_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_84966994a8d7df93 (void)) (triplet arg1) { return &fun_2; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "triplet",
                  nameHsIdent = Identifier
                    "Triplet"}
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))]
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
        "hs_bindgen_test_array_3e6c940dbd7e5492",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsIncompleteArray
                (HsPrimType HsPrimCInt))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_3e6c940dbd7e5492",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_3_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_3e6c940dbd7e5492 (void)) (signed int arg1[]) { return &fun_3; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeIncompleteArray
              (TypePrim
                (PrimIntegral PrimInt Signed))]
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
        "hs_bindgen_test_array_d9f87d3e541b15e5",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "List"))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_d9f87d3e541b15e5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_4_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_d9f87d3e541b15e5 (void)) (list arg1) { return &fun_4; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "list",
                  nameHsIdent = Identifier "List"}
                (TypeIncompleteArray
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))]
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
        "hs_bindgen_test_array_cd41e41992d89300",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsConstArray
                4
                (HsConstArray
                  3
                  (HsPrimType HsPrimCInt)))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_cd41e41992d89300",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_5_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_cd41e41992d89300 (void)) (signed int arg1[4][3]) { return &fun_5; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeConstArray
              4
              (TypeConstArray
                3
                (TypePrim
                  (PrimIntegral PrimInt Signed)))]
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
        "hs_bindgen_test_array_db0e2655437ab8bb",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "Matrix"))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_db0e2655437ab8bb",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_6_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_db0e2655437ab8bb (void)) (matrix arg1) { return &fun_6; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "matrix",
                  nameHsIdent = Identifier
                    "Matrix"}
                (TypeConstArray
                  4
                  (TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))]
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
        "hs_bindgen_test_array_9ec02aa16b020aa0",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsIncompleteArray
                (HsConstArray
                  3
                  (HsPrimType HsPrimCInt)))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_9ec02aa16b020aa0",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_7_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_9ec02aa16b020aa0 (void)) (signed int arg1[][3]) { return &fun_7; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeIncompleteArray
              (TypeConstArray
                3
                (TypePrim
                  (PrimIntegral PrimInt Signed)))]
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
        "hs_bindgen_test_array_a41b8d1332b69b95",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Tripletlist"))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_a41b8d1332b69b95",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_8_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_a41b8d1332b69b95 (void)) (tripletlist arg1) { return &fun_8; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "tripletlist",
                  nameHsIdent = Identifier
                    "Tripletlist"}
                (TypeIncompleteArray
                  (TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))]
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
        "hs_bindgen_test_array_bdf2a6a8a3dd5b04",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "Sudoku"))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_bdf2a6a8a3dd5b04",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_isSolved_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_bdf2a6a8a3dd5b04 (void)) (sudoku arg1) { return &isSolved; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "sudoku",
                  nameHsIdent = Identifier
                    "Sudoku"}
                (TypeConstArray
                  3
                  (TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "triplet",
                        nameHsIdent = Identifier
                          "Triplet"}
                      (TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral
                            PrimInt
                            Signed)))))))]
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
        "hs_bindgen_test_array_a3de5f7e233ad0e1",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsFun
                (HsConstArray
                  3
                  (HsPrimType HsPrimCInt))
                (HsFun
                  (HsConstArray
                    3
                    (HsPrimType HsPrimCInt))
                  (HsIO
                    (HsPrimType HsPrimCInt))))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_a3de5f7e233ad0e1",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_1_const_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_a3de5f7e233ad0e1 (void)) (signed int arg1, signed int arg2[3], signed int const arg3[3]) { return &fun_1_const; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypeConstArray
              3
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            TypeConstArray
              3
              (TypeQualified
                TypeQualifierConst
                (TypePrim
                  (PrimIntegral PrimInt Signed)))]
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
        "hs_bindgen_test_array_3c09bbba7534ca1d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Triplet"))
              (HsFun
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Triplet"))
                (HsIO
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_3c09bbba7534ca1d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_2_const_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_3c09bbba7534ca1d (void)) (triplet arg1, triplet const arg2) { return &fun_2_const; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "triplet",
                  nameHsIdent = Identifier
                    "Triplet"}
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))),
            TypeQualified
              TypeQualifierConst
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "triplet",
                    nameHsIdent = Identifier
                      "Triplet"}
                  (TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))]
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
        "hs_bindgen_test_array_0e53ed28ec1ca276",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsIncompleteArray
                (HsPrimType HsPrimCInt))
              (HsFun
                (HsIncompleteArray
                  (HsPrimType HsPrimCInt))
                (HsIO
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_0e53ed28ec1ca276",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_3_const_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_0e53ed28ec1ca276 (void)) (signed int arg1[], signed int const arg2[]) { return &fun_3_const; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeIncompleteArray
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            TypeIncompleteArray
              (TypeQualified
                TypeQualifierConst
                (TypePrim
                  (PrimIntegral PrimInt Signed)))]
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
        "hs_bindgen_test_array_07d860d5e74c415b",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "List"))
              (HsFun
                (HsTypRef
                  (Name "@NsTypeConstr" "List"))
                (HsIO
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_07d860d5e74c415b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_4_const_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_07d860d5e74c415b (void)) (list arg1, list const arg2) { return &fun_4_const; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "list",
                  nameHsIdent = Identifier "List"}
                (TypeIncompleteArray
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))),
            TypeQualified
              TypeQualifierConst
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "list",
                    nameHsIdent = Identifier "List"}
                  (TypeIncompleteArray
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))]
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
        "hs_bindgen_test_array_3c0a139c24d7202a",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsConstArray
                4
                (HsConstArray
                  3
                  (HsPrimType HsPrimCInt)))
              (HsFun
                (HsConstArray
                  4
                  (HsConstArray
                    3
                    (HsPrimType HsPrimCInt)))
                (HsIO
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_3c0a139c24d7202a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_5_const_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_3c0a139c24d7202a (void)) (signed int arg1[4][3], signed int const arg2[4][3]) { return &fun_5_const; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeConstArray
              4
              (TypeConstArray
                3
                (TypePrim
                  (PrimIntegral PrimInt Signed))),
            TypeConstArray
              4
              (TypeConstArray
                3
                (TypeQualified
                  TypeQualifierConst
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))]
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
        "hs_bindgen_test_array_62d236581cc18366",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "Matrix"))
              (HsFun
                (HsTypRef
                  (Name "@NsTypeConstr" "Matrix"))
                (HsIO
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_62d236581cc18366",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_6_const_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_62d236581cc18366 (void)) (matrix arg1, matrix const arg2) { return &fun_6_const; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "matrix",
                  nameHsIdent = Identifier
                    "Matrix"}
                (TypeConstArray
                  4
                  (TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))),
            TypeQualified
              TypeQualifierConst
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "matrix",
                    nameHsIdent = Identifier
                      "Matrix"}
                  (TypeConstArray
                    4
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed))))))]
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
        "hs_bindgen_test_array_b4bf67c3cec12e54",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsIncompleteArray
                (HsConstArray
                  3
                  (HsPrimType HsPrimCInt)))
              (HsFun
                (HsIncompleteArray
                  (HsConstArray
                    3
                    (HsPrimType HsPrimCInt)))
                (HsIO
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_b4bf67c3cec12e54",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_7_const_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_b4bf67c3cec12e54 (void)) (signed int arg1[][3], signed int const arg2[][3]) { return &fun_7_const; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeIncompleteArray
              (TypeConstArray
                3
                (TypePrim
                  (PrimIntegral PrimInt Signed))),
            TypeIncompleteArray
              (TypeConstArray
                3
                (TypeQualified
                  TypeQualifierConst
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))]
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
        "hs_bindgen_test_array_99dd6a6017eb0eec",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Tripletlist"))
              (HsFun
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Tripletlist"))
                (HsIO
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_99dd6a6017eb0eec",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_8_const_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_99dd6a6017eb0eec (void)) (tripletlist arg1, tripletlist const arg2) { return &fun_8_const; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "tripletlist",
                  nameHsIdent = Identifier
                    "Tripletlist"}
                (TypeIncompleteArray
                  (TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))),
            TypeQualified
              TypeQualifierConst
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "tripletlist",
                    nameHsIdent = Identifier
                      "Tripletlist"}
                  (TypeIncompleteArray
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed))))))]
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
        "hs_bindgen_test_array_6deec046c95e4e0d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "Sudoku"))
              (HsFun
                (HsTypRef
                  (Name "@NsTypeConstr" "Sudoku"))
                (HsIO
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_6deec046c95e4e0d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_isSolved_const_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_6deec046c95e4e0d (void)) (sudoku arg1, sudoku const arg2) { return &isSolved_const; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "sudoku",
                  nameHsIdent = Identifier
                    "Sudoku"}
                (TypeConstArray
                  3
                  (TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "triplet",
                        nameHsIdent = Identifier
                          "Triplet"}
                      (TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral
                            PrimInt
                            Signed))))))),
            TypeQualified
              TypeQualifierConst
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "sudoku",
                    nameHsIdent = Identifier
                      "Sudoku"}
                  (TypeConstArray
                    3
                    (TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "triplet",
                          nameHsIdent = Identifier
                            "Triplet"}
                        (TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral
                              PrimInt
                              Signed))))))))]
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
        "hs_bindgen_test_array_76f53f330102e743",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPtr
                (HsConstArray
                  3
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_76f53f330102e743",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_9_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_array_76f53f330102e743 (void)) (void))[3] { return &fun_9; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
          (TypePointer
            (TypeConstArray
              3
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_array_abcc94f01de77b25",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Triplet")))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_abcc94f01de77b25",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_10_ptr */ __attribute__ ((const)) triplet *(*hs_bindgen_test_array_abcc94f01de77b25 (void)) (void) { return &fun_10; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
          (TypePointer
            (TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "triplet",
                  nameHsIdent = Identifier
                    "Triplet"}
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_array_6661b46e4a751a85",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPtr
                (HsIncompleteArray
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_6661b46e4a751a85",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_11_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_array_6661b46e4a751a85 (void)) (void))[] { return &fun_11; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
          (TypePointer
            (TypeIncompleteArray
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_array_9c80a9e3300aad15",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "List")))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_9c80a9e3300aad15",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_12_ptr */ __attribute__ ((const)) list *(*hs_bindgen_test_array_9c80a9e3300aad15 (void)) (void) { return &fun_12; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
          (TypePointer
            (TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "list",
                  nameHsIdent = Identifier "List"}
                (TypeIncompleteArray
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_array_bb741b7e8c029e7e",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPtr
                (HsConstArray
                  4
                  (HsConstArray
                    3
                    (HsPrimType HsPrimCInt))))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_bb741b7e8c029e7e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_13_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_array_bb741b7e8c029e7e (void)) (void))[4][3] { return &fun_13; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
          (TypePointer
            (TypeConstArray
              4
              (TypeConstArray
                3
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
        "hs_bindgen_test_array_75d83252a55a5c64",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Matrix")))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_75d83252a55a5c64",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_14_ptr */ __attribute__ ((const)) matrix *(*hs_bindgen_test_array_75d83252a55a5c64 (void)) (void) { return &fun_14; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
          (TypePointer
            (TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "matrix",
                  nameHsIdent = Identifier
                    "Matrix"}
                (TypeConstArray
                  4
                  (TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_array_069ac2d1873f3210",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPtr
                (HsIncompleteArray
                  (HsConstArray
                    3
                    (HsPrimType HsPrimCInt))))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_069ac2d1873f3210",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_15_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_array_069ac2d1873f3210 (void)) (void))[][3] { return &fun_15; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
          (TypePointer
            (TypeIncompleteArray
              (TypeConstArray
                3
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
        "hs_bindgen_test_array_314971335aaa6db3",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Tripletlist")))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_314971335aaa6db3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_16_ptr */ __attribute__ ((const)) tripletlist *(*hs_bindgen_test_array_314971335aaa6db3 (void)) (void) { return &fun_16; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
          (TypePointer
            (TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "tripletlist",
                  nameHsIdent = Identifier
                    "Tripletlist"}
                (TypeIncompleteArray
                  (TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_array_9a62b5848be64bd4",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Sudoku")))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_9a62b5848be64bd4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_solve_ptr */ __attribute__ ((const)) sudoku *(*hs_bindgen_test_array_9a62b5848be64bd4 (void)) (void) { return &solve; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
          (TypePointer
            (TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "sudoku",
                  nameHsIdent = Identifier
                    "Sudoku"}
                (TypeConstArray
                  3
                  (TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "triplet",
                        nameHsIdent = Identifier
                          "Triplet"}
                      (TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral
                            PrimInt
                            Signed)))))))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_array_a6413f4d2092265d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_array_a6413f4d2092265d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr0_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_a6413f4d2092265d (void))[3] { return &arr0; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeConstArray
          3
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
        "hs_bindgen_test_array_1693226264ba4aeb",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_array_1693226264ba4aeb",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_1693226264ba4aeb (void))[3] { return &arr1; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeConstArray
          3
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
        "hs_bindgen_test_array_dafcf99a73b93389",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_array_dafcf99a73b93389",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr2_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_dafcf99a73b93389 (void))[3] { return &arr2; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeConstArray
          3
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
        "hs_bindgen_test_array_ca1016acc3449dee",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_array_ca1016acc3449dee",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr3_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_ca1016acc3449dee (void))[3] { return &arr3; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeConstArray
          3
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
        "hs_bindgen_test_array_1a8c921160bc99a6",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              1
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_array_1a8c921160bc99a6",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr6_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_1a8c921160bc99a6 (void))[1] { return &arr6; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeConstArray
          1
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
        "hs_bindgen_test_array_17cf970243739b65",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsIncompleteArray
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_array_17cf970243739b65",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr7_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_17cf970243739b65 (void))[] { return &arr7; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeIncompleteArray
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
        "hs_bindgen_test_array_85bc33b188037456",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_array_85bc33b188037456",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr_1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_85bc33b188037456 (void))[3] { return &arr_1; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeConstArray
          3
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
        "hs_bindgen_test_array_87c784150cd3ff65",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")))),
      foreignImportOrigName =
      "hs_bindgen_test_array_87c784150cd3ff65",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr_2_ptr */ __attribute__ ((const)) triplet *hs_bindgen_test_array_87c784150cd3ff65 (void) { return &arr_2; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeTypedef
          (TypedefRegular
            NamePair {
              nameC = Name "triplet",
              nameHsIdent = Identifier
                "Triplet"}
            (TypeConstArray
              3
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_array_e7b0de7633a7a62a",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsIncompleteArray
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_array_e7b0de7633a7a62a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr_3_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_e7b0de7633a7a62a (void))[] { return &arr_3; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeIncompleteArray
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
        "hs_bindgen_test_array_8fb64bc6c2bd4c73",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "List")))),
      foreignImportOrigName =
      "hs_bindgen_test_array_8fb64bc6c2bd4c73",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr_4_ptr */ __attribute__ ((const)) list *hs_bindgen_test_array_8fb64bc6c2bd4c73 (void) { return &arr_4; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeTypedef
          (TypedefRegular
            NamePair {
              nameC = Name "list",
              nameHsIdent = Identifier "List"}
            (TypeIncompleteArray
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_array_7348a94e6adce96e",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              4
              (HsConstArray
                3
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_7348a94e6adce96e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr_5_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_7348a94e6adce96e (void))[4][3] { return &arr_5; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeConstArray
          4
          (TypeConstArray
            3
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_array_1308613140bb4b80",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Matrix")))),
      foreignImportOrigName =
      "hs_bindgen_test_array_1308613140bb4b80",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr_6_ptr */ __attribute__ ((const)) matrix *hs_bindgen_test_array_1308613140bb4b80 (void) { return &arr_6; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeTypedef
          (TypedefRegular
            NamePair {
              nameC = Name "matrix",
              nameHsIdent = Identifier
                "Matrix"}
            (TypeConstArray
              4
              (TypeConstArray
                3
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
        "hs_bindgen_test_array_a060984b378ed676",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsIncompleteArray
              (HsConstArray
                3
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_a060984b378ed676",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr_7_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_a060984b378ed676 (void))[][3] { return &arr_7; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeIncompleteArray
          (TypeConstArray
            3
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_array_d82706abb6d8ea04",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Tripletlist")))),
      foreignImportOrigName =
      "hs_bindgen_test_array_d82706abb6d8ea04",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr_8_ptr */ __attribute__ ((const)) tripletlist *hs_bindgen_test_array_d82706abb6d8ea04 (void) { return &arr_8; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeTypedef
          (TypedefRegular
            NamePair {
              nameC = Name "tripletlist",
              nameHsIdent = Identifier
                "Tripletlist"}
            (TypeIncompleteArray
              (TypeConstArray
                3
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
        "hs_bindgen_test_array_7376d172f5729493",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_array_7376d172f5729493",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr_1_const_ptr */ __attribute__ ((const)) signed int const (*hs_bindgen_test_array_7376d172f5729493 (void))[3] { return &arr_1_const; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeConstArray
          3
          (TypeQualified
            TypeQualifierConst
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
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
        "hs_bindgen_test_array_f03586aa57dfce29",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Triplet")))),
      foreignImportOrigName =
      "hs_bindgen_test_array_f03586aa57dfce29",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr_2_const_ptr */ __attribute__ ((const)) triplet const *hs_bindgen_test_array_f03586aa57dfce29 (void) { return &arr_2_const; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeQualified
          TypeQualifierConst
          (TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = Name "triplet",
                nameHsIdent = Identifier
                  "Triplet"}
              (TypeConstArray
                3
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))))),
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
        "hs_bindgen_test_array_54ffd4ffcd2dad61",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsIncompleteArray
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_array_54ffd4ffcd2dad61",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr_3_const_ptr */ __attribute__ ((const)) signed int const (*hs_bindgen_test_array_54ffd4ffcd2dad61 (void))[] { return &arr_3_const; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeIncompleteArray
          (TypeQualified
            TypeQualifierConst
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_array_8896c2ff5b9ce9c9",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "List")))),
      foreignImportOrigName =
      "hs_bindgen_test_array_8896c2ff5b9ce9c9",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr_4_const_ptr */ __attribute__ ((const)) list const *hs_bindgen_test_array_8896c2ff5b9ce9c9 (void) { return &arr_4_const; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeQualified
          TypeQualifierConst
          (TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = Name "list",
                nameHsIdent = Identifier "List"}
              (TypeIncompleteArray
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
        "hs_bindgen_test_array_46b406e096f6c9c1",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              4
              (HsConstArray
                3
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_46b406e096f6c9c1",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr_5_const_ptr */ __attribute__ ((const)) signed int const (*hs_bindgen_test_array_46b406e096f6c9c1 (void))[4][3] { return &arr_5_const; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeConstArray
          4
          (TypeConstArray
            3
            (TypeQualified
              TypeQualifierConst
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
        "hs_bindgen_test_array_ceb7f2027865ce12",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Matrix")))),
      foreignImportOrigName =
      "hs_bindgen_test_array_ceb7f2027865ce12",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr_6_const_ptr */ __attribute__ ((const)) matrix const *hs_bindgen_test_array_ceb7f2027865ce12 (void) { return &arr_6_const; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeQualified
          TypeQualifierConst
          (TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = Name "matrix",
                nameHsIdent = Identifier
                  "Matrix"}
              (TypeConstArray
                4
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))))),
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
        "hs_bindgen_test_array_2b565b2b97acdcb7",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsIncompleteArray
              (HsConstArray
                3
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_array_2b565b2b97acdcb7",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr_7_const_ptr */ __attribute__ ((const)) signed int const (*hs_bindgen_test_array_2b565b2b97acdcb7 (void))[][3] { return &arr_7_const; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeIncompleteArray
          (TypeConstArray
            3
            (TypeQualified
              TypeQualifierConst
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_array_03e2d9c4ef2ae993",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Tripletlist")))),
      foreignImportOrigName =
      "hs_bindgen_test_array_03e2d9c4ef2ae993",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr_8_const_ptr */ __attribute__ ((const)) tripletlist const *hs_bindgen_test_array_03e2d9c4ef2ae993 (void) { return &arr_8_const; } ",
          capiWrapperImport = "array.h"},
      foreignImportOrigin = Global
        (TypeQualified
          TypeQualifierConst
          (TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = Name "tripletlist",
                nameHsIdent = Identifier
                  "Tripletlist"}
              (TypeIncompleteArray
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
