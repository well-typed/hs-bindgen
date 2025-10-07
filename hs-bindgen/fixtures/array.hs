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
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_1_wrapper",
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
            "array.h:87:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_2_wrapper",
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
                      "Triplet"}))],
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
            "array.h:90:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_3_wrapper",
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
            "array.h:93:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_4_wrapper",
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
                    nameHsIdent = Identifier
                      "List"}))],
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
            "array.h:96:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_5_wrapper",
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
            "array.h:99:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_6_wrapper",
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
                      "Matrix"}))],
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
            "array.h:102:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_7_wrapper",
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
            "array.h:105:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_8_wrapper",
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
                      "Tripletlist"}))],
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
            "array.h:108:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
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
            "array.h:120:7",
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
                    "Triplet"}))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of known size, typedef"],
          commentOrigin = Just "fun_10",
          commentLocation = Just
            "array.h:123:10",
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
            "array.h:126:7",
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
                  nameHsIdent = Identifier
                    "List"}))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of unknown size, typedef"],
          commentOrigin = Just "fun_12",
          commentLocation = Just
            "array.h:129:7",
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
            "array.h:132:7",
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
                    "Matrix"}))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of known size, typedef"],
          commentOrigin = Just "fun_14",
          commentLocation = Just
            "array.h:135:9",
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
            "array.h:138:7",
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
                    "Tripletlist"}))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of unknown size, typedef"],
          commentOrigin = Just "fun_16",
          commentLocation = Just
            "array.h:141:14",
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
        "fun_1_wrapper",
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
            "array.h:87:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_2_wrapper",
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
                      "Triplet"}))],
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
            "array.h:90:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_3_wrapper",
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
            "array.h:93:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_4_wrapper",
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
                    nameHsIdent = Identifier
                      "List"}))],
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
            "array.h:96:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_5_wrapper",
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
            "array.h:99:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_6_wrapper",
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
                      "Matrix"}))],
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
            "array.h:102:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_7_wrapper",
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
            "array.h:105:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun_8_wrapper",
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
                      "Tripletlist"}))],
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
            "array.h:108:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["array.h"],
              headerInclude = "array.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
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
            "array.h:120:7",
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
                    "Triplet"}))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of known size, typedef"],
          commentOrigin = Just "fun_10",
          commentLocation = Just
            "array.h:123:10",
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
            "array.h:126:7",
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
                  nameHsIdent = Identifier
                    "List"}))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Array of unknown size, typedef"],
          commentOrigin = Just "fun_12",
          commentLocation = Just
            "array.h:129:7",
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
            "array.h:132:7",
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
                    "Matrix"}))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of known size, typedef"],
          commentOrigin = Just "fun_14",
          commentLocation = Just
            "array.h:135:9",
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
            "array.h:138:7",
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
                    "Tripletlist"}))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Multi-dimensional array of unknown size, typedef"],
          commentOrigin = Just "fun_16",
          commentLocation = Just
            "array.h:141:14",
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
                    "Triplet"})]
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
                  nameHsIdent = Identifier
                    "List"})]
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
                    "Matrix"})]
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
                    "Tripletlist"})]
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
                    "Triplet"})))),
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
                  nameHsIdent = Identifier
                    "List"})))),
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
                    "Matrix"})))),
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
                    "Tripletlist"})))),
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
                "Triplet"})),
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
              nameHsIdent = Identifier
                "List"})),
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
                "Matrix"})),
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
                "Tripletlist"})),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
