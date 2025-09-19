[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Complex_object_t",
      structConstr = HsName
        "@NsConstr"
        "Complex_object_t",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "complex_object_t_velocity",
          fieldType = HsComplexType
            HsPrimCFloat,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "hsb_complex_test.h:25:20",
                fieldName = NamePair {
                  nameC = Name "velocity",
                  nameHsIdent = HsIdentifier
                    "complex_object_t_velocity"},
                fieldComment = Nothing},
              structFieldType = TypeComplex
                (PrimFloating PrimFloat),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "velocity",
              commentLocation = Just
                "hsb_complex_test.h:25:20",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["hsb_complex_test.h"],
                  headerInclude =
                  "hsb_complex_test.h"},
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "complex_object_t_position",
          fieldType = HsComplexType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "hsb_complex_test.h:26:20",
                fieldName = NamePair {
                  nameC = Name "position",
                  nameHsIdent = HsIdentifier
                    "complex_object_t_position"},
                fieldComment = Nothing},
              structFieldType = TypeComplex
                (PrimFloating PrimDouble),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "position",
              commentLocation = Just
                "hsb_complex_test.h:26:20",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["hsb_complex_test.h"],
                  headerInclude =
                  "hsb_complex_test.h"},
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "complex_object_t_id",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "hsb_complex_test.h:27:9",
                fieldName = NamePair {
                  nameC = Name "id",
                  nameHsIdent = HsIdentifier
                    "complex_object_t_id"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 192,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "id",
              commentLocation = Just
                "hsb_complex_test.h:27:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["hsb_complex_test.h"],
                  headerInclude =
                  "hsb_complex_test.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "hsb_complex_test.h:24:9",
            declId = NamePair {
              nameC = Name "complex_object_t",
              nameHsIdent = HsIdentifier
                "Complex_object_t"},
            declOrigin = NameOriginGenerated
              (AnonId
                "hsb_complex_test.h:24:9"),
            declAliases = [
              Name "complex_object_t"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["hsb_complex_test.h"],
                headerInclude =
                "hsb_complex_test.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Complex_object_t"),
              structSizeof = 32,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "hsb_complex_test.h:25:20",
                    fieldName = NamePair {
                      nameC = Name "velocity",
                      nameHsIdent = HsIdentifier
                        "complex_object_t_velocity"},
                    fieldComment = Nothing},
                  structFieldType = TypeComplex
                    (PrimFloating PrimFloat),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "hsb_complex_test.h:26:20",
                    fieldName = NamePair {
                      nameC = Name "position",
                      nameHsIdent = HsIdentifier
                        "complex_object_t_position"},
                    fieldComment = Nothing},
                  structFieldType = TypeComplex
                    (PrimFloating PrimDouble),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "hsb_complex_test.h:27:9",
                    fieldName = NamePair {
                      nameC = Name "id",
                      nameHsIdent = HsIdentifier
                        "complex_object_t_id"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 192,
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
            "complex_object_t",
          commentLocation = Just
            "hsb_complex_test.h:24:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["hsb_complex_test.h"],
              headerInclude =
              "hsb_complex_test.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Complex_object_t",
          structConstr = HsName
            "@NsConstr"
            "Complex_object_t",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "complex_object_t_velocity",
              fieldType = HsComplexType
                HsPrimCFloat,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "hsb_complex_test.h:25:20",
                    fieldName = NamePair {
                      nameC = Name "velocity",
                      nameHsIdent = HsIdentifier
                        "complex_object_t_velocity"},
                    fieldComment = Nothing},
                  structFieldType = TypeComplex
                    (PrimFloating PrimFloat),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "velocity",
                  commentLocation = Just
                    "hsb_complex_test.h:25:20",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["hsb_complex_test.h"],
                      headerInclude =
                      "hsb_complex_test.h"},
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "complex_object_t_position",
              fieldType = HsComplexType
                HsPrimCDouble,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "hsb_complex_test.h:26:20",
                    fieldName = NamePair {
                      nameC = Name "position",
                      nameHsIdent = HsIdentifier
                        "complex_object_t_position"},
                    fieldComment = Nothing},
                  structFieldType = TypeComplex
                    (PrimFloating PrimDouble),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "position",
                  commentLocation = Just
                    "hsb_complex_test.h:26:20",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["hsb_complex_test.h"],
                      headerInclude =
                      "hsb_complex_test.h"},
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "complex_object_t_id",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "hsb_complex_test.h:27:9",
                    fieldName = NamePair {
                      nameC = Name "id",
                      nameHsIdent = HsIdentifier
                        "complex_object_t_id"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 192,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "id",
                  commentLocation = Just
                    "hsb_complex_test.h:27:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["hsb_complex_test.h"],
                      headerInclude =
                      "hsb_complex_test.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "hsb_complex_test.h:24:9",
                declId = NamePair {
                  nameC = Name "complex_object_t",
                  nameHsIdent = HsIdentifier
                    "Complex_object_t"},
                declOrigin = NameOriginGenerated
                  (AnonId
                    "hsb_complex_test.h:24:9"),
                declAliases = [
                  Name "complex_object_t"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["hsb_complex_test.h"],
                    headerInclude =
                    "hsb_complex_test.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName
                      "@NsConstr"
                      "Complex_object_t"),
                  structSizeof = 32,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "hsb_complex_test.h:25:20",
                        fieldName = NamePair {
                          nameC = Name "velocity",
                          nameHsIdent = HsIdentifier
                            "complex_object_t_velocity"},
                        fieldComment = Nothing},
                      structFieldType = TypeComplex
                        (PrimFloating PrimFloat),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "hsb_complex_test.h:26:20",
                        fieldName = NamePair {
                          nameC = Name "position",
                          nameHsIdent = HsIdentifier
                            "complex_object_t_position"},
                        fieldComment = Nothing},
                      structFieldType = TypeComplex
                        (PrimFloating PrimDouble),
                      structFieldOffset = 64,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "hsb_complex_test.h:27:9",
                        fieldName = NamePair {
                          nameC = Name "id",
                          nameHsIdent = HsIdentifier
                            "complex_object_t_id"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 192,
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
                "complex_object_t",
              commentLocation = Just
                "hsb_complex_test.h:24:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["hsb_complex_test.h"],
                  headerInclude =
                  "hsb_complex_test.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 32,
          storableAlignment = 8,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Complex_object_t",
                  structConstr = HsName
                    "@NsConstr"
                    "Complex_object_t",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "complex_object_t_velocity",
                      fieldType = HsComplexType
                        HsPrimCFloat,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "hsb_complex_test.h:25:20",
                            fieldName = NamePair {
                              nameC = Name "velocity",
                              nameHsIdent = HsIdentifier
                                "complex_object_t_velocity"},
                            fieldComment = Nothing},
                          structFieldType = TypeComplex
                            (PrimFloating PrimFloat),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "velocity",
                          commentLocation = Just
                            "hsb_complex_test.h:25:20",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["hsb_complex_test.h"],
                              headerInclude =
                              "hsb_complex_test.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "complex_object_t_position",
                      fieldType = HsComplexType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "hsb_complex_test.h:26:20",
                            fieldName = NamePair {
                              nameC = Name "position",
                              nameHsIdent = HsIdentifier
                                "complex_object_t_position"},
                            fieldComment = Nothing},
                          structFieldType = TypeComplex
                            (PrimFloating PrimDouble),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "position",
                          commentLocation = Just
                            "hsb_complex_test.h:26:20",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["hsb_complex_test.h"],
                              headerInclude =
                              "hsb_complex_test.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "complex_object_t_id",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "hsb_complex_test.h:27:9",
                            fieldName = NamePair {
                              nameC = Name "id",
                              nameHsIdent = HsIdentifier
                                "complex_object_t_id"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 192,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "id",
                          commentLocation = Just
                            "hsb_complex_test.h:27:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["hsb_complex_test.h"],
                              headerInclude =
                              "hsb_complex_test.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "hsb_complex_test.h:24:9",
                        declId = NamePair {
                          nameC = Name "complex_object_t",
                          nameHsIdent = HsIdentifier
                            "Complex_object_t"},
                        declOrigin = NameOriginGenerated
                          (AnonId
                            "hsb_complex_test.h:24:9"),
                        declAliases = [
                          Name "complex_object_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["hsb_complex_test.h"],
                            headerInclude =
                            "hsb_complex_test.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Complex_object_t"),
                          structSizeof = 32,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "hsb_complex_test.h:25:20",
                                fieldName = NamePair {
                                  nameC = Name "velocity",
                                  nameHsIdent = HsIdentifier
                                    "complex_object_t_velocity"},
                                fieldComment = Nothing},
                              structFieldType = TypeComplex
                                (PrimFloating PrimFloat),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "hsb_complex_test.h:26:20",
                                fieldName = NamePair {
                                  nameC = Name "position",
                                  nameHsIdent = HsIdentifier
                                    "complex_object_t_position"},
                                fieldComment = Nothing},
                              structFieldType = TypeComplex
                                (PrimFloating PrimDouble),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "hsb_complex_test.h:27:9",
                                fieldName = NamePair {
                                  nameC = Name "id",
                                  nameHsIdent = HsIdentifier
                                    "complex_object_t_id"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 192,
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
                        "complex_object_t",
                      commentLocation = Just
                        "hsb_complex_test.h:24:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["hsb_complex_test.h"],
                          headerInclude =
                          "hsb_complex_test.h"},
                      commentChildren = []}})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 8,
                PeekByteOff (Idx 0) 24]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Complex_object_t",
                  structConstr = HsName
                    "@NsConstr"
                    "Complex_object_t",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "complex_object_t_velocity",
                      fieldType = HsComplexType
                        HsPrimCFloat,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "hsb_complex_test.h:25:20",
                            fieldName = NamePair {
                              nameC = Name "velocity",
                              nameHsIdent = HsIdentifier
                                "complex_object_t_velocity"},
                            fieldComment = Nothing},
                          structFieldType = TypeComplex
                            (PrimFloating PrimFloat),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "velocity",
                          commentLocation = Just
                            "hsb_complex_test.h:25:20",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["hsb_complex_test.h"],
                              headerInclude =
                              "hsb_complex_test.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "complex_object_t_position",
                      fieldType = HsComplexType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "hsb_complex_test.h:26:20",
                            fieldName = NamePair {
                              nameC = Name "position",
                              nameHsIdent = HsIdentifier
                                "complex_object_t_position"},
                            fieldComment = Nothing},
                          structFieldType = TypeComplex
                            (PrimFloating PrimDouble),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "position",
                          commentLocation = Just
                            "hsb_complex_test.h:26:20",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["hsb_complex_test.h"],
                              headerInclude =
                              "hsb_complex_test.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "complex_object_t_id",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "hsb_complex_test.h:27:9",
                            fieldName = NamePair {
                              nameC = Name "id",
                              nameHsIdent = HsIdentifier
                                "complex_object_t_id"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 192,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "id",
                          commentLocation = Just
                            "hsb_complex_test.h:27:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["hsb_complex_test.h"],
                              headerInclude =
                              "hsb_complex_test.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "hsb_complex_test.h:24:9",
                        declId = NamePair {
                          nameC = Name "complex_object_t",
                          nameHsIdent = HsIdentifier
                            "Complex_object_t"},
                        declOrigin = NameOriginGenerated
                          (AnonId
                            "hsb_complex_test.h:24:9"),
                        declAliases = [
                          Name "complex_object_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["hsb_complex_test.h"],
                            headerInclude =
                            "hsb_complex_test.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Complex_object_t"),
                          structSizeof = 32,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "hsb_complex_test.h:25:20",
                                fieldName = NamePair {
                                  nameC = Name "velocity",
                                  nameHsIdent = HsIdentifier
                                    "complex_object_t_velocity"},
                                fieldComment = Nothing},
                              structFieldType = TypeComplex
                                (PrimFloating PrimFloat),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "hsb_complex_test.h:26:20",
                                fieldName = NamePair {
                                  nameC = Name "position",
                                  nameHsIdent = HsIdentifier
                                    "complex_object_t_position"},
                                fieldComment = Nothing},
                              structFieldType = TypeComplex
                                (PrimFloating PrimDouble),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "hsb_complex_test.h:27:9",
                                fieldName = NamePair {
                                  nameC = Name "id",
                                  nameHsIdent = HsIdentifier
                                    "complex_object_t_id"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 192,
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
                        "complex_object_t",
                      commentLocation = Just
                        "hsb_complex_test.h:24:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["hsb_complex_test.h"],
                          headerInclude =
                          "hsb_complex_test.h"},
                      commentChildren = []}}
                (Add 3)
                (Seq
                  [
                    PokeByteOff (Idx 4) 0 (Idx 0),
                    PokeByteOff (Idx 4) 8 (Idx 1),
                    PokeByteOff
                      (Idx 4)
                      24
                      (Idx 2)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Complex_object_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Complex_object_t",
      deriveInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "multiply_complex_f_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "a"),
          functionParameterType = HsPtr
            (HsComplexType HsPrimCFloat),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "a",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "b"),
          functionParameterType = HsPtr
            (HsComplexType HsPrimCFloat),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "b",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      HeapResultType
        (HsPtr
          (HsComplexType HsPrimCFloat)),
      foreignImportOrigName =
      "hs_bindgen_test_hsb_complex_test_b84ea846e04d5fd6",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_hsb_complex_test_b84ea846e04d5fd6 (float _Complex *arg1, float _Complex *arg2, float _Complex *arg3) { *arg3 = multiply_complex_f(*arg1, *arg2); }",
          capiWrapperImport =
          "hsb_complex_test.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "a",
                  nameHsIdent = HsIdentifier "a"})
              (TypeComplex
                (PrimFloating PrimFloat)),
            _×_
              (Just
                NamePair {
                  nameC = Name "b",
                  nameHsIdent = HsIdentifier "b"})
              (TypeComplex
                (PrimFloating PrimFloat))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeComplex
            (PrimFloating PrimFloat)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "multiply_complex_f",
          commentLocation = Just
            "hsb_complex_test.h:21:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["hsb_complex_test.h"],
              headerInclude =
              "hsb_complex_test.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "add_complex_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "a"),
          functionParameterType = HsPtr
            (HsComplexType HsPrimCDouble),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "a",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "b"),
          functionParameterType = HsPtr
            (HsComplexType HsPrimCDouble),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "b",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      HeapResultType
        (HsPtr
          (HsComplexType HsPrimCDouble)),
      foreignImportOrigName =
      "hs_bindgen_test_hsb_complex_test_8dd079d1707c36b3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_hsb_complex_test_8dd079d1707c36b3 (double _Complex *arg1, double _Complex *arg2, double _Complex *arg3) { *arg3 = add_complex(*arg1, *arg2); }",
          capiWrapperImport =
          "hsb_complex_test.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "a",
                  nameHsIdent = HsIdentifier "a"})
              (TypeComplex
                (PrimFloating PrimDouble)),
            _×_
              (Just
                NamePair {
                  nameC = Name "b",
                  nameHsIdent = HsIdentifier "b"})
              (TypeComplex
                (PrimFloating PrimDouble))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeComplex
            (PrimFloating PrimDouble)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "add_complex",
          commentLocation = Just
            "hsb_complex_test.h:22:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["hsb_complex_test.h"],
              headerInclude =
              "hsb_complex_test.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "multiply_complex_f_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "a"),
          functionParameterType = HsPtr
            (HsComplexType HsPrimCFloat),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "a",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "b"),
          functionParameterType = HsPtr
            (HsComplexType HsPrimCFloat),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "b",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      HeapResultType
        (HsPtr
          (HsComplexType HsPrimCFloat)),
      foreignImportOrigName =
      "hs_bindgen_test_hsb_complex_test_5b05fdb10924da35",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_hsb_complex_test_5b05fdb10924da35 (float _Complex *arg1, float _Complex *arg2, float _Complex *arg3) { *arg3 = multiply_complex_f(*arg1, *arg2); }",
          capiWrapperImport =
          "hsb_complex_test.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "a",
                  nameHsIdent = HsIdentifier "a"})
              (TypeComplex
                (PrimFloating PrimFloat)),
            _×_
              (Just
                NamePair {
                  nameC = Name "b",
                  nameHsIdent = HsIdentifier "b"})
              (TypeComplex
                (PrimFloating PrimFloat))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeComplex
            (PrimFloating PrimFloat)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "multiply_complex_f",
          commentLocation = Just
            "hsb_complex_test.h:21:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["hsb_complex_test.h"],
              headerInclude =
              "hsb_complex_test.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "add_complex_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "a"),
          functionParameterType = HsPtr
            (HsComplexType HsPrimCDouble),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "a",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "b"),
          functionParameterType = HsPtr
            (HsComplexType HsPrimCDouble),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "b",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      HeapResultType
        (HsPtr
          (HsComplexType HsPrimCDouble)),
      foreignImportOrigName =
      "hs_bindgen_test_hsb_complex_test_59f299d5d991ed72",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_hsb_complex_test_59f299d5d991ed72 (double _Complex *arg1, double _Complex *arg2, double _Complex *arg3) { *arg3 = add_complex(*arg1, *arg2); }",
          capiWrapperImport =
          "hsb_complex_test.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "a",
                  nameHsIdent = HsIdentifier "a"})
              (TypeComplex
                (PrimFloating PrimDouble)),
            _×_
              (Just
                NamePair {
                  nameC = Name "b",
                  nameHsIdent = HsIdentifier "b"})
              (TypeComplex
                (PrimFloating PrimDouble))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeComplex
            (PrimFloating PrimDouble)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "add_complex",
          commentLocation = Just
            "hsb_complex_test.h:22:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["hsb_complex_test.h"],
              headerInclude =
              "hsb_complex_test.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_hsb_complex_test_a7d89c01385c8c56",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsComplexType HsPrimCFloat)
              (HsFun
                (HsComplexType HsPrimCFloat)
                (HsIO
                  (HsComplexType
                    HsPrimCFloat)))))),
      foreignImportOrigName =
      "hs_bindgen_test_hsb_complex_test_a7d89c01385c8c56",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_multiply_complex_f_ptr */ __attribute__ ((const)) float _Complex (*hs_bindgen_test_hsb_complex_test_a7d89c01385c8c56 (void)) (float _Complex arg1, float _Complex arg2) { return &multiply_complex_f; } ",
          capiWrapperImport =
          "hsb_complex_test.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeComplex
              (PrimFloating PrimFloat),
            TypeComplex
              (PrimFloating PrimFloat)]
          (TypeComplex
            (PrimFloating PrimFloat))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "multiply_complex_f",
          commentLocation = Just
            "hsb_complex_test.h:21:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["hsb_complex_test.h"],
              headerInclude =
              "hsb_complex_test.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_hsb_complex_test_b6226a5bde741b3f",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsComplexType HsPrimCDouble)
              (HsFun
                (HsComplexType HsPrimCDouble)
                (HsIO
                  (HsComplexType
                    HsPrimCDouble)))))),
      foreignImportOrigName =
      "hs_bindgen_test_hsb_complex_test_b6226a5bde741b3f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_add_complex_ptr */ __attribute__ ((const)) double _Complex (*hs_bindgen_test_hsb_complex_test_b6226a5bde741b3f (void)) (double _Complex arg1, double _Complex arg2) { return &add_complex; } ",
          capiWrapperImport =
          "hsb_complex_test.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeComplex
              (PrimFloating PrimDouble),
            TypeComplex
              (PrimFloating PrimDouble)]
          (TypeComplex
            (PrimFloating PrimDouble))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "add_complex",
          commentLocation = Just
            "hsb_complex_test.h:22:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["hsb_complex_test.h"],
              headerInclude =
              "hsb_complex_test.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_hsb_complex_test_69e4d4972011967b",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsComplexType HsPrimCFloat))),
      foreignImportOrigName =
      "hs_bindgen_test_hsb_complex_test_69e4d4972011967b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_global_complex_float_ptr */ __attribute__ ((const)) float _Complex *hs_bindgen_test_hsb_complex_test_69e4d4972011967b (void) { return &global_complex_float; } ",
          capiWrapperImport =
          "hsb_complex_test.h"},
      foreignImportOrigin = Global
        (TypeComplex
          (PrimFloating PrimFloat)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "global_complex_float",
          commentLocation = Just
            "hsb_complex_test.h:3:23",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["hsb_complex_test.h"],
              headerInclude =
              "hsb_complex_test.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_hsb_complex_test_c3633906ced5dab3",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsComplexType HsPrimCDouble))),
      foreignImportOrigName =
      "hs_bindgen_test_hsb_complex_test_c3633906ced5dab3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_global_complex_double_ptr */ __attribute__ ((const)) double _Complex *hs_bindgen_test_hsb_complex_test_c3633906ced5dab3 (void) { return &global_complex_double; } ",
          capiWrapperImport =
          "hsb_complex_test.h"},
      foreignImportOrigin = Global
        (TypeComplex
          (PrimFloating PrimDouble)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "global_complex_double",
          commentLocation = Just
            "hsb_complex_test.h:4:23",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["hsb_complex_test.h"],
              headerInclude =
              "hsb_complex_test.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_hsb_complex_test_7ef41813e25ff8c1",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsComplexType HsPrimCFloat))),
      foreignImportOrigName =
      "hs_bindgen_test_hsb_complex_test_7ef41813e25ff8c1",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_global_complex_float_flipped_ptr */ __attribute__ ((const)) float _Complex *hs_bindgen_test_hsb_complex_test_7ef41813e25ff8c1 (void) { return &global_complex_float_flipped; } ",
          capiWrapperImport =
          "hsb_complex_test.h"},
      foreignImportOrigin = Global
        (TypeComplex
          (PrimFloating PrimFloat)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "global_complex_float_flipped",
          commentLocation = Just
            "hsb_complex_test.h:6:23",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["hsb_complex_test.h"],
              headerInclude =
              "hsb_complex_test.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_hsb_complex_test_abdd562bd1b14921",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsComplexType HsPrimCDouble))),
      foreignImportOrigName =
      "hs_bindgen_test_hsb_complex_test_abdd562bd1b14921",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_global_complex_double_flipped_ptr */ __attribute__ ((const)) double _Complex *hs_bindgen_test_hsb_complex_test_abdd562bd1b14921 (void) { return &global_complex_double_flipped; } ",
          capiWrapperImport =
          "hsb_complex_test.h"},
      foreignImportOrigin = Global
        (TypeComplex
          (PrimFloating PrimDouble)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "global_complex_double_flipped",
          commentLocation = Just
            "hsb_complex_test.h:7:23",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["hsb_complex_test.h"],
              headerInclude =
              "hsb_complex_test.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_hsb_complex_test_02f701d4163d6ce7",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsComplexType HsPrimCFloat))),
      foreignImportOrigName =
      "hs_bindgen_test_hsb_complex_test_02f701d4163d6ce7",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_global_Complex_float_ptr */ __attribute__ ((const)) float _Complex *hs_bindgen_test_hsb_complex_test_02f701d4163d6ce7 (void) { return &global_Complex_float; } ",
          capiWrapperImport =
          "hsb_complex_test.h"},
      foreignImportOrigin = Global
        (TypeComplex
          (PrimFloating PrimFloat)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "global_Complex_float",
          commentLocation = Just
            "hsb_complex_test.h:9:24",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["hsb_complex_test.h"],
              headerInclude =
              "hsb_complex_test.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_hsb_complex_test_a6117bb5e7cacd17",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsComplexType HsPrimCDouble))),
      foreignImportOrigName =
      "hs_bindgen_test_hsb_complex_test_a6117bb5e7cacd17",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_global_Complex_double_ptr */ __attribute__ ((const)) double _Complex *hs_bindgen_test_hsb_complex_test_a6117bb5e7cacd17 (void) { return &global_Complex_double; } ",
          capiWrapperImport =
          "hsb_complex_test.h"},
      foreignImportOrigin = Global
        (TypeComplex
          (PrimFloating PrimDouble)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "global_Complex_double",
          commentLocation = Just
            "hsb_complex_test.h:10:24",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["hsb_complex_test.h"],
              headerInclude =
              "hsb_complex_test.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_hsb_complex_test_da2309480d364cee",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsComplexType HsPrimCFloat))),
      foreignImportOrigName =
      "hs_bindgen_test_hsb_complex_test_da2309480d364cee",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_global_Complex_float_flipped_ptr */ __attribute__ ((const)) float _Complex *hs_bindgen_test_hsb_complex_test_da2309480d364cee (void) { return &global_Complex_float_flipped; } ",
          capiWrapperImport =
          "hsb_complex_test.h"},
      foreignImportOrigin = Global
        (TypeComplex
          (PrimFloating PrimFloat)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "global_Complex_float_flipped",
          commentLocation = Just
            "hsb_complex_test.h:12:24",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["hsb_complex_test.h"],
              headerInclude =
              "hsb_complex_test.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_hsb_complex_test_467427dc59fbef50",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsComplexType HsPrimCDouble))),
      foreignImportOrigName =
      "hs_bindgen_test_hsb_complex_test_467427dc59fbef50",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_global_Complex_double_flipped_ptr */ __attribute__ ((const)) double _Complex *hs_bindgen_test_hsb_complex_test_467427dc59fbef50 (void) { return &global_Complex_double_flipped; } ",
          capiWrapperImport =
          "hsb_complex_test.h"},
      foreignImportOrigin = Global
        (TypeComplex
          (PrimFloating PrimDouble)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "global_Complex_double_flipped",
          commentLocation = Just
            "hsb_complex_test.h:13:24",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["hsb_complex_test.h"],
              headerInclude =
              "hsb_complex_test.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_hsb_complex_test_bb0fb18f3dfee47d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsComplexType HsPrimCFloat))),
      foreignImportOrigName =
      "hs_bindgen_test_hsb_complex_test_bb0fb18f3dfee47d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_complex_float_ptr */ __attribute__ ((const)) float _Complex const *hs_bindgen_test_hsb_complex_test_bb0fb18f3dfee47d (void) { return &const_complex_float; } ",
          capiWrapperImport =
          "hsb_complex_test.h"},
      foreignImportOrigin = Global
        (TypeConst
          (TypeComplex
            (PrimFloating PrimFloat))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_complex_float",
          commentLocation = Just
            "hsb_complex_test.h:15:29",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["hsb_complex_test.h"],
              headerInclude =
              "hsb_complex_test.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_hsb_complex_test_f491f52e529a459a",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsComplexType HsPrimCDouble))),
      foreignImportOrigName =
      "hs_bindgen_test_hsb_complex_test_f491f52e529a459a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_complex_double_ptr */ __attribute__ ((const)) double _Complex const *hs_bindgen_test_hsb_complex_test_f491f52e529a459a (void) { return &const_complex_double; } ",
          capiWrapperImport =
          "hsb_complex_test.h"},
      foreignImportOrigin = Global
        (TypeConst
          (TypeComplex
            (PrimFloating PrimDouble))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_complex_double",
          commentLocation = Just
            "hsb_complex_test.h:16:29",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["hsb_complex_test.h"],
              headerInclude =
              "hsb_complex_test.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_hsb_complex_test_ecb5f4a0ccb7ee75",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsComplexType HsPrimCFloat))),
      foreignImportOrigName =
      "hs_bindgen_test_hsb_complex_test_ecb5f4a0ccb7ee75",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_volatile_complex_float_ptr */ __attribute__ ((const)) float _Complex *hs_bindgen_test_hsb_complex_test_ecb5f4a0ccb7ee75 (void) { return &volatile_complex_float; } ",
          capiWrapperImport =
          "hsb_complex_test.h"},
      foreignImportOrigin = Global
        (TypeComplex
          (PrimFloating PrimFloat)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "volatile_complex_float",
          commentLocation = Just
            "hsb_complex_test.h:18:23",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["hsb_complex_test.h"],
              headerInclude =
              "hsb_complex_test.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_hsb_complex_test_6b136090c38a69c4",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsComplexType HsPrimCDouble))),
      foreignImportOrigName =
      "hs_bindgen_test_hsb_complex_test_6b136090c38a69c4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_volatile_complex_double_ptr */ __attribute__ ((const)) double _Complex *hs_bindgen_test_hsb_complex_test_6b136090c38a69c4 (void) { return &volatile_complex_double; } ",
          capiWrapperImport =
          "hsb_complex_test.h"},
      foreignImportOrigin = Global
        (TypeComplex
          (PrimFloating PrimDouble)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "volatile_complex_double",
          commentLocation = Just
            "hsb_complex_test.h:19:23",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["hsb_complex_test.h"],
              headerInclude =
              "hsb_complex_test.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_hsb_complex_test_55b7fb104be53f70",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              10
              (HsComplexType HsPrimCFloat)))),
      foreignImportOrigName =
      "hs_bindgen_test_hsb_complex_test_55b7fb104be53f70",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_complex_float_array_ptr */ __attribute__ ((const)) float _Complex (*hs_bindgen_test_hsb_complex_test_55b7fb104be53f70 (void))[10] { return &complex_float_array; } ",
          capiWrapperImport =
          "hsb_complex_test.h"},
      foreignImportOrigin = Global
        (TypeConstArray
          10
          (TypeComplex
            (PrimFloating PrimFloat))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "complex_float_array",
          commentLocation = Just
            "hsb_complex_test.h:30:23",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["hsb_complex_test.h"],
              headerInclude =
              "hsb_complex_test.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_hsb_complex_test_0b63f3bda9243457",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              10
              (HsComplexType
                HsPrimCDouble)))),
      foreignImportOrigName =
      "hs_bindgen_test_hsb_complex_test_0b63f3bda9243457",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_complex_double_array_ptr */ __attribute__ ((const)) double _Complex (*hs_bindgen_test_hsb_complex_test_0b63f3bda9243457 (void))[10] { return &complex_double_array; } ",
          capiWrapperImport =
          "hsb_complex_test.h"},
      foreignImportOrigin = Global
        (TypeConstArray
          10
          (TypeComplex
            (PrimFloating PrimDouble))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "complex_double_array",
          commentLocation = Just
            "hsb_complex_test.h:31:23",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["hsb_complex_test.h"],
              headerInclude =
              "hsb_complex_test.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
