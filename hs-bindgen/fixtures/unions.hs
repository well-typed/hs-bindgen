[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Dim2",
      structConstr = HsName
        "@NsConstr"
        "Dim2",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "dim2_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:2:9",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier
                    "dim2_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Just
                "unions.h:2:9",
              commentHeader = Just "unions.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "dim2_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:3:9",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier
                    "dim2_y"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Just
                "unions.h:3:9",
              commentHeader = Just "unions.h",
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "unions.h:1:8",
            declId = NamePair {
              nameC = Name "Dim2",
              nameHsIdent = HsIdentifier
                "Dim2"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "unions.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Dim2"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:2:9",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "dim2_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:3:9",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "dim2_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
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
          commentOrigin = Just "Dim2",
          commentLocation = Just
            "unions.h:1:8",
          commentHeader = Just "unions.h",
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Dim2",
          structConstr = HsName
            "@NsConstr"
            "Dim2",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "dim2_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:2:9",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "dim2_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "x",
                  commentLocation = Just
                    "unions.h:2:9",
                  commentHeader = Just "unions.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "dim2_y",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:3:9",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "dim2_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "y",
                  commentLocation = Just
                    "unions.h:3:9",
                  commentHeader = Just "unions.h",
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "unions.h:1:8",
                declId = NamePair {
                  nameC = Name "Dim2",
                  nameHsIdent = HsIdentifier
                    "Dim2"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "unions.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Dim2"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:2:9",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "dim2_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:3:9",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = HsIdentifier
                            "dim2_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 32,
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
              commentOrigin = Just "Dim2",
              commentLocation = Just
                "unions.h:1:8",
              commentHeader = Just "unions.h",
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 8,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Dim2",
                  structConstr = HsName
                    "@NsConstr"
                    "Dim2",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "dim2_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:2:9",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "dim2_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "unions.h:2:9",
                          commentHeader = Just "unions.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "dim2_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:3:9",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "dim2_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "unions.h:3:9",
                          commentHeader = Just "unions.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:1:8",
                        declId = NamePair {
                          nameC = Name "Dim2",
                          nameHsIdent = HsIdentifier
                            "Dim2"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "unions.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Dim2"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:2:9",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "dim2_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:3:9",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "dim2_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
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
                      commentOrigin = Just "Dim2",
                      commentLocation = Just
                        "unions.h:1:8",
                      commentHeader = Just "unions.h",
                      commentChildren = []}})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 4]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Dim2",
                  structConstr = HsName
                    "@NsConstr"
                    "Dim2",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "dim2_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:2:9",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "dim2_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "unions.h:2:9",
                          commentHeader = Just "unions.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "dim2_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:3:9",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "dim2_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "unions.h:3:9",
                          commentHeader = Just "unions.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:1:8",
                        declId = NamePair {
                          nameC = Name "Dim2",
                          nameHsIdent = HsIdentifier
                            "Dim2"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "unions.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Dim2"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:2:9",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "dim2_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:3:9",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "dim2_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
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
                      commentOrigin = Just "Dim2",
                      commentLocation = Just
                        "unions.h:1:8",
                      commentHeader = Just "unions.h",
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      4
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
        "Dim2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Dim2",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Dim3",
      structConstr = HsName
        "@NsConstr"
        "Dim3",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "dim3_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:7:9",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier
                    "dim3_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Just
                "unions.h:7:9",
              commentHeader = Just "unions.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "dim3_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:8:9",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier
                    "dim3_y"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Just
                "unions.h:8:9",
              commentHeader = Just "unions.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "dim3_z",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:9:9",
                fieldName = NamePair {
                  nameC = Name "z",
                  nameHsIdent = HsIdentifier
                    "dim3_z"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "z",
              commentLocation = Just
                "unions.h:9:9",
              commentHeader = Just "unions.h",
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "unions.h:6:8",
            declId = NamePair {
              nameC = Name "Dim3",
              nameHsIdent = HsIdentifier
                "Dim3"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "unions.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Dim3"),
              structSizeof = 12,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:7:9",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "dim3_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:8:9",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "dim3_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:9:9",
                    fieldName = NamePair {
                      nameC = Name "z",
                      nameHsIdent = HsIdentifier
                        "dim3_z"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
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
          commentOrigin = Just "Dim3",
          commentLocation = Just
            "unions.h:6:8",
          commentHeader = Just "unions.h",
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Dim3",
          structConstr = HsName
            "@NsConstr"
            "Dim3",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "dim3_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:7:9",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "dim3_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "x",
                  commentLocation = Just
                    "unions.h:7:9",
                  commentHeader = Just "unions.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "dim3_y",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:8:9",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "dim3_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "y",
                  commentLocation = Just
                    "unions.h:8:9",
                  commentHeader = Just "unions.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "dim3_z",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:9:9",
                    fieldName = NamePair {
                      nameC = Name "z",
                      nameHsIdent = HsIdentifier
                        "dim3_z"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "z",
                  commentLocation = Just
                    "unions.h:9:9",
                  commentHeader = Just "unions.h",
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "unions.h:6:8",
                declId = NamePair {
                  nameC = Name "Dim3",
                  nameHsIdent = HsIdentifier
                    "Dim3"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "unions.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Dim3"),
                  structSizeof = 12,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:7:9",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "dim3_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:8:9",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = HsIdentifier
                            "dim3_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:9:9",
                        fieldName = NamePair {
                          nameC = Name "z",
                          nameHsIdent = HsIdentifier
                            "dim3_z"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
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
              commentOrigin = Just "Dim3",
              commentLocation = Just
                "unions.h:6:8",
              commentHeader = Just "unions.h",
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 12,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Dim3",
                  structConstr = HsName
                    "@NsConstr"
                    "Dim3",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "dim3_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:7:9",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "dim3_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "unions.h:7:9",
                          commentHeader = Just "unions.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "dim3_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:8:9",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "dim3_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "unions.h:8:9",
                          commentHeader = Just "unions.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "dim3_z",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:9:9",
                            fieldName = NamePair {
                              nameC = Name "z",
                              nameHsIdent = HsIdentifier
                                "dim3_z"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "z",
                          commentLocation = Just
                            "unions.h:9:9",
                          commentHeader = Just "unions.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:6:8",
                        declId = NamePair {
                          nameC = Name "Dim3",
                          nameHsIdent = HsIdentifier
                            "Dim3"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "unions.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Dim3"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:7:9",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "dim3_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:8:9",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "dim3_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:9:9",
                                fieldName = NamePair {
                                  nameC = Name "z",
                                  nameHsIdent = HsIdentifier
                                    "dim3_z"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
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
                      commentOrigin = Just "Dim3",
                      commentLocation = Just
                        "unions.h:6:8",
                      commentHeader = Just "unions.h",
                      commentChildren = []}})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 4,
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
                    "Dim3",
                  structConstr = HsName
                    "@NsConstr"
                    "Dim3",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "dim3_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:7:9",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "dim3_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "unions.h:7:9",
                          commentHeader = Just "unions.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "dim3_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:8:9",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "dim3_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "unions.h:8:9",
                          commentHeader = Just "unions.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "dim3_z",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:9:9",
                            fieldName = NamePair {
                              nameC = Name "z",
                              nameHsIdent = HsIdentifier
                                "dim3_z"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "z",
                          commentLocation = Just
                            "unions.h:9:9",
                          commentHeader = Just "unions.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:6:8",
                        declId = NamePair {
                          nameC = Name "Dim3",
                          nameHsIdent = HsIdentifier
                            "Dim3"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "unions.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Dim3"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:7:9",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "dim3_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:8:9",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "dim3_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:9:9",
                                fieldName = NamePair {
                                  nameC = Name "z",
                                  nameHsIdent = HsIdentifier
                                    "dim3_z"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
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
                      commentOrigin = Just "Dim3",
                      commentLocation = Just
                        "unions.h:6:8",
                      commentHeader = Just "unions.h",
                      commentChildren = []}}
                (Add 3)
                (Seq
                  [
                    PokeByteOff (Idx 4) 0 (Idx 0),
                    PokeByteOff (Idx 4) 4 (Idx 1),
                    PokeByteOff
                      (Idx 4)
                      8
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
        "Dim3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Dim3",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "DimPayload",
      newtypeConstr = HsName
        "@NsConstr"
        "DimPayload",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_DimPayload",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "unions.h:12:7",
          declId = NamePair {
            nameC = Name "DimPayload",
            nameHsIdent = HsIdentifier
              "DimPayload"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "unions.h",
          declComment = Nothing},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "DimPayload",
              newtypeField = HsName
                "@NsVar"
                "un_DimPayload"},
            unionSizeof = 8,
            unionAlignment = 4,
            unionFields = [
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc = "unions.h:13:17",
                  fieldName = NamePair {
                    nameC = Name "dim2",
                    nameHsIdent = HsIdentifier
                      "dimPayload_dim2"},
                  fieldComment = Nothing},
                unionFieldType = TypeStruct
                  NamePair {
                    nameC = Name "Dim2",
                    nameHsIdent = HsIdentifier
                      "Dim2"}
                  NameOriginInSource},
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc = "unions.h:14:17",
                  fieldName = NamePair {
                    nameC = Name "dim3",
                    nameHsIdent = HsIdentifier
                      "dimPayload_dim3"},
                  fieldComment = Nothing},
                unionFieldType = TypeStruct
                  NamePair {
                    nameC = Name "Dim2",
                    nameHsIdent = HsIdentifier
                      "Dim2"}
                  NameOriginInSource}]},
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
            "DimPayload",
          commentLocation = Just
            "unions.h:12:7",
          commentHeader = Just "unions.h",
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveVia
        (HsSizedByteArray 8 4),
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "DimPayload",
      deriveInstanceComment =
      Nothing},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = HsName
        "@NsVar"
        "get_dimPayload_dim2",
      unionGetterType = HsTypRef
        (HsName "@NsTypeConstr" "Dim2"),
      unionGetterConstr = HsName
        "@NsTypeConstr"
        "DimPayload",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "dim2",
          commentLocation = Just
            "unions.h:13:17",
          commentHeader = Just "unions.h",
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_dimPayload_dim2"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = HsName
        "@NsVar"
        "set_dimPayload_dim2",
      unionSetterType = HsTypRef
        (HsName "@NsTypeConstr" "Dim2"),
      unionSetterConstr = HsName
        "@NsTypeConstr"
        "DimPayload",
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
                  "get_dimPayload_dim2"]]}},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = HsName
        "@NsVar"
        "get_dimPayload_dim3",
      unionGetterType = HsTypRef
        (HsName "@NsTypeConstr" "Dim2"),
      unionGetterConstr = HsName
        "@NsTypeConstr"
        "DimPayload",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "dim3",
          commentLocation = Just
            "unions.h:14:17",
          commentHeader = Just "unions.h",
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_dimPayload_dim3"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = HsName
        "@NsVar"
        "set_dimPayload_dim3",
      unionSetterType = HsTypRef
        (HsName "@NsTypeConstr" "Dim2"),
      unionSetterConstr = HsName
        "@NsTypeConstr"
        "DimPayload",
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
                  "get_dimPayload_dim3"]]}},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Dim",
      structConstr = HsName
        "@NsConstr"
        "Dim",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "dim_tag",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:18:9",
                fieldName = NamePair {
                  nameC = Name "tag",
                  nameHsIdent = HsIdentifier
                    "dim_tag"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "tag",
              commentLocation = Just
                "unions.h:18:9",
              commentHeader = Just "unions.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "dim_payload",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "DimPayload"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:19:22",
                fieldName = NamePair {
                  nameC = Name "payload",
                  nameHsIdent = HsIdentifier
                    "dim_payload"},
                fieldComment = Nothing},
              structFieldType = TypeUnion
                NamePair {
                  nameC = Name "DimPayload",
                  nameHsIdent = HsIdentifier
                    "DimPayload"}
                NameOriginInSource,
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "payload",
              commentLocation = Just
                "unions.h:19:22",
              commentHeader = Just "unions.h",
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "unions.h:17:8",
            declId = NamePair {
              nameC = Name "Dim",
              nameHsIdent = HsIdentifier
                "Dim"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "unions.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Dim"),
              structSizeof = 12,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:18:9",
                    fieldName = NamePair {
                      nameC = Name "tag",
                      nameHsIdent = HsIdentifier
                        "dim_tag"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:19:22",
                    fieldName = NamePair {
                      nameC = Name "payload",
                      nameHsIdent = HsIdentifier
                        "dim_payload"},
                    fieldComment = Nothing},
                  structFieldType = TypeUnion
                    NamePair {
                      nameC = Name "DimPayload",
                      nameHsIdent = HsIdentifier
                        "DimPayload"}
                    NameOriginInSource,
                  structFieldOffset = 32,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            TypeSpec {
              typeSpecModule = Nothing,
              typeSpecIdentifier = Nothing,
              typeSpecInstances = Map.fromList
                []}},
      structInstances = Set.fromList
        [Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "Dim",
          commentLocation = Just
            "unions.h:17:8",
          commentHeader = Just "unions.h",
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Dim",
          structConstr = HsName
            "@NsConstr"
            "Dim",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "dim_tag",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:18:9",
                    fieldName = NamePair {
                      nameC = Name "tag",
                      nameHsIdent = HsIdentifier
                        "dim_tag"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "tag",
                  commentLocation = Just
                    "unions.h:18:9",
                  commentHeader = Just "unions.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "dim_payload",
              fieldType = HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "DimPayload"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:19:22",
                    fieldName = NamePair {
                      nameC = Name "payload",
                      nameHsIdent = HsIdentifier
                        "dim_payload"},
                    fieldComment = Nothing},
                  structFieldType = TypeUnion
                    NamePair {
                      nameC = Name "DimPayload",
                      nameHsIdent = HsIdentifier
                        "DimPayload"}
                    NameOriginInSource,
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "payload",
                  commentLocation = Just
                    "unions.h:19:22",
                  commentHeader = Just "unions.h",
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "unions.h:17:8",
                declId = NamePair {
                  nameC = Name "Dim",
                  nameHsIdent = HsIdentifier
                    "Dim"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "unions.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Dim"),
                  structSizeof = 12,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:18:9",
                        fieldName = NamePair {
                          nameC = Name "tag",
                          nameHsIdent = HsIdentifier
                            "dim_tag"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:19:22",
                        fieldName = NamePair {
                          nameC = Name "payload",
                          nameHsIdent = HsIdentifier
                            "dim_payload"},
                        fieldComment = Nothing},
                      structFieldType = TypeUnion
                        NamePair {
                          nameC = Name "DimPayload",
                          nameHsIdent = HsIdentifier
                            "DimPayload"}
                        NameOriginInSource,
                      structFieldOffset = 32,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                TypeSpec {
                  typeSpecModule = Nothing,
                  typeSpecIdentifier = Nothing,
                  typeSpecInstances = Map.fromList
                    []}},
          structInstances = Set.fromList
            [Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "Dim",
              commentLocation = Just
                "unions.h:17:8",
              commentHeader = Just "unions.h",
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 12,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Dim",
                  structConstr = HsName
                    "@NsConstr"
                    "Dim",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "dim_tag",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:18:9",
                            fieldName = NamePair {
                              nameC = Name "tag",
                              nameHsIdent = HsIdentifier
                                "dim_tag"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "tag",
                          commentLocation = Just
                            "unions.h:18:9",
                          commentHeader = Just "unions.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "dim_payload",
                      fieldType = HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "DimPayload"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:19:22",
                            fieldName = NamePair {
                              nameC = Name "payload",
                              nameHsIdent = HsIdentifier
                                "dim_payload"},
                            fieldComment = Nothing},
                          structFieldType = TypeUnion
                            NamePair {
                              nameC = Name "DimPayload",
                              nameHsIdent = HsIdentifier
                                "DimPayload"}
                            NameOriginInSource,
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "payload",
                          commentLocation = Just
                            "unions.h:19:22",
                          commentHeader = Just "unions.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:17:8",
                        declId = NamePair {
                          nameC = Name "Dim",
                          nameHsIdent = HsIdentifier
                            "Dim"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "unions.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Dim"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:18:9",
                                fieldName = NamePair {
                                  nameC = Name "tag",
                                  nameHsIdent = HsIdentifier
                                    "dim_tag"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:19:22",
                                fieldName = NamePair {
                                  nameC = Name "payload",
                                  nameHsIdent = HsIdentifier
                                    "dim_payload"},
                                fieldComment = Nothing},
                              structFieldType = TypeUnion
                                NamePair {
                                  nameC = Name "DimPayload",
                                  nameHsIdent = HsIdentifier
                                    "DimPayload"}
                                NameOriginInSource,
                              structFieldOffset = 32,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "Dim",
                      commentLocation = Just
                        "unions.h:17:8",
                      commentHeader = Just "unions.h",
                      commentChildren = []}})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 4]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Dim",
                  structConstr = HsName
                    "@NsConstr"
                    "Dim",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "dim_tag",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:18:9",
                            fieldName = NamePair {
                              nameC = Name "tag",
                              nameHsIdent = HsIdentifier
                                "dim_tag"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "tag",
                          commentLocation = Just
                            "unions.h:18:9",
                          commentHeader = Just "unions.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "dim_payload",
                      fieldType = HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "DimPayload"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:19:22",
                            fieldName = NamePair {
                              nameC = Name "payload",
                              nameHsIdent = HsIdentifier
                                "dim_payload"},
                            fieldComment = Nothing},
                          structFieldType = TypeUnion
                            NamePair {
                              nameC = Name "DimPayload",
                              nameHsIdent = HsIdentifier
                                "DimPayload"}
                            NameOriginInSource,
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "payload",
                          commentLocation = Just
                            "unions.h:19:22",
                          commentHeader = Just "unions.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:17:8",
                        declId = NamePair {
                          nameC = Name "Dim",
                          nameHsIdent = HsIdentifier
                            "Dim"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "unions.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Dim"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:18:9",
                                fieldName = NamePair {
                                  nameC = Name "tag",
                                  nameHsIdent = HsIdentifier
                                    "dim_tag"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:19:22",
                                fieldName = NamePair {
                                  nameC = Name "payload",
                                  nameHsIdent = HsIdentifier
                                    "dim_payload"},
                                fieldComment = Nothing},
                              structFieldType = TypeUnion
                                NamePair {
                                  nameC = Name "DimPayload",
                                  nameHsIdent = HsIdentifier
                                    "DimPayload"}
                                NameOriginInSource,
                              structFieldOffset = 32,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "Dim",
                      commentLocation = Just
                        "unions.h:17:8",
                      commentHeader = Just "unions.h",
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      4
                      (Idx 1)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "DimPayloadB",
      newtypeConstr = HsName
        "@NsConstr"
        "DimPayloadB",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_DimPayloadB",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "unions.h:23:15",
          declId = NamePair {
            nameC = Name "DimPayloadB",
            nameHsIdent = HsIdentifier
              "DimPayloadB"},
          declOrigin = NameOriginInSource,
          declAliases = [
            Name "DimPayloadB"],
          declHeader = "unions.h",
          declComment = Nothing},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "DimPayloadB",
              newtypeField = HsName
                "@NsVar"
                "un_DimPayloadB"},
            unionSizeof = 8,
            unionAlignment = 4,
            unionFields = [
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc = "unions.h:24:17",
                  fieldName = NamePair {
                    nameC = Name "dim2",
                    nameHsIdent = HsIdentifier
                      "dimPayloadB_dim2"},
                  fieldComment = Nothing},
                unionFieldType = TypeStruct
                  NamePair {
                    nameC = Name "Dim2",
                    nameHsIdent = HsIdentifier
                      "Dim2"}
                  NameOriginInSource},
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc = "unions.h:25:17",
                  fieldName = NamePair {
                    nameC = Name "dim3",
                    nameHsIdent = HsIdentifier
                      "dimPayloadB_dim3"},
                  fieldComment = Nothing},
                unionFieldType = TypeStruct
                  NamePair {
                    nameC = Name "Dim2",
                    nameHsIdent = HsIdentifier
                      "Dim2"}
                  NameOriginInSource}]},
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
            "DimPayloadB",
          commentLocation = Just
            "unions.h:23:15",
          commentHeader = Just "unions.h",
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveVia
        (HsSizedByteArray 8 4),
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "DimPayloadB",
      deriveInstanceComment =
      Nothing},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = HsName
        "@NsVar"
        "get_dimPayloadB_dim2",
      unionGetterType = HsTypRef
        (HsName "@NsTypeConstr" "Dim2"),
      unionGetterConstr = HsName
        "@NsTypeConstr"
        "DimPayloadB",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "dim2",
          commentLocation = Just
            "unions.h:24:17",
          commentHeader = Just "unions.h",
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_dimPayloadB_dim2"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = HsName
        "@NsVar"
        "set_dimPayloadB_dim2",
      unionSetterType = HsTypRef
        (HsName "@NsTypeConstr" "Dim2"),
      unionSetterConstr = HsName
        "@NsTypeConstr"
        "DimPayloadB",
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
                  "get_dimPayloadB_dim2"]]}},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = HsName
        "@NsVar"
        "get_dimPayloadB_dim3",
      unionGetterType = HsTypRef
        (HsName "@NsTypeConstr" "Dim2"),
      unionGetterConstr = HsName
        "@NsTypeConstr"
        "DimPayloadB",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "dim3",
          commentLocation = Just
            "unions.h:25:17",
          commentHeader = Just "unions.h",
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_dimPayloadB_dim3"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = HsName
        "@NsVar"
        "set_dimPayloadB_dim3",
      unionSetterType = HsTypRef
        (HsName "@NsTypeConstr" "Dim2"),
      unionSetterConstr = HsName
        "@NsTypeConstr"
        "DimPayloadB",
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
                  "get_dimPayloadB_dim3"]]}},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "DimB",
      structConstr = HsName
        "@NsConstr"
        "DimB",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "dimB_tag",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:29:9",
                fieldName = NamePair {
                  nameC = Name "tag",
                  nameHsIdent = HsIdentifier
                    "dimB_tag"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "tag",
              commentLocation = Just
                "unions.h:29:9",
              commentHeader = Just "unions.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "dimB_payload",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "DimPayloadB"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:30:17",
                fieldName = NamePair {
                  nameC = Name "payload",
                  nameHsIdent = HsIdentifier
                    "dimB_payload"},
                fieldComment = Nothing},
              structFieldType = TypeTypedef
                (TypedefSquashed
                  (Name "DimPayloadB")
                  (TypeUnion
                    NamePair {
                      nameC = Name "DimPayloadB",
                      nameHsIdent = HsIdentifier
                        "DimPayloadB"}
                    NameOriginInSource)),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "payload",
              commentLocation = Just
                "unions.h:30:17",
              commentHeader = Just "unions.h",
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "unions.h:28:8",
            declId = NamePair {
              nameC = Name "DimB",
              nameHsIdent = HsIdentifier
                "DimB"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "unions.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "DimB"),
              structSizeof = 12,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:29:9",
                    fieldName = NamePair {
                      nameC = Name "tag",
                      nameHsIdent = HsIdentifier
                        "dimB_tag"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:30:17",
                    fieldName = NamePair {
                      nameC = Name "payload",
                      nameHsIdent = HsIdentifier
                        "dimB_payload"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name "DimPayloadB")
                      (TypeUnion
                        NamePair {
                          nameC = Name "DimPayloadB",
                          nameHsIdent = HsIdentifier
                            "DimPayloadB"}
                        NameOriginInSource)),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            TypeSpec {
              typeSpecModule = Nothing,
              typeSpecIdentifier = Nothing,
              typeSpecInstances = Map.fromList
                []}},
      structInstances = Set.fromList
        [Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "DimB",
          commentLocation = Just
            "unions.h:28:8",
          commentHeader = Just "unions.h",
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "DimB",
          structConstr = HsName
            "@NsConstr"
            "DimB",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "dimB_tag",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:29:9",
                    fieldName = NamePair {
                      nameC = Name "tag",
                      nameHsIdent = HsIdentifier
                        "dimB_tag"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "tag",
                  commentLocation = Just
                    "unions.h:29:9",
                  commentHeader = Just "unions.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "dimB_payload",
              fieldType = HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "DimPayloadB"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:30:17",
                    fieldName = NamePair {
                      nameC = Name "payload",
                      nameHsIdent = HsIdentifier
                        "dimB_payload"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name "DimPayloadB")
                      (TypeUnion
                        NamePair {
                          nameC = Name "DimPayloadB",
                          nameHsIdent = HsIdentifier
                            "DimPayloadB"}
                        NameOriginInSource)),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "payload",
                  commentLocation = Just
                    "unions.h:30:17",
                  commentHeader = Just "unions.h",
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "unions.h:28:8",
                declId = NamePair {
                  nameC = Name "DimB",
                  nameHsIdent = HsIdentifier
                    "DimB"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "unions.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "DimB"),
                  structSizeof = 12,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:29:9",
                        fieldName = NamePair {
                          nameC = Name "tag",
                          nameHsIdent = HsIdentifier
                            "dimB_tag"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:30:17",
                        fieldName = NamePair {
                          nameC = Name "payload",
                          nameHsIdent = HsIdentifier
                            "dimB_payload"},
                        fieldComment = Nothing},
                      structFieldType = TypeTypedef
                        (TypedefSquashed
                          (Name "DimPayloadB")
                          (TypeUnion
                            NamePair {
                              nameC = Name "DimPayloadB",
                              nameHsIdent = HsIdentifier
                                "DimPayloadB"}
                            NameOriginInSource)),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                TypeSpec {
                  typeSpecModule = Nothing,
                  typeSpecIdentifier = Nothing,
                  typeSpecInstances = Map.fromList
                    []}},
          structInstances = Set.fromList
            [Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "DimB",
              commentLocation = Just
                "unions.h:28:8",
              commentHeader = Just "unions.h",
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 12,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "DimB",
                  structConstr = HsName
                    "@NsConstr"
                    "DimB",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "dimB_tag",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:29:9",
                            fieldName = NamePair {
                              nameC = Name "tag",
                              nameHsIdent = HsIdentifier
                                "dimB_tag"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "tag",
                          commentLocation = Just
                            "unions.h:29:9",
                          commentHeader = Just "unions.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "dimB_payload",
                      fieldType = HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "DimPayloadB"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:30:17",
                            fieldName = NamePair {
                              nameC = Name "payload",
                              nameHsIdent = HsIdentifier
                                "dimB_payload"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefSquashed
                              (Name "DimPayloadB")
                              (TypeUnion
                                NamePair {
                                  nameC = Name "DimPayloadB",
                                  nameHsIdent = HsIdentifier
                                    "DimPayloadB"}
                                NameOriginInSource)),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "payload",
                          commentLocation = Just
                            "unions.h:30:17",
                          commentHeader = Just "unions.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:28:8",
                        declId = NamePair {
                          nameC = Name "DimB",
                          nameHsIdent = HsIdentifier
                            "DimB"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "unions.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "DimB"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:29:9",
                                fieldName = NamePair {
                                  nameC = Name "tag",
                                  nameHsIdent = HsIdentifier
                                    "dimB_tag"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:30:17",
                                fieldName = NamePair {
                                  nameC = Name "payload",
                                  nameHsIdent = HsIdentifier
                                    "dimB_payload"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefSquashed
                                  (Name "DimPayloadB")
                                  (TypeUnion
                                    NamePair {
                                      nameC = Name "DimPayloadB",
                                      nameHsIdent = HsIdentifier
                                        "DimPayloadB"}
                                    NameOriginInSource)),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "DimB",
                      commentLocation = Just
                        "unions.h:28:8",
                      commentHeader = Just "unions.h",
                      commentChildren = []}})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 4]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "DimB",
                  structConstr = HsName
                    "@NsConstr"
                    "DimB",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "dimB_tag",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:29:9",
                            fieldName = NamePair {
                              nameC = Name "tag",
                              nameHsIdent = HsIdentifier
                                "dimB_tag"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "tag",
                          commentLocation = Just
                            "unions.h:29:9",
                          commentHeader = Just "unions.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "dimB_payload",
                      fieldType = HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "DimPayloadB"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:30:17",
                            fieldName = NamePair {
                              nameC = Name "payload",
                              nameHsIdent = HsIdentifier
                                "dimB_payload"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefSquashed
                              (Name "DimPayloadB")
                              (TypeUnion
                                NamePair {
                                  nameC = Name "DimPayloadB",
                                  nameHsIdent = HsIdentifier
                                    "DimPayloadB"}
                                NameOriginInSource)),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "payload",
                          commentLocation = Just
                            "unions.h:30:17",
                          commentHeader = Just "unions.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:28:8",
                        declId = NamePair {
                          nameC = Name "DimB",
                          nameHsIdent = HsIdentifier
                            "DimB"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "unions.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "DimB"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:29:9",
                                fieldName = NamePair {
                                  nameC = Name "tag",
                                  nameHsIdent = HsIdentifier
                                    "dimB_tag"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:30:17",
                                fieldName = NamePair {
                                  nameC = Name "payload",
                                  nameHsIdent = HsIdentifier
                                    "dimB_payload"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefSquashed
                                  (Name "DimPayloadB")
                                  (TypeUnion
                                    NamePair {
                                      nameC = Name "DimPayloadB",
                                      nameHsIdent = HsIdentifier
                                        "DimPayloadB"}
                                    NameOriginInSource)),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "DimB",
                      commentLocation = Just
                        "unions.h:28:8",
                      commentHeader = Just "unions.h",
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      4
                      (Idx 1)])))},
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "AnonA_xy",
      structConstr = HsName
        "@NsConstr"
        "AnonA_xy",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "anonA_xy_x",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:35:21",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier
                    "anonA_xy_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Just
                "unions.h:35:21",
              commentHeader = Just "unions.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "anonA_xy_y",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:35:31",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier
                    "anonA_xy_y"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Just
                "unions.h:35:31",
              commentHeader = Just "unions.h",
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "unions.h:35:5",
            declId = NamePair {
              nameC = Name "AnonA_xy",
              nameHsIdent = HsIdentifier
                "AnonA_xy"},
            declOrigin = NameOriginGenerated
              (AnonId "unions.h:35:5"),
            declAliases = [],
            declHeader = "unions.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "AnonA_xy"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:35:21",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "anonA_xy_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:35:31",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "anonA_xy_y"},
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
          commentOrigin = Just "AnonA_xy",
          commentLocation = Just
            "unions.h:35:5",
          commentHeader = Just "unions.h",
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "AnonA_xy",
          structConstr = HsName
            "@NsConstr"
            "AnonA_xy",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "anonA_xy_x",
              fieldType = HsPrimType
                HsPrimCDouble,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:35:21",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "anonA_xy_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "x",
                  commentLocation = Just
                    "unions.h:35:21",
                  commentHeader = Just "unions.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "anonA_xy_y",
              fieldType = HsPrimType
                HsPrimCDouble,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:35:31",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "anonA_xy_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "y",
                  commentLocation = Just
                    "unions.h:35:31",
                  commentHeader = Just "unions.h",
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "unions.h:35:5",
                declId = NamePair {
                  nameC = Name "AnonA_xy",
                  nameHsIdent = HsIdentifier
                    "AnonA_xy"},
                declOrigin = NameOriginGenerated
                  (AnonId "unions.h:35:5"),
                declAliases = [],
                declHeader = "unions.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "AnonA_xy"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:35:21",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "anonA_xy_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimFloating PrimDouble),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:35:31",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = HsIdentifier
                            "anonA_xy_y"},
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
              commentOrigin = Just "AnonA_xy",
              commentLocation = Just
                "unions.h:35:5",
              commentHeader = Just "unions.h",
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
                    "AnonA_xy",
                  structConstr = HsName
                    "@NsConstr"
                    "AnonA_xy",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "anonA_xy_x",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:35:21",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "anonA_xy_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "unions.h:35:21",
                          commentHeader = Just "unions.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "anonA_xy_y",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:35:31",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "anonA_xy_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "unions.h:35:31",
                          commentHeader = Just "unions.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:35:5",
                        declId = NamePair {
                          nameC = Name "AnonA_xy",
                          nameHsIdent = HsIdentifier
                            "AnonA_xy"},
                        declOrigin = NameOriginGenerated
                          (AnonId "unions.h:35:5"),
                        declAliases = [],
                        declHeader = "unions.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "AnonA_xy"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:35:21",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "anonA_xy_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:35:31",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "anonA_xy_y"},
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
                      commentOrigin = Just "AnonA_xy",
                      commentLocation = Just
                        "unions.h:35:5",
                      commentHeader = Just "unions.h",
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
                    "AnonA_xy",
                  structConstr = HsName
                    "@NsConstr"
                    "AnonA_xy",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "anonA_xy_x",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:35:21",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "anonA_xy_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "unions.h:35:21",
                          commentHeader = Just "unions.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "anonA_xy_y",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:35:31",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "anonA_xy_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "unions.h:35:31",
                          commentHeader = Just "unions.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:35:5",
                        declId = NamePair {
                          nameC = Name "AnonA_xy",
                          nameHsIdent = HsIdentifier
                            "AnonA_xy"},
                        declOrigin = NameOriginGenerated
                          (AnonId "unions.h:35:5"),
                        declAliases = [],
                        declHeader = "unions.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "AnonA_xy"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:35:21",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "anonA_xy_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:35:31",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "anonA_xy_y"},
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
                      commentOrigin = Just "AnonA_xy",
                      commentLocation = Just
                        "unions.h:35:5",
                      commentHeader = Just "unions.h",
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
        "AnonA_xy",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "AnonA_xy",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "AnonA_polar",
      structConstr = HsName
        "@NsConstr"
        "AnonA_polar",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "anonA_polar_r",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:36:21",
                fieldName = NamePair {
                  nameC = Name "r",
                  nameHsIdent = HsIdentifier
                    "anonA_polar_r"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "r",
              commentLocation = Just
                "unions.h:36:21",
              commentHeader = Just "unions.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "anonA_polar_p",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "unions.h:36:31",
                fieldName = NamePair {
                  nameC = Name "p",
                  nameHsIdent = HsIdentifier
                    "anonA_polar_p"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "p",
              commentLocation = Just
                "unions.h:36:31",
              commentHeader = Just "unions.h",
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "unions.h:36:5",
            declId = NamePair {
              nameC = Name "AnonA_polar",
              nameHsIdent = HsIdentifier
                "AnonA_polar"},
            declOrigin = NameOriginGenerated
              (AnonId "unions.h:36:5"),
            declAliases = [],
            declHeader = "unions.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "AnonA_polar"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:36:21",
                    fieldName = NamePair {
                      nameC = Name "r",
                      nameHsIdent = HsIdentifier
                        "anonA_polar_r"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:36:31",
                    fieldName = NamePair {
                      nameC = Name "p",
                      nameHsIdent = HsIdentifier
                        "anonA_polar_p"},
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
            "AnonA_polar",
          commentLocation = Just
            "unions.h:36:5",
          commentHeader = Just "unions.h",
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "AnonA_polar",
          structConstr = HsName
            "@NsConstr"
            "AnonA_polar",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "anonA_polar_r",
              fieldType = HsPrimType
                HsPrimCDouble,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:36:21",
                    fieldName = NamePair {
                      nameC = Name "r",
                      nameHsIdent = HsIdentifier
                        "anonA_polar_r"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "r",
                  commentLocation = Just
                    "unions.h:36:21",
                  commentHeader = Just "unions.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "anonA_polar_p",
              fieldType = HsPrimType
                HsPrimCDouble,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "unions.h:36:31",
                    fieldName = NamePair {
                      nameC = Name "p",
                      nameHsIdent = HsIdentifier
                        "anonA_polar_p"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "p",
                  commentLocation = Just
                    "unions.h:36:31",
                  commentHeader = Just "unions.h",
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "unions.h:36:5",
                declId = NamePair {
                  nameC = Name "AnonA_polar",
                  nameHsIdent = HsIdentifier
                    "AnonA_polar"},
                declOrigin = NameOriginGenerated
                  (AnonId "unions.h:36:5"),
                declAliases = [],
                declHeader = "unions.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName
                      "@NsConstr"
                      "AnonA_polar"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:36:21",
                        fieldName = NamePair {
                          nameC = Name "r",
                          nameHsIdent = HsIdentifier
                            "anonA_polar_r"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimFloating PrimDouble),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "unions.h:36:31",
                        fieldName = NamePair {
                          nameC = Name "p",
                          nameHsIdent = HsIdentifier
                            "anonA_polar_p"},
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
                "AnonA_polar",
              commentLocation = Just
                "unions.h:36:5",
              commentHeader = Just "unions.h",
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
                    "AnonA_polar",
                  structConstr = HsName
                    "@NsConstr"
                    "AnonA_polar",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "anonA_polar_r",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:36:21",
                            fieldName = NamePair {
                              nameC = Name "r",
                              nameHsIdent = HsIdentifier
                                "anonA_polar_r"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "r",
                          commentLocation = Just
                            "unions.h:36:21",
                          commentHeader = Just "unions.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "anonA_polar_p",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:36:31",
                            fieldName = NamePair {
                              nameC = Name "p",
                              nameHsIdent = HsIdentifier
                                "anonA_polar_p"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "p",
                          commentLocation = Just
                            "unions.h:36:31",
                          commentHeader = Just "unions.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:36:5",
                        declId = NamePair {
                          nameC = Name "AnonA_polar",
                          nameHsIdent = HsIdentifier
                            "AnonA_polar"},
                        declOrigin = NameOriginGenerated
                          (AnonId "unions.h:36:5"),
                        declAliases = [],
                        declHeader = "unions.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "AnonA_polar"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:36:21",
                                fieldName = NamePair {
                                  nameC = Name "r",
                                  nameHsIdent = HsIdentifier
                                    "anonA_polar_r"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:36:31",
                                fieldName = NamePair {
                                  nameC = Name "p",
                                  nameHsIdent = HsIdentifier
                                    "anonA_polar_p"},
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
                        "AnonA_polar",
                      commentLocation = Just
                        "unions.h:36:5",
                      commentHeader = Just "unions.h",
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
                    "AnonA_polar",
                  structConstr = HsName
                    "@NsConstr"
                    "AnonA_polar",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "anonA_polar_r",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:36:21",
                            fieldName = NamePair {
                              nameC = Name "r",
                              nameHsIdent = HsIdentifier
                                "anonA_polar_r"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "r",
                          commentLocation = Just
                            "unions.h:36:21",
                          commentHeader = Just "unions.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "anonA_polar_p",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "unions.h:36:31",
                            fieldName = NamePair {
                              nameC = Name "p",
                              nameHsIdent = HsIdentifier
                                "anonA_polar_p"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "p",
                          commentLocation = Just
                            "unions.h:36:31",
                          commentHeader = Just "unions.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "unions.h:36:5",
                        declId = NamePair {
                          nameC = Name "AnonA_polar",
                          nameHsIdent = HsIdentifier
                            "AnonA_polar"},
                        declOrigin = NameOriginGenerated
                          (AnonId "unions.h:36:5"),
                        declAliases = [],
                        declHeader = "unions.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "AnonA_polar"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:36:21",
                                fieldName = NamePair {
                                  nameC = Name "r",
                                  nameHsIdent = HsIdentifier
                                    "anonA_polar_r"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "unions.h:36:31",
                                fieldName = NamePair {
                                  nameC = Name "p",
                                  nameHsIdent = HsIdentifier
                                    "anonA_polar_p"},
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
                        "AnonA_polar",
                      commentLocation = Just
                        "unions.h:36:5",
                      commentHeader = Just "unions.h",
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
        "AnonA_polar",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "AnonA_polar",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "AnonA",
      newtypeConstr = HsName
        "@NsConstr"
        "AnonA",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_AnonA",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "unions.h:34:7",
          declId = NamePair {
            nameC = Name "AnonA",
            nameHsIdent = HsIdentifier
              "AnonA"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "unions.h",
          declComment = Nothing},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "AnonA",
              newtypeField = HsName
                "@NsVar"
                "un_AnonA"},
            unionSizeof = 16,
            unionAlignment = 8,
            unionFields = [
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc = "unions.h:35:36",
                  fieldName = NamePair {
                    nameC = Name "xy",
                    nameHsIdent = HsIdentifier
                      "anonA_xy"},
                  fieldComment = Nothing},
                unionFieldType = TypeStruct
                  NamePair {
                    nameC = Name "AnonA_xy",
                    nameHsIdent = HsIdentifier
                      "AnonA_xy"}
                  (NameOriginGenerated
                    (AnonId "unions.h:35:5"))},
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc = "unions.h:36:36",
                  fieldName = NamePair {
                    nameC = Name "polar",
                    nameHsIdent = HsIdentifier
                      "anonA_polar"},
                  fieldComment = Nothing},
                unionFieldType = TypeStruct
                  NamePair {
                    nameC = Name "AnonA_polar",
                    nameHsIdent = HsIdentifier
                      "AnonA_polar"}
                  (NameOriginGenerated
                    (AnonId "unions.h:36:5"))}]},
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
          commentOrigin = Just "AnonA",
          commentLocation = Just
            "unions.h:34:7",
          commentHeader = Just "unions.h",
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveVia
        (HsSizedByteArray 16 8),
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "AnonA",
      deriveInstanceComment =
      Nothing},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = HsName
        "@NsVar"
        "get_anonA_xy",
      unionGetterType = HsTypRef
        (HsName
          "@NsTypeConstr"
          "AnonA_xy"),
      unionGetterConstr = HsName
        "@NsTypeConstr"
        "AnonA",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "xy",
          commentLocation = Just
            "unions.h:35:36",
          commentHeader = Just "unions.h",
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier "set_anonA_xy"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = HsName
        "@NsVar"
        "set_anonA_xy",
      unionSetterType = HsTypRef
        (HsName
          "@NsTypeConstr"
          "AnonA_xy"),
      unionSetterConstr = HsName
        "@NsTypeConstr"
        "AnonA",
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
                Identifier "get_anonA_xy"]]}},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = HsName
        "@NsVar"
        "get_anonA_polar",
      unionGetterType = HsTypRef
        (HsName
          "@NsTypeConstr"
          "AnonA_polar"),
      unionGetterConstr = HsName
        "@NsTypeConstr"
        "AnonA",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "polar",
          commentLocation = Just
            "unions.h:36:36",
          commentHeader = Just "unions.h",
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_anonA_polar"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = HsName
        "@NsVar"
        "set_anonA_polar",
      unionSetterType = HsTypRef
        (HsName
          "@NsTypeConstr"
          "AnonA_polar"),
      unionSetterConstr = HsName
        "@NsTypeConstr"
        "AnonA",
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
                  "get_anonA_polar"]]}}]
