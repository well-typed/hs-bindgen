[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Bools1",
      structConstr = HsName
        "@NsConstr"
        "Bools1",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "bools1_x",
          fieldType = HsPrimType
            HsPrimCBool,
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "bool.h:2:11",
              structFieldName = NamePair {
                nameC = Name "x",
                nameHsIdent = HsIdentifier
                  "bools1_x"},
              structFieldType = TypePrim
                PrimBool,
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing},
        Field {
          fieldName = HsName
            "@NsVar"
            "bools1_y",
          fieldType = HsPrimType
            HsPrimCBool,
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "bool.h:3:11",
              structFieldName = NamePair {
                nameC = Name "y",
                nameHsIdent = HsIdentifier
                  "bools1_y"},
              structFieldType = TypePrim
                PrimBool,
              structFieldOffset = 8,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "bool.h:1:8",
            declId = NamePair {
              nameC = Name "bools1",
              nameHsIdent = HsIdentifier
                "Bools1"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "bool.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Bools1"),
              structSizeof = 2,
              structAlignment = 1,
              structFields = [
                StructField {
                  structFieldLoc = "bool.h:2:11",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "bools1_x"},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
                StructField {
                  structFieldLoc = "bool.h:3:11",
                  structFieldName = NamePair {
                    nameC = Name "y",
                    nameHsIdent = HsIdentifier
                      "bools1_y"},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 8,
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
            "Bools1",
          structConstr = HsName
            "@NsConstr"
            "Bools1",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "bools1_x",
              fieldType = HsPrimType
                HsPrimCBool,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc = "bool.h:2:11",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "bools1_x"},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing},
            Field {
              fieldName = HsName
                "@NsVar"
                "bools1_y",
              fieldType = HsPrimType
                HsPrimCBool,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc = "bool.h:3:11",
                  structFieldName = NamePair {
                    nameC = Name "y",
                    nameHsIdent = HsIdentifier
                      "bools1_y"},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 8,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "bool.h:1:8",
                declId = NamePair {
                  nameC = Name "bools1",
                  nameHsIdent = HsIdentifier
                    "Bools1"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "bool.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Bools1"),
                  structSizeof = 2,
                  structAlignment = 1,
                  structFields = [
                    StructField {
                      structFieldLoc = "bool.h:2:11",
                      structFieldName = NamePair {
                        nameC = Name "x",
                        nameHsIdent = HsIdentifier
                          "bools1_x"},
                      structFieldType = TypePrim
                        PrimBool,
                      structFieldOffset = 0,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing},
                    StructField {
                      structFieldLoc = "bool.h:3:11",
                      structFieldName = NamePair {
                        nameC = Name "y",
                        nameHsIdent = HsIdentifier
                          "bools1_y"},
                      structFieldType = TypePrim
                        PrimBool,
                      structFieldOffset = 8,
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
          storableSizeOf = 2,
          storableAlignment = 1,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Bools1",
                  structConstr = HsName
                    "@NsConstr"
                    "Bools1",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bools1_x",
                      fieldType = HsPrimType
                        HsPrimCBool,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "bool.h:2:11",
                          structFieldName = NamePair {
                            nameC = Name "x",
                            nameHsIdent = HsIdentifier
                              "bools1_x"},
                          structFieldType = TypePrim
                            PrimBool,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bools1_y",
                      fieldType = HsPrimType
                        HsPrimCBool,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "bool.h:3:11",
                          structFieldName = NamePair {
                            nameC = Name "y",
                            nameHsIdent = HsIdentifier
                              "bools1_y"},
                          structFieldType = TypePrim
                            PrimBool,
                          structFieldOffset = 8,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bool.h:1:8",
                        declId = NamePair {
                          nameC = Name "bools1",
                          nameHsIdent = HsIdentifier
                            "Bools1"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "bool.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Bools1"),
                          structSizeof = 2,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldLoc = "bool.h:2:11",
                              structFieldName = NamePair {
                                nameC = Name "x",
                                nameHsIdent = HsIdentifier
                                  "bools1_x"},
                              structFieldType = TypePrim
                                PrimBool,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc = "bool.h:3:11",
                              structFieldName = NamePair {
                                nameC = Name "y",
                                nameHsIdent = HsIdentifier
                                  "bools1_y"},
                              structFieldType = TypePrim
                                PrimBool,
                              structFieldOffset = 8,
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
                PeekByteOff (Idx 0) 1]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Bools1",
                  structConstr = HsName
                    "@NsConstr"
                    "Bools1",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bools1_x",
                      fieldType = HsPrimType
                        HsPrimCBool,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "bool.h:2:11",
                          structFieldName = NamePair {
                            nameC = Name "x",
                            nameHsIdent = HsIdentifier
                              "bools1_x"},
                          structFieldType = TypePrim
                            PrimBool,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bools1_y",
                      fieldType = HsPrimType
                        HsPrimCBool,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "bool.h:3:11",
                          structFieldName = NamePair {
                            nameC = Name "y",
                            nameHsIdent = HsIdentifier
                              "bools1_y"},
                          structFieldType = TypePrim
                            PrimBool,
                          structFieldOffset = 8,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bool.h:1:8",
                        declId = NamePair {
                          nameC = Name "bools1",
                          nameHsIdent = HsIdentifier
                            "Bools1"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "bool.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Bools1"),
                          structSizeof = 2,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldLoc = "bool.h:2:11",
                              structFieldName = NamePair {
                                nameC = Name "x",
                                nameHsIdent = HsIdentifier
                                  "bools1_x"},
                              structFieldType = TypePrim
                                PrimBool,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc = "bool.h:3:11",
                              structFieldName = NamePair {
                                nameC = Name "y",
                                nameHsIdent = HsIdentifier
                                  "bools1_y"},
                              structFieldType = TypePrim
                                PrimBool,
                              structFieldOffset = 8,
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
                      1
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
        "Bools1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Bools1",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Bools2",
      structConstr = HsName
        "@NsConstr"
        "Bools2",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "bools2_x",
          fieldType = HsPrimType
            HsPrimCBool,
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "bool.h:9:10",
              structFieldName = NamePair {
                nameC = Name "x",
                nameHsIdent = HsIdentifier
                  "bools2_x"},
              structFieldType = TypePrim
                PrimBool,
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing},
        Field {
          fieldName = HsName
            "@NsVar"
            "bools2_y",
          fieldType = HsPrimType
            HsPrimCBool,
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "bool.h:10:10",
              structFieldName = NamePair {
                nameC = Name "y",
                nameHsIdent = HsIdentifier
                  "bools2_y"},
              structFieldType = TypePrim
                PrimBool,
              structFieldOffset = 8,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "bool.h:8:8",
            declId = NamePair {
              nameC = Name "bools2",
              nameHsIdent = HsIdentifier
                "Bools2"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "bool.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Bools2"),
              structSizeof = 2,
              structAlignment = 1,
              structFields = [
                StructField {
                  structFieldLoc = "bool.h:9:10",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "bools2_x"},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
                StructField {
                  structFieldLoc = "bool.h:10:10",
                  structFieldName = NamePair {
                    nameC = Name "y",
                    nameHsIdent = HsIdentifier
                      "bools2_y"},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 8,
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
            "Bools2",
          structConstr = HsName
            "@NsConstr"
            "Bools2",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "bools2_x",
              fieldType = HsPrimType
                HsPrimCBool,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc = "bool.h:9:10",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "bools2_x"},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing},
            Field {
              fieldName = HsName
                "@NsVar"
                "bools2_y",
              fieldType = HsPrimType
                HsPrimCBool,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc = "bool.h:10:10",
                  structFieldName = NamePair {
                    nameC = Name "y",
                    nameHsIdent = HsIdentifier
                      "bools2_y"},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 8,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "bool.h:8:8",
                declId = NamePair {
                  nameC = Name "bools2",
                  nameHsIdent = HsIdentifier
                    "Bools2"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "bool.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Bools2"),
                  structSizeof = 2,
                  structAlignment = 1,
                  structFields = [
                    StructField {
                      structFieldLoc = "bool.h:9:10",
                      structFieldName = NamePair {
                        nameC = Name "x",
                        nameHsIdent = HsIdentifier
                          "bools2_x"},
                      structFieldType = TypePrim
                        PrimBool,
                      structFieldOffset = 0,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing},
                    StructField {
                      structFieldLoc = "bool.h:10:10",
                      structFieldName = NamePair {
                        nameC = Name "y",
                        nameHsIdent = HsIdentifier
                          "bools2_y"},
                      structFieldType = TypePrim
                        PrimBool,
                      structFieldOffset = 8,
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
          storableSizeOf = 2,
          storableAlignment = 1,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Bools2",
                  structConstr = HsName
                    "@NsConstr"
                    "Bools2",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bools2_x",
                      fieldType = HsPrimType
                        HsPrimCBool,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "bool.h:9:10",
                          structFieldName = NamePair {
                            nameC = Name "x",
                            nameHsIdent = HsIdentifier
                              "bools2_x"},
                          structFieldType = TypePrim
                            PrimBool,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bools2_y",
                      fieldType = HsPrimType
                        HsPrimCBool,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "bool.h:10:10",
                          structFieldName = NamePair {
                            nameC = Name "y",
                            nameHsIdent = HsIdentifier
                              "bools2_y"},
                          structFieldType = TypePrim
                            PrimBool,
                          structFieldOffset = 8,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bool.h:8:8",
                        declId = NamePair {
                          nameC = Name "bools2",
                          nameHsIdent = HsIdentifier
                            "Bools2"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "bool.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Bools2"),
                          structSizeof = 2,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldLoc = "bool.h:9:10",
                              structFieldName = NamePair {
                                nameC = Name "x",
                                nameHsIdent = HsIdentifier
                                  "bools2_x"},
                              structFieldType = TypePrim
                                PrimBool,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc = "bool.h:10:10",
                              structFieldName = NamePair {
                                nameC = Name "y",
                                nameHsIdent = HsIdentifier
                                  "bools2_y"},
                              structFieldType = TypePrim
                                PrimBool,
                              structFieldOffset = 8,
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
                PeekByteOff (Idx 0) 1]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Bools2",
                  structConstr = HsName
                    "@NsConstr"
                    "Bools2",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bools2_x",
                      fieldType = HsPrimType
                        HsPrimCBool,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "bool.h:9:10",
                          structFieldName = NamePair {
                            nameC = Name "x",
                            nameHsIdent = HsIdentifier
                              "bools2_x"},
                          structFieldType = TypePrim
                            PrimBool,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bools2_y",
                      fieldType = HsPrimType
                        HsPrimCBool,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "bool.h:10:10",
                          structFieldName = NamePair {
                            nameC = Name "y",
                            nameHsIdent = HsIdentifier
                              "bools2_y"},
                          structFieldType = TypePrim
                            PrimBool,
                          structFieldOffset = 8,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bool.h:8:8",
                        declId = NamePair {
                          nameC = Name "bools2",
                          nameHsIdent = HsIdentifier
                            "Bools2"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "bool.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Bools2"),
                          structSizeof = 2,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldLoc = "bool.h:9:10",
                              structFieldName = NamePair {
                                nameC = Name "x",
                                nameHsIdent = HsIdentifier
                                  "bools2_x"},
                              structFieldType = TypePrim
                                PrimBool,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc = "bool.h:10:10",
                              structFieldName = NamePair {
                                nameC = Name "y",
                                nameHsIdent = HsIdentifier
                                  "bools2_y"},
                              structFieldType = TypePrim
                                PrimBool,
                              structFieldOffset = 8,
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
                      1
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
        "Bools2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Bools2",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "BOOL",
      newtypeConstr = HsName
        "@NsConstr"
        "BOOL",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_BOOL",
        fieldType = HsPrimType
          HsPrimCBool,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "bool.h:13:9",
          declId = NamePair {
            nameC = Name "BOOL",
            nameHsIdent = HsIdentifier
              "BOOL"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "bool.h",
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "BOOL",
              newtypeField = HsName
                "@NsVar"
                "un_BOOL"},
            macroType = TypePrim PrimBool},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [
          Eq,
          Ord,
          Enum,
          Ix,
          Bounded,
          Read,
          Show,
          Bits,
          FiniteBits,
          Integral,
          Num,
          Real,
          Storable],
      newtypeComment = Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass =
      FiniteBits,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Bools3",
      structConstr = HsName
        "@NsConstr"
        "Bools3",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "bools3_x",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "BOOL"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "bool.h:16:10",
              structFieldName = NamePair {
                nameC = Name "x",
                nameHsIdent = HsIdentifier
                  "bools3_x"},
              structFieldType =
              TypeMacroTypedef
                NamePair {
                  nameC = Name "BOOL",
                  nameHsIdent = HsIdentifier
                    "BOOL"}
                NameOriginInSource,
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing},
        Field {
          fieldName = HsName
            "@NsVar"
            "bools3_y",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "BOOL"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "bool.h:17:10",
              structFieldName = NamePair {
                nameC = Name "y",
                nameHsIdent = HsIdentifier
                  "bools3_y"},
              structFieldType =
              TypeMacroTypedef
                NamePair {
                  nameC = Name "BOOL",
                  nameHsIdent = HsIdentifier
                    "BOOL"}
                NameOriginInSource,
              structFieldOffset = 8,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "bool.h:15:8",
            declId = NamePair {
              nameC = Name "bools3",
              nameHsIdent = HsIdentifier
                "Bools3"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "bool.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Bools3"),
              structSizeof = 2,
              structAlignment = 1,
              structFields = [
                StructField {
                  structFieldLoc = "bool.h:16:10",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "bools3_x"},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = Name "BOOL",
                      nameHsIdent = HsIdentifier
                        "BOOL"}
                    NameOriginInSource,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
                StructField {
                  structFieldLoc = "bool.h:17:10",
                  structFieldName = NamePair {
                    nameC = Name "y",
                    nameHsIdent = HsIdentifier
                      "bools3_y"},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = Name "BOOL",
                      nameHsIdent = HsIdentifier
                        "BOOL"}
                    NameOriginInSource,
                  structFieldOffset = 8,
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
            "Bools3",
          structConstr = HsName
            "@NsConstr"
            "Bools3",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "bools3_x",
              fieldType = HsTypRef
                (HsName "@NsTypeConstr" "BOOL"),
              fieldOrigin = StructField
                StructField {
                  structFieldLoc = "bool.h:16:10",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "bools3_x"},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = Name "BOOL",
                      nameHsIdent = HsIdentifier
                        "BOOL"}
                    NameOriginInSource,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing},
            Field {
              fieldName = HsName
                "@NsVar"
                "bools3_y",
              fieldType = HsTypRef
                (HsName "@NsTypeConstr" "BOOL"),
              fieldOrigin = StructField
                StructField {
                  structFieldLoc = "bool.h:17:10",
                  structFieldName = NamePair {
                    nameC = Name "y",
                    nameHsIdent = HsIdentifier
                      "bools3_y"},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = Name "BOOL",
                      nameHsIdent = HsIdentifier
                        "BOOL"}
                    NameOriginInSource,
                  structFieldOffset = 8,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "bool.h:15:8",
                declId = NamePair {
                  nameC = Name "bools3",
                  nameHsIdent = HsIdentifier
                    "Bools3"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "bool.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Bools3"),
                  structSizeof = 2,
                  structAlignment = 1,
                  structFields = [
                    StructField {
                      structFieldLoc = "bool.h:16:10",
                      structFieldName = NamePair {
                        nameC = Name "x",
                        nameHsIdent = HsIdentifier
                          "bools3_x"},
                      structFieldType =
                      TypeMacroTypedef
                        NamePair {
                          nameC = Name "BOOL",
                          nameHsIdent = HsIdentifier
                            "BOOL"}
                        NameOriginInSource,
                      structFieldOffset = 0,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing},
                    StructField {
                      structFieldLoc = "bool.h:17:10",
                      structFieldName = NamePair {
                        nameC = Name "y",
                        nameHsIdent = HsIdentifier
                          "bools3_y"},
                      structFieldType =
                      TypeMacroTypedef
                        NamePair {
                          nameC = Name "BOOL",
                          nameHsIdent = HsIdentifier
                            "BOOL"}
                        NameOriginInSource,
                      structFieldOffset = 8,
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
          storableSizeOf = 2,
          storableAlignment = 1,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Bools3",
                  structConstr = HsName
                    "@NsConstr"
                    "Bools3",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bools3_x",
                      fieldType = HsTypRef
                        (HsName "@NsTypeConstr" "BOOL"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "bool.h:16:10",
                          structFieldName = NamePair {
                            nameC = Name "x",
                            nameHsIdent = HsIdentifier
                              "bools3_x"},
                          structFieldType =
                          TypeMacroTypedef
                            NamePair {
                              nameC = Name "BOOL",
                              nameHsIdent = HsIdentifier
                                "BOOL"}
                            NameOriginInSource,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bools3_y",
                      fieldType = HsTypRef
                        (HsName "@NsTypeConstr" "BOOL"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "bool.h:17:10",
                          structFieldName = NamePair {
                            nameC = Name "y",
                            nameHsIdent = HsIdentifier
                              "bools3_y"},
                          structFieldType =
                          TypeMacroTypedef
                            NamePair {
                              nameC = Name "BOOL",
                              nameHsIdent = HsIdentifier
                                "BOOL"}
                            NameOriginInSource,
                          structFieldOffset = 8,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bool.h:15:8",
                        declId = NamePair {
                          nameC = Name "bools3",
                          nameHsIdent = HsIdentifier
                            "Bools3"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "bool.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Bools3"),
                          structSizeof = 2,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldLoc = "bool.h:16:10",
                              structFieldName = NamePair {
                                nameC = Name "x",
                                nameHsIdent = HsIdentifier
                                  "bools3_x"},
                              structFieldType =
                              TypeMacroTypedef
                                NamePair {
                                  nameC = Name "BOOL",
                                  nameHsIdent = HsIdentifier
                                    "BOOL"}
                                NameOriginInSource,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc = "bool.h:17:10",
                              structFieldName = NamePair {
                                nameC = Name "y",
                                nameHsIdent = HsIdentifier
                                  "bools3_y"},
                              structFieldType =
                              TypeMacroTypedef
                                NamePair {
                                  nameC = Name "BOOL",
                                  nameHsIdent = HsIdentifier
                                    "BOOL"}
                                NameOriginInSource,
                              structFieldOffset = 8,
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
                PeekByteOff (Idx 0) 1]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Bools3",
                  structConstr = HsName
                    "@NsConstr"
                    "Bools3",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bools3_x",
                      fieldType = HsTypRef
                        (HsName "@NsTypeConstr" "BOOL"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "bool.h:16:10",
                          structFieldName = NamePair {
                            nameC = Name "x",
                            nameHsIdent = HsIdentifier
                              "bools3_x"},
                          structFieldType =
                          TypeMacroTypedef
                            NamePair {
                              nameC = Name "BOOL",
                              nameHsIdent = HsIdentifier
                                "BOOL"}
                            NameOriginInSource,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bools3_y",
                      fieldType = HsTypRef
                        (HsName "@NsTypeConstr" "BOOL"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "bool.h:17:10",
                          structFieldName = NamePair {
                            nameC = Name "y",
                            nameHsIdent = HsIdentifier
                              "bools3_y"},
                          structFieldType =
                          TypeMacroTypedef
                            NamePair {
                              nameC = Name "BOOL",
                              nameHsIdent = HsIdentifier
                                "BOOL"}
                            NameOriginInSource,
                          structFieldOffset = 8,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bool.h:15:8",
                        declId = NamePair {
                          nameC = Name "bools3",
                          nameHsIdent = HsIdentifier
                            "Bools3"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "bool.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Bools3"),
                          structSizeof = 2,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldLoc = "bool.h:16:10",
                              structFieldName = NamePair {
                                nameC = Name "x",
                                nameHsIdent = HsIdentifier
                                  "bools3_x"},
                              structFieldType =
                              TypeMacroTypedef
                                NamePair {
                                  nameC = Name "BOOL",
                                  nameHsIdent = HsIdentifier
                                    "BOOL"}
                                NameOriginInSource,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc = "bool.h:17:10",
                              structFieldName = NamePair {
                                nameC = Name "y",
                                nameHsIdent = HsIdentifier
                                  "bools3_y"},
                              structFieldType =
                              TypeMacroTypedef
                                NamePair {
                                  nameC = Name "BOOL",
                                  nameHsIdent = HsIdentifier
                                    "BOOL"}
                                NameOriginInSource,
                              structFieldOffset = 8,
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
                      1
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
        "Bools3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Bools3",
      deriveInstanceComment =
      Nothing}]
