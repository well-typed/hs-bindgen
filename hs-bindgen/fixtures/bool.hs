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
                nameC = CName "x",
                nameHsIdent = HsIdentifier
                  "bools1_x"},
              structFieldType = TypePrim
                PrimBool,
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
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
                nameC = CName "y",
                nameHsIdent = HsIdentifier
                  "bools1_y"},
              structFieldType = TypePrim
                PrimBool,
              structFieldOffset = 8,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "bool.h:1:8",
            declId = NamePair {
              nameC = CName "bools1",
              nameHsIdent = HsIdentifier
                "Bools1"},
            declOrigin = NameOriginInSource,
            declAliases = []},
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
                    nameC = CName "x",
                    nameHsIdent = HsIdentifier
                      "bools1_x"},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc = "bool.h:3:11",
                  structFieldName = NamePair {
                    nameC = CName "y",
                    nameHsIdent = HsIdentifier
                      "bools1_y"},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 8,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            TypeSpec {
              typeSpecModule = Nothing,
              typeSpecIdentifier = Nothing,
              typeSpecInstances = Map.fromList
                []}},
      structInstances = Set.fromList
        [Eq, Show, Storable]},
  DeclInstance
    (InstanceStorable
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
                  nameC = CName "x",
                  nameHsIdent = HsIdentifier
                    "bools1_x"},
                structFieldType = TypePrim
                  PrimBool,
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
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
                  nameC = CName "y",
                  nameHsIdent = HsIdentifier
                    "bools1_y"},
                structFieldType = TypePrim
                  PrimBool,
                structFieldOffset = 8,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "bool.h:1:8",
              declId = NamePair {
                nameC = CName "bools1",
                nameHsIdent = HsIdentifier
                  "Bools1"},
              declOrigin = NameOriginInSource,
              declAliases = []},
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
                      nameC = CName "x",
                      nameHsIdent = HsIdentifier
                        "bools1_x"},
                    structFieldType = TypePrim
                      PrimBool,
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc = "bool.h:3:11",
                    structFieldName = NamePair {
                      nameC = CName "y",
                      nameHsIdent = HsIdentifier
                        "bools1_y"},
                    structFieldType = TypePrim
                      PrimBool,
                    structFieldOffset = 8,
                    structFieldWidth = Nothing}],
                structFlam = Nothing},
            declSpec = DeclSpec
              TypeSpec {
                typeSpecModule = Nothing,
                typeSpecIdentifier = Nothing,
                typeSpecInstances = Map.fromList
                  []}},
        structInstances = Set.fromList
          [Eq, Show, Storable]}
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
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "bools1_x"},
                        structFieldType = TypePrim
                          PrimBool,
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
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
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "bools1_y"},
                        structFieldType = TypePrim
                          PrimBool,
                        structFieldOffset = 8,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "bool.h:1:8",
                      declId = NamePair {
                        nameC = CName "bools1",
                        nameHsIdent = HsIdentifier
                          "Bools1"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
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
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "bools1_x"},
                            structFieldType = TypePrim
                              PrimBool,
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc = "bool.h:3:11",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "bools1_y"},
                            structFieldType = TypePrim
                              PrimBool,
                            structFieldOffset = 8,
                            structFieldWidth = Nothing}],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]})
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
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "bools1_x"},
                        structFieldType = TypePrim
                          PrimBool,
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
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
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "bools1_y"},
                        structFieldType = TypePrim
                          PrimBool,
                        structFieldOffset = 8,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "bool.h:1:8",
                      declId = NamePair {
                        nameC = CName "bools1",
                        nameHsIdent = HsIdentifier
                          "Bools1"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
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
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "bools1_x"},
                            structFieldType = TypePrim
                              PrimBool,
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc = "bool.h:3:11",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "bools1_y"},
                            structFieldType = TypePrim
                              PrimBool,
                            structFieldOffset = 8,
                            structFieldWidth = Nothing}],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    1
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Bools1"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Bools1"),
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
                nameC = CName "x",
                nameHsIdent = HsIdentifier
                  "bools2_x"},
              structFieldType = TypePrim
                PrimBool,
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
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
                nameC = CName "y",
                nameHsIdent = HsIdentifier
                  "bools2_y"},
              structFieldType = TypePrim
                PrimBool,
              structFieldOffset = 8,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "bool.h:8:8",
            declId = NamePair {
              nameC = CName "bools2",
              nameHsIdent = HsIdentifier
                "Bools2"},
            declOrigin = NameOriginInSource,
            declAliases = []},
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
                    nameC = CName "x",
                    nameHsIdent = HsIdentifier
                      "bools2_x"},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc = "bool.h:10:10",
                  structFieldName = NamePair {
                    nameC = CName "y",
                    nameHsIdent = HsIdentifier
                      "bools2_y"},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 8,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            TypeSpec {
              typeSpecModule = Nothing,
              typeSpecIdentifier = Nothing,
              typeSpecInstances = Map.fromList
                []}},
      structInstances = Set.fromList
        [Eq, Show, Storable]},
  DeclInstance
    (InstanceStorable
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
                  nameC = CName "x",
                  nameHsIdent = HsIdentifier
                    "bools2_x"},
                structFieldType = TypePrim
                  PrimBool,
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
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
                  nameC = CName "y",
                  nameHsIdent = HsIdentifier
                    "bools2_y"},
                structFieldType = TypePrim
                  PrimBool,
                structFieldOffset = 8,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "bool.h:8:8",
              declId = NamePair {
                nameC = CName "bools2",
                nameHsIdent = HsIdentifier
                  "Bools2"},
              declOrigin = NameOriginInSource,
              declAliases = []},
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
                      nameC = CName "x",
                      nameHsIdent = HsIdentifier
                        "bools2_x"},
                    structFieldType = TypePrim
                      PrimBool,
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc = "bool.h:10:10",
                    structFieldName = NamePair {
                      nameC = CName "y",
                      nameHsIdent = HsIdentifier
                        "bools2_y"},
                    structFieldType = TypePrim
                      PrimBool,
                    structFieldOffset = 8,
                    structFieldWidth = Nothing}],
                structFlam = Nothing},
            declSpec = DeclSpec
              TypeSpec {
                typeSpecModule = Nothing,
                typeSpecIdentifier = Nothing,
                typeSpecInstances = Map.fromList
                  []}},
        structInstances = Set.fromList
          [Eq, Show, Storable]}
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
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "bools2_x"},
                        structFieldType = TypePrim
                          PrimBool,
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
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
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "bools2_y"},
                        structFieldType = TypePrim
                          PrimBool,
                        structFieldOffset = 8,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "bool.h:8:8",
                      declId = NamePair {
                        nameC = CName "bools2",
                        nameHsIdent = HsIdentifier
                          "Bools2"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
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
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "bools2_x"},
                            structFieldType = TypePrim
                              PrimBool,
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc = "bool.h:10:10",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "bools2_y"},
                            structFieldType = TypePrim
                              PrimBool,
                            structFieldOffset = 8,
                            structFieldWidth = Nothing}],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]})
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
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "bools2_x"},
                        structFieldType = TypePrim
                          PrimBool,
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
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
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "bools2_y"},
                        structFieldType = TypePrim
                          PrimBool,
                        structFieldOffset = 8,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "bool.h:8:8",
                      declId = NamePair {
                        nameC = CName "bools2",
                        nameHsIdent = HsIdentifier
                          "Bools2"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
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
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "bools2_x"},
                            structFieldType = TypePrim
                              PrimBool,
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc = "bool.h:10:10",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "bools2_y"},
                            structFieldType = TypePrim
                              PrimBool,
                            structFieldOffset = 8,
                            structFieldWidth = Nothing}],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    1
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Bools2"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Bools2"),
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
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "bool.h:13:9",
          declId = NamePair {
            nameC = CName "BOOL",
            nameHsIdent = HsIdentifier
              "BOOL"},
          declOrigin = NameOriginInSource,
          declAliases = []},
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
          Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "BOOL"),
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
                nameC = CName "x",
                nameHsIdent = HsIdentifier
                  "bools3_x"},
              structFieldType =
              TypeMacroTypedef
                NamePair {
                  nameC = CName "BOOL",
                  nameHsIdent = HsIdentifier
                    "BOOL"}
                NameOriginInSource,
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
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
                nameC = CName "y",
                nameHsIdent = HsIdentifier
                  "bools3_y"},
              structFieldType =
              TypeMacroTypedef
                NamePair {
                  nameC = CName "BOOL",
                  nameHsIdent = HsIdentifier
                    "BOOL"}
                NameOriginInSource,
              structFieldOffset = 8,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "bool.h:15:8",
            declId = NamePair {
              nameC = CName "bools3",
              nameHsIdent = HsIdentifier
                "Bools3"},
            declOrigin = NameOriginInSource,
            declAliases = []},
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
                    nameC = CName "x",
                    nameHsIdent = HsIdentifier
                      "bools3_x"},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = CName "BOOL",
                      nameHsIdent = HsIdentifier
                        "BOOL"}
                    NameOriginInSource,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc = "bool.h:17:10",
                  structFieldName = NamePair {
                    nameC = CName "y",
                    nameHsIdent = HsIdentifier
                      "bools3_y"},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = CName "BOOL",
                      nameHsIdent = HsIdentifier
                        "BOOL"}
                    NameOriginInSource,
                  structFieldOffset = 8,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            TypeSpec {
              typeSpecModule = Nothing,
              typeSpecIdentifier = Nothing,
              typeSpecInstances = Map.fromList
                []}},
      structInstances = Set.fromList
        [Eq, Show, Storable]},
  DeclInstance
    (InstanceStorable
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
                  nameC = CName "x",
                  nameHsIdent = HsIdentifier
                    "bools3_x"},
                structFieldType =
                TypeMacroTypedef
                  NamePair {
                    nameC = CName "BOOL",
                    nameHsIdent = HsIdentifier
                      "BOOL"}
                  NameOriginInSource,
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
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
                  nameC = CName "y",
                  nameHsIdent = HsIdentifier
                    "bools3_y"},
                structFieldType =
                TypeMacroTypedef
                  NamePair {
                    nameC = CName "BOOL",
                    nameHsIdent = HsIdentifier
                      "BOOL"}
                  NameOriginInSource,
                structFieldOffset = 8,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "bool.h:15:8",
              declId = NamePair {
                nameC = CName "bools3",
                nameHsIdent = HsIdentifier
                  "Bools3"},
              declOrigin = NameOriginInSource,
              declAliases = []},
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
                      nameC = CName "x",
                      nameHsIdent = HsIdentifier
                        "bools3_x"},
                    structFieldType =
                    TypeMacroTypedef
                      NamePair {
                        nameC = CName "BOOL",
                        nameHsIdent = HsIdentifier
                          "BOOL"}
                      NameOriginInSource,
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc = "bool.h:17:10",
                    structFieldName = NamePair {
                      nameC = CName "y",
                      nameHsIdent = HsIdentifier
                        "bools3_y"},
                    structFieldType =
                    TypeMacroTypedef
                      NamePair {
                        nameC = CName "BOOL",
                        nameHsIdent = HsIdentifier
                          "BOOL"}
                      NameOriginInSource,
                    structFieldOffset = 8,
                    structFieldWidth = Nothing}],
                structFlam = Nothing},
            declSpec = DeclSpec
              TypeSpec {
                typeSpecModule = Nothing,
                typeSpecIdentifier = Nothing,
                typeSpecInstances = Map.fromList
                  []}},
        structInstances = Set.fromList
          [Eq, Show, Storable]}
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
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "bools3_x"},
                        structFieldType =
                        TypeMacroTypedef
                          NamePair {
                            nameC = CName "BOOL",
                            nameHsIdent = HsIdentifier
                              "BOOL"}
                          NameOriginInSource,
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
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
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "bools3_y"},
                        structFieldType =
                        TypeMacroTypedef
                          NamePair {
                            nameC = CName "BOOL",
                            nameHsIdent = HsIdentifier
                              "BOOL"}
                          NameOriginInSource,
                        structFieldOffset = 8,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "bool.h:15:8",
                      declId = NamePair {
                        nameC = CName "bools3",
                        nameHsIdent = HsIdentifier
                          "Bools3"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
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
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "bools3_x"},
                            structFieldType =
                            TypeMacroTypedef
                              NamePair {
                                nameC = CName "BOOL",
                                nameHsIdent = HsIdentifier
                                  "BOOL"}
                              NameOriginInSource,
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc = "bool.h:17:10",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "bools3_y"},
                            structFieldType =
                            TypeMacroTypedef
                              NamePair {
                                nameC = CName "BOOL",
                                nameHsIdent = HsIdentifier
                                  "BOOL"}
                              NameOriginInSource,
                            structFieldOffset = 8,
                            structFieldWidth = Nothing}],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]})
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
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "bools3_x"},
                        structFieldType =
                        TypeMacroTypedef
                          NamePair {
                            nameC = CName "BOOL",
                            nameHsIdent = HsIdentifier
                              "BOOL"}
                          NameOriginInSource,
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
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
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "bools3_y"},
                        structFieldType =
                        TypeMacroTypedef
                          NamePair {
                            nameC = CName "BOOL",
                            nameHsIdent = HsIdentifier
                              "BOOL"}
                          NameOriginInSource,
                        structFieldOffset = 8,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "bool.h:15:8",
                      declId = NamePair {
                        nameC = CName "bools3",
                        nameHsIdent = HsIdentifier
                          "Bools3"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
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
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "bools3_x"},
                            structFieldType =
                            TypeMacroTypedef
                              NamePair {
                                nameC = CName "BOOL",
                                nameHsIdent = HsIdentifier
                                  "BOOL"}
                              NameOriginInSource,
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc = "bool.h:17:10",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "bools3_y"},
                            structFieldType =
                            TypeMacroTypedef
                              NamePair {
                                nameC = CName "BOOL",
                                nameHsIdent = HsIdentifier
                                  "BOOL"}
                              NameOriginInSource,
                            structFieldOffset = 8,
                            structFieldWidth = Nothing}],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    1
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Bools3"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Bools3")]
