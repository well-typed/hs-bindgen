[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "MY_TYPE",
      newtypeConstr = HsName
        "@NsConstr"
        "MY_TYPE",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_MY_TYPE",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "macro_typedef_struct.h:1:9",
          declId = NamePair {
            nameC = CName "MY_TYPE",
            nameHsIdent = HsIdentifier
              "MY_TYPE"}},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "MY_TYPE",
              newtypeField = HsName
                "@NsVar"
                "un_MY_TYPE"},
            macroType = TypePrim
              (PrimIntegral PrimInt Signed)},
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
    (HsName
      "@NsTypeConstr"
      "MY_TYPE"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "MY_TYPE"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "MY_TYPE"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "MY_TYPE"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "MY_TYPE"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "MY_TYPE"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "MY_TYPE"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "MY_TYPE"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "MY_TYPE"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "MY_TYPE"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "MY_TYPE"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "MY_TYPE"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "MY_TYPE"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Bar",
      structConstr = HsName
        "@NsConstr"
        "Bar",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "bar_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "macro_typedef_struct.h:4:7",
              structFieldName = NamePair {
                nameC = CName "x",
                nameHsIdent = HsIdentifier
                  "bar_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "bar_y",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "MY_TYPE"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "macro_typedef_struct.h:5:11",
              structFieldName = NamePair {
                nameC = CName "y",
                nameHsIdent = HsIdentifier
                  "bar_y"},
              structFieldType = TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = CName "MY_TYPE",
                    nameHsIdent = HsIdentifier
                      "MY_TYPE"}),
              structFieldOffset = 32,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "macro_typedef_struct.h:3:9",
            declId = NamePair {
              nameC = CName "bar",
              nameHsIdent = HsIdentifier
                "Bar"}},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Bar"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "macro_typedef_struct.h:4:7",
                  structFieldName = NamePair {
                    nameC = CName "x",
                    nameHsIdent = HsIdentifier
                      "bar_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "macro_typedef_struct.h:5:11",
                  structFieldName = NamePair {
                    nameC = CName "y",
                    nameHsIdent = HsIdentifier
                      "bar_y"},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = CName "MY_TYPE",
                        nameHsIdent = HsIdentifier
                          "MY_TYPE"}),
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
        [Eq, Show, Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Bar",
        structConstr = HsName
          "@NsConstr"
          "Bar",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "bar_x",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "macro_typedef_struct.h:4:7",
                structFieldName = NamePair {
                  nameC = CName "x",
                  nameHsIdent = HsIdentifier
                    "bar_x"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "bar_y",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "MY_TYPE"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "macro_typedef_struct.h:5:11",
                structFieldName = NamePair {
                  nameC = CName "y",
                  nameHsIdent = HsIdentifier
                    "bar_y"},
                structFieldType = TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = CName "MY_TYPE",
                      nameHsIdent = HsIdentifier
                        "MY_TYPE"}),
                structFieldOffset = 32,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "macro_typedef_struct.h:3:9",
              declId = NamePair {
                nameC = CName "bar",
                nameHsIdent = HsIdentifier
                  "Bar"}},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Bar"),
                structSizeof = 8,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "macro_typedef_struct.h:4:7",
                    structFieldName = NamePair {
                      nameC = CName "x",
                      nameHsIdent = HsIdentifier
                        "bar_x"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "macro_typedef_struct.h:5:11",
                    structFieldName = NamePair {
                      nameC = CName "y",
                      nameHsIdent = HsIdentifier
                        "bar_y"},
                    structFieldType = TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = CName "MY_TYPE",
                          nameHsIdent = HsIdentifier
                            "MY_TYPE"}),
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
          [Eq, Show, Storable]}
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
                  "Bar",
                structConstr = HsName
                  "@NsConstr"
                  "Bar",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bar_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "macro_typedef_struct.h:4:7",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "bar_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bar_y",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "MY_TYPE"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "macro_typedef_struct.h:5:11",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "bar_y"},
                        structFieldType = TypeTypedef
                          (TypedefRegular
                            NamePair {
                              nameC = CName "MY_TYPE",
                              nameHsIdent = HsIdentifier
                                "MY_TYPE"}),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "macro_typedef_struct.h:3:9",
                      declId = NamePair {
                        nameC = CName "bar",
                        nameHsIdent = HsIdentifier
                          "Bar"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Bar"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "macro_typedef_struct.h:4:7",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "bar_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "macro_typedef_struct.h:5:11",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "bar_y"},
                            structFieldType = TypeTypedef
                              (TypedefRegular
                                NamePair {
                                  nameC = CName "MY_TYPE",
                                  nameHsIdent = HsIdentifier
                                    "MY_TYPE"}),
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
                  [Eq, Show, Storable]})
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
                  "Bar",
                structConstr = HsName
                  "@NsConstr"
                  "Bar",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bar_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "macro_typedef_struct.h:4:7",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "bar_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bar_y",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "MY_TYPE"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "macro_typedef_struct.h:5:11",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "bar_y"},
                        structFieldType = TypeTypedef
                          (TypedefRegular
                            NamePair {
                              nameC = CName "MY_TYPE",
                              nameHsIdent = HsIdentifier
                                "MY_TYPE"}),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "macro_typedef_struct.h:3:9",
                      declId = NamePair {
                        nameC = CName "bar",
                        nameHsIdent = HsIdentifier
                          "Bar"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Bar"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "macro_typedef_struct.h:4:7",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "bar_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "macro_typedef_struct.h:5:11",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "bar_y"},
                            structFieldType = TypeTypedef
                              (TypedefRegular
                                NamePair {
                                  nameC = CName "MY_TYPE",
                                  nameHsIdent = HsIdentifier
                                    "MY_TYPE"}),
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
                  [Eq, Show, Storable]}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    4
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Bar"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Bar")]
