[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S",
      structConstr = HsName
        "@NsConstr"
        "S",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s_f",
          fieldType = HsConstArray
            3
            (HsPrimType HsPrimCShort),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "type_attributes.h:8:18",
              structFieldName = NamePair {
                nameC = CName "f",
                nameHsIdent = HsIdentifier
                  "s_f"},
              structFieldType = TypeConstArray
                3
                (TypePrim
                  (PrimIntegral
                    PrimShort
                    Signed)),
              structFieldOffset = 0,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "type_attributes.h:8:8",
            declId = NamePair {
              nameC = CName "S",
              nameHsIdent = HsIdentifier "S"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "type_attributes.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "S"),
              structSizeof = 8,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "type_attributes.h:8:18",
                  structFieldName = NamePair {
                    nameC = CName "f",
                    nameHsIdent = HsIdentifier
                      "s_f"},
                  structFieldType = TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimShort
                        Signed)),
                  structFieldOffset = 0,
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
          "S",
        structConstr = HsName
          "@NsConstr"
          "S",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "s_f",
            fieldType = HsConstArray
              3
              (HsPrimType HsPrimCShort),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "type_attributes.h:8:18",
                structFieldName = NamePair {
                  nameC = CName "f",
                  nameHsIdent = HsIdentifier
                    "s_f"},
                structFieldType = TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral
                      PrimShort
                      Signed)),
                structFieldOffset = 0,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "type_attributes.h:8:8",
              declId = NamePair {
                nameC = CName "S",
                nameHsIdent = HsIdentifier "S"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader =
              "type_attributes.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "S"),
                structSizeof = 8,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "type_attributes.h:8:18",
                    structFieldName = NamePair {
                      nameC = CName "f",
                      nameHsIdent = HsIdentifier
                        "s_f"},
                    structFieldType = TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral
                          PrimShort
                          Signed)),
                    structFieldOffset = 0,
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
        storableAlignment = 8,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "S",
                structConstr = HsName
                  "@NsConstr"
                  "S",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s_f",
                    fieldType = HsConstArray
                      3
                      (HsPrimType HsPrimCShort),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "type_attributes.h:8:18",
                        structFieldName = NamePair {
                          nameC = CName "f",
                          nameHsIdent = HsIdentifier
                            "s_f"},
                        structFieldType = TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral
                              PrimShort
                              Signed)),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "type_attributes.h:8:8",
                      declId = NamePair {
                        nameC = CName "S",
                        nameHsIdent = HsIdentifier "S"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "type_attributes.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S"),
                        structSizeof = 8,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "type_attributes.h:8:18",
                            structFieldName = NamePair {
                              nameC = CName "f",
                              nameHsIdent = HsIdentifier
                                "s_f"},
                            structFieldType = TypeConstArray
                              3
                              (TypePrim
                                (PrimIntegral
                                  PrimShort
                                  Signed)),
                            structFieldOffset = 0,
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
            [PeekByteOff (Idx 0) 0]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "S",
                structConstr = HsName
                  "@NsConstr"
                  "S",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s_f",
                    fieldType = HsConstArray
                      3
                      (HsPrimType HsPrimCShort),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "type_attributes.h:8:18",
                        structFieldName = NamePair {
                          nameC = CName "f",
                          nameHsIdent = HsIdentifier
                            "s_f"},
                        structFieldType = TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral
                              PrimShort
                              Signed)),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "type_attributes.h:8:8",
                      declId = NamePair {
                        nameC = CName "S",
                        nameHsIdent = HsIdentifier "S"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "type_attributes.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S"),
                        structSizeof = 8,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "type_attributes.h:8:18",
                            structFieldName = NamePair {
                              nameC = CName "f",
                              nameHsIdent = HsIdentifier
                                "s_f"},
                            structFieldType = TypeConstArray
                              3
                              (TypePrim
                                (PrimIntegral
                                  PrimShort
                                  Signed)),
                            structFieldOffset = 0,
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
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "S"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "S"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "More_aligned_int",
      newtypeConstr = HsName
        "@NsConstr"
        "More_aligned_int",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_More_aligned_int",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "type_attributes.h:9:13",
          declId = NamePair {
            nameC = CName
              "more_aligned_int",
            nameHsIdent = HsIdentifier
              "More_aligned_int"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "type_attributes.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "More_aligned_int",
              newtypeField = HsName
                "@NsVar"
                "un_More_aligned_int"},
            typedefType = TypePrim
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
      "More_aligned_int"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "More_aligned_int"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "More_aligned_int"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "More_aligned_int"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "More_aligned_int"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "More_aligned_int"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "More_aligned_int"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "More_aligned_int"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "More_aligned_int"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "More_aligned_int"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "More_aligned_int"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "More_aligned_int"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "More_aligned_int"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S2",
      structConstr = HsName
        "@NsConstr"
        "S2",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s2_f",
          fieldType = HsConstArray
            3
            (HsPrimType HsPrimCShort),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "type_attributes.h:11:19",
              structFieldName = NamePair {
                nameC = CName "f",
                nameHsIdent = HsIdentifier
                  "s2_f"},
              structFieldType = TypeConstArray
                3
                (TypePrim
                  (PrimIntegral
                    PrimShort
                    Signed)),
              structFieldOffset = 0,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "type_attributes.h:11:8",
            declId = NamePair {
              nameC = CName "S2",
              nameHsIdent = HsIdentifier
                "S2"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "type_attributes.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "S2"),
              structSizeof = 16,
              structAlignment = 16,
              structFields = [
                StructField {
                  structFieldLoc =
                  "type_attributes.h:11:19",
                  structFieldName = NamePair {
                    nameC = CName "f",
                    nameHsIdent = HsIdentifier
                      "s2_f"},
                  structFieldType = TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimShort
                        Signed)),
                  structFieldOffset = 0,
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
          "S2",
        structConstr = HsName
          "@NsConstr"
          "S2",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "s2_f",
            fieldType = HsConstArray
              3
              (HsPrimType HsPrimCShort),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "type_attributes.h:11:19",
                structFieldName = NamePair {
                  nameC = CName "f",
                  nameHsIdent = HsIdentifier
                    "s2_f"},
                structFieldType = TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral
                      PrimShort
                      Signed)),
                structFieldOffset = 0,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "type_attributes.h:11:8",
              declId = NamePair {
                nameC = CName "S2",
                nameHsIdent = HsIdentifier
                  "S2"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader =
              "type_attributes.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "S2"),
                structSizeof = 16,
                structAlignment = 16,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "type_attributes.h:11:19",
                    structFieldName = NamePair {
                      nameC = CName "f",
                      nameHsIdent = HsIdentifier
                        "s2_f"},
                    structFieldType = TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral
                          PrimShort
                          Signed)),
                    structFieldOffset = 0,
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
        storableSizeOf = 16,
        storableAlignment = 16,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "S2",
                structConstr = HsName
                  "@NsConstr"
                  "S2",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_f",
                    fieldType = HsConstArray
                      3
                      (HsPrimType HsPrimCShort),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "type_attributes.h:11:19",
                        structFieldName = NamePair {
                          nameC = CName "f",
                          nameHsIdent = HsIdentifier
                            "s2_f"},
                        structFieldType = TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral
                              PrimShort
                              Signed)),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "type_attributes.h:11:8",
                      declId = NamePair {
                        nameC = CName "S2",
                        nameHsIdent = HsIdentifier
                          "S2"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "type_attributes.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S2"),
                        structSizeof = 16,
                        structAlignment = 16,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "type_attributes.h:11:19",
                            structFieldName = NamePair {
                              nameC = CName "f",
                              nameHsIdent = HsIdentifier
                                "s2_f"},
                            structFieldType = TypeConstArray
                              3
                              (TypePrim
                                (PrimIntegral
                                  PrimShort
                                  Signed)),
                            structFieldOffset = 0,
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
            [PeekByteOff (Idx 0) 0]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "S2",
                structConstr = HsName
                  "@NsConstr"
                  "S2",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_f",
                    fieldType = HsConstArray
                      3
                      (HsPrimType HsPrimCShort),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "type_attributes.h:11:19",
                        structFieldName = NamePair {
                          nameC = CName "f",
                          nameHsIdent = HsIdentifier
                            "s2_f"},
                        structFieldType = TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral
                              PrimShort
                              Signed)),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "type_attributes.h:11:8",
                      declId = NamePair {
                        nameC = CName "S2",
                        nameHsIdent = HsIdentifier
                          "S2"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "type_attributes.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S2"),
                        structSizeof = 16,
                        structAlignment = 16,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "type_attributes.h:11:19",
                            structFieldName = NamePair {
                              nameC = CName "f",
                              nameHsIdent = HsIdentifier
                                "s2_f"},
                            structFieldType = TypeConstArray
                              3
                              (TypePrim
                                (PrimIntegral
                                  PrimShort
                                  Signed)),
                            structFieldOffset = 0,
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
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "S2"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "S2"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "My_unpacked_struct",
      structConstr = HsName
        "@NsConstr"
        "My_unpacked_struct",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "my_unpacked_struct_c",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "type_attributes.h:15:8",
              structFieldName = NamePair {
                nameC = CName "c",
                nameHsIdent = HsIdentifier
                  "my_unpacked_struct_c"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "my_unpacked_struct_i",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "type_attributes.h:16:7",
              structFieldName = NamePair {
                nameC = CName "i",
                nameHsIdent = HsIdentifier
                  "my_unpacked_struct_i"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "type_attributes.h:13:8",
            declId = NamePair {
              nameC = CName
                "my_unpacked_struct",
              nameHsIdent = HsIdentifier
                "My_unpacked_struct"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "type_attributes.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "My_unpacked_struct"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "type_attributes.h:15:8",
                  structFieldName = NamePair {
                    nameC = CName "c",
                    nameHsIdent = HsIdentifier
                      "my_unpacked_struct_c"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "type_attributes.h:16:7",
                  structFieldName = NamePair {
                    nameC = CName "i",
                    nameHsIdent = HsIdentifier
                      "my_unpacked_struct_i"},
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
        [Eq, Show, Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "My_unpacked_struct",
        structConstr = HsName
          "@NsConstr"
          "My_unpacked_struct",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "my_unpacked_struct_c",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "type_attributes.h:15:8",
                structFieldName = NamePair {
                  nameC = CName "c",
                  nameHsIdent = HsIdentifier
                    "my_unpacked_struct_c"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "my_unpacked_struct_i",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "type_attributes.h:16:7",
                structFieldName = NamePair {
                  nameC = CName "i",
                  nameHsIdent = HsIdentifier
                    "my_unpacked_struct_i"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 32,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "type_attributes.h:13:8",
              declId = NamePair {
                nameC = CName
                  "my_unpacked_struct",
                nameHsIdent = HsIdentifier
                  "My_unpacked_struct"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader =
              "type_attributes.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "My_unpacked_struct"),
                structSizeof = 8,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "type_attributes.h:15:8",
                    structFieldName = NamePair {
                      nameC = CName "c",
                      nameHsIdent = HsIdentifier
                        "my_unpacked_struct_c"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "type_attributes.h:16:7",
                    structFieldName = NamePair {
                      nameC = CName "i",
                      nameHsIdent = HsIdentifier
                        "my_unpacked_struct_i"},
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
                  "My_unpacked_struct",
                structConstr = HsName
                  "@NsConstr"
                  "My_unpacked_struct",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "my_unpacked_struct_c",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "type_attributes.h:15:8",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "my_unpacked_struct_c"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "my_unpacked_struct_i",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "type_attributes.h:16:7",
                        structFieldName = NamePair {
                          nameC = CName "i",
                          nameHsIdent = HsIdentifier
                            "my_unpacked_struct_i"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "type_attributes.h:13:8",
                      declId = NamePair {
                        nameC = CName
                          "my_unpacked_struct",
                        nameHsIdent = HsIdentifier
                          "My_unpacked_struct"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "type_attributes.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "My_unpacked_struct"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "type_attributes.h:15:8",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "my_unpacked_struct_c"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "type_attributes.h:16:7",
                            structFieldName = NamePair {
                              nameC = CName "i",
                              nameHsIdent = HsIdentifier
                                "my_unpacked_struct_i"},
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
                  "My_unpacked_struct",
                structConstr = HsName
                  "@NsConstr"
                  "My_unpacked_struct",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "my_unpacked_struct_c",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "type_attributes.h:15:8",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "my_unpacked_struct_c"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "my_unpacked_struct_i",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "type_attributes.h:16:7",
                        structFieldName = NamePair {
                          nameC = CName "i",
                          nameHsIdent = HsIdentifier
                            "my_unpacked_struct_i"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "type_attributes.h:13:8",
                      declId = NamePair {
                        nameC = CName
                          "my_unpacked_struct",
                        nameHsIdent = HsIdentifier
                          "My_unpacked_struct"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "type_attributes.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "My_unpacked_struct"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "type_attributes.h:15:8",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "my_unpacked_struct_c"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "type_attributes.h:16:7",
                            structFieldName = NamePair {
                              nameC = CName "i",
                              nameHsIdent = HsIdentifier
                                "my_unpacked_struct_i"},
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
    (HsName
      "@NsTypeConstr"
      "My_unpacked_struct"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "My_unpacked_struct"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "My_packed_struct",
      structConstr = HsName
        "@NsConstr"
        "My_packed_struct",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "my_packed_struct_c",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "type_attributes.h:21:9",
              structFieldName = NamePair {
                nameC = CName "c",
                nameHsIdent = HsIdentifier
                  "my_packed_struct_c"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "my_packed_struct_i",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "type_attributes.h:22:9",
              structFieldName = NamePair {
                nameC = CName "i",
                nameHsIdent = HsIdentifier
                  "my_packed_struct_i"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 8,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "my_packed_struct_s",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "My_unpacked_struct"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "type_attributes.h:23:30",
              structFieldName = NamePair {
                nameC = CName "s",
                nameHsIdent = HsIdentifier
                  "my_packed_struct_s"},
              structFieldType = TypeStruct
                NamePair {
                  nameC = CName
                    "my_unpacked_struct",
                  nameHsIdent = HsIdentifier
                    "My_unpacked_struct"}
                NameOriginInSource,
              structFieldOffset = 40,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "type_attributes.h:19:37",
            declId = NamePair {
              nameC = CName
                "my_packed_struct",
              nameHsIdent = HsIdentifier
                "My_packed_struct"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "type_attributes.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "My_packed_struct"),
              structSizeof = 13,
              structAlignment = 1,
              structFields = [
                StructField {
                  structFieldLoc =
                  "type_attributes.h:21:9",
                  structFieldName = NamePair {
                    nameC = CName "c",
                    nameHsIdent = HsIdentifier
                      "my_packed_struct_c"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "type_attributes.h:22:9",
                  structFieldName = NamePair {
                    nameC = CName "i",
                    nameHsIdent = HsIdentifier
                      "my_packed_struct_i"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 8,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "type_attributes.h:23:30",
                  structFieldName = NamePair {
                    nameC = CName "s",
                    nameHsIdent = HsIdentifier
                      "my_packed_struct_s"},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = CName
                        "my_unpacked_struct",
                      nameHsIdent = HsIdentifier
                        "My_unpacked_struct"}
                    NameOriginInSource,
                  structFieldOffset = 40,
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
          "My_packed_struct",
        structConstr = HsName
          "@NsConstr"
          "My_packed_struct",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "my_packed_struct_c",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "type_attributes.h:21:9",
                structFieldName = NamePair {
                  nameC = CName "c",
                  nameHsIdent = HsIdentifier
                    "my_packed_struct_c"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "my_packed_struct_i",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "type_attributes.h:22:9",
                structFieldName = NamePair {
                  nameC = CName "i",
                  nameHsIdent = HsIdentifier
                    "my_packed_struct_i"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 8,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "my_packed_struct_s",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "My_unpacked_struct"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "type_attributes.h:23:30",
                structFieldName = NamePair {
                  nameC = CName "s",
                  nameHsIdent = HsIdentifier
                    "my_packed_struct_s"},
                structFieldType = TypeStruct
                  NamePair {
                    nameC = CName
                      "my_unpacked_struct",
                    nameHsIdent = HsIdentifier
                      "My_unpacked_struct"}
                  NameOriginInSource,
                structFieldOffset = 40,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "type_attributes.h:19:37",
              declId = NamePair {
                nameC = CName
                  "my_packed_struct",
                nameHsIdent = HsIdentifier
                  "My_packed_struct"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader =
              "type_attributes.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "My_packed_struct"),
                structSizeof = 13,
                structAlignment = 1,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "type_attributes.h:21:9",
                    structFieldName = NamePair {
                      nameC = CName "c",
                      nameHsIdent = HsIdentifier
                        "my_packed_struct_c"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "type_attributes.h:22:9",
                    structFieldName = NamePair {
                      nameC = CName "i",
                      nameHsIdent = HsIdentifier
                        "my_packed_struct_i"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 8,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "type_attributes.h:23:30",
                    structFieldName = NamePair {
                      nameC = CName "s",
                      nameHsIdent = HsIdentifier
                        "my_packed_struct_s"},
                    structFieldType = TypeStruct
                      NamePair {
                        nameC = CName
                          "my_unpacked_struct",
                        nameHsIdent = HsIdentifier
                          "My_unpacked_struct"}
                      NameOriginInSource,
                    structFieldOffset = 40,
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
        storableSizeOf = 13,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "My_packed_struct",
                structConstr = HsName
                  "@NsConstr"
                  "My_packed_struct",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "my_packed_struct_c",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "type_attributes.h:21:9",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "my_packed_struct_c"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "my_packed_struct_i",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "type_attributes.h:22:9",
                        structFieldName = NamePair {
                          nameC = CName "i",
                          nameHsIdent = HsIdentifier
                            "my_packed_struct_i"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 8,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "my_packed_struct_s",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "My_unpacked_struct"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "type_attributes.h:23:30",
                        structFieldName = NamePair {
                          nameC = CName "s",
                          nameHsIdent = HsIdentifier
                            "my_packed_struct_s"},
                        structFieldType = TypeStruct
                          NamePair {
                            nameC = CName
                              "my_unpacked_struct",
                            nameHsIdent = HsIdentifier
                              "My_unpacked_struct"}
                          NameOriginInSource,
                        structFieldOffset = 40,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "type_attributes.h:19:37",
                      declId = NamePair {
                        nameC = CName
                          "my_packed_struct",
                        nameHsIdent = HsIdentifier
                          "My_packed_struct"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "type_attributes.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "My_packed_struct"),
                        structSizeof = 13,
                        structAlignment = 1,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "type_attributes.h:21:9",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "my_packed_struct_c"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "type_attributes.h:22:9",
                            structFieldName = NamePair {
                              nameC = CName "i",
                              nameHsIdent = HsIdentifier
                                "my_packed_struct_i"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 8,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "type_attributes.h:23:30",
                            structFieldName = NamePair {
                              nameC = CName "s",
                              nameHsIdent = HsIdentifier
                                "my_packed_struct_s"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = CName
                                  "my_unpacked_struct",
                                nameHsIdent = HsIdentifier
                                  "My_unpacked_struct"}
                              NameOriginInSource,
                            structFieldOffset = 40,
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
              PeekByteOff (Idx 0) 1,
              PeekByteOff (Idx 0) 5]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "My_packed_struct",
                structConstr = HsName
                  "@NsConstr"
                  "My_packed_struct",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "my_packed_struct_c",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "type_attributes.h:21:9",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "my_packed_struct_c"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "my_packed_struct_i",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "type_attributes.h:22:9",
                        structFieldName = NamePair {
                          nameC = CName "i",
                          nameHsIdent = HsIdentifier
                            "my_packed_struct_i"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 8,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "my_packed_struct_s",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "My_unpacked_struct"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "type_attributes.h:23:30",
                        structFieldName = NamePair {
                          nameC = CName "s",
                          nameHsIdent = HsIdentifier
                            "my_packed_struct_s"},
                        structFieldType = TypeStruct
                          NamePair {
                            nameC = CName
                              "my_unpacked_struct",
                            nameHsIdent = HsIdentifier
                              "My_unpacked_struct"}
                          NameOriginInSource,
                        structFieldOffset = 40,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "type_attributes.h:19:37",
                      declId = NamePair {
                        nameC = CName
                          "my_packed_struct",
                        nameHsIdent = HsIdentifier
                          "My_packed_struct"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "type_attributes.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "My_packed_struct"),
                        structSizeof = 13,
                        structAlignment = 1,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "type_attributes.h:21:9",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "my_packed_struct_c"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "type_attributes.h:22:9",
                            structFieldName = NamePair {
                              nameC = CName "i",
                              nameHsIdent = HsIdentifier
                                "my_packed_struct_i"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 8,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "type_attributes.h:23:30",
                            structFieldName = NamePair {
                              nameC = CName "s",
                              nameHsIdent = HsIdentifier
                                "my_packed_struct_s"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = CName
                                  "my_unpacked_struct",
                                nameHsIdent = HsIdentifier
                                  "My_unpacked_struct"}
                              NameOriginInSource,
                            structFieldOffset = 40,
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
              (Add 3)
              (Seq
                [
                  PokeByteOff (Idx 4) 0 (Idx 0),
                  PokeByteOff (Idx 4) 1 (Idx 1),
                  PokeByteOff
                    (Idx 4)
                    5
                    (Idx 2)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "My_packed_struct"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "My_packed_struct"),
  DeclEmpty
    EmptyData {
      emptyDataName = HsName
        "@NsTypeConstr"
        "Wait",
      emptyDataOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "type_attributes.h:29:9",
          declId = NamePair {
            nameC = CName "wait",
            nameHsIdent = HsIdentifier
              "Wait"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "type_attributes.h"},
        declKind = OpaqueUnion,
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Wait_status_ptr_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Wait_status_ptr_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Wait_status_ptr_t",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "type_attributes.h:26:9",
          declId = NamePair {
            nameC = CName
              "wait_status_ptr_t",
            nameHsIdent = HsIdentifier
              "Wait_status_ptr_t"},
          declOrigin = NameOriginGenerated
            (AnonId
              "type_attributes.h:26:9"),
          declAliases = [
            CName "wait_status_ptr_t"],
          declHeader =
          "type_attributes.h"},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Wait_status_ptr_t",
              newtypeField = HsName
                "@NsVar"
                "un_Wait_status_ptr_t"},
            unionSizeof = 8,
            unionAlignment = 8,
            unionFields = [
              UnionField {
                unionFieldLoc =
                "type_attributes.h:28:8",
                unionFieldName = NamePair {
                  nameC = CName "__ip",
                  nameHsIdent = HsIdentifier
                    "wait_status_ptr_t___ip"},
                unionFieldType = TypePointer
                  (TypePrim
                    (PrimIntegral PrimInt Signed))},
              UnionField {
                unionFieldLoc =
                "type_attributes.h:29:15",
                unionFieldName = NamePair {
                  nameC = CName "__up",
                  nameHsIdent = HsIdentifier
                    "wait_status_ptr_t___up"},
                unionFieldType = TypePointer
                  (TypeUnion
                    NamePair {
                      nameC = CName "wait",
                      nameHsIdent = HsIdentifier
                        "Wait"}
                    NameOriginInSource)}]},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Storable]},
  DeclNewtypeInstance
    (DeriveVia
      (HsSizedByteArray 8 8))
    Storable
    (HsName
      "@NsTypeConstr"
      "Wait_status_ptr_t"),
  DeclUnionGetter
    (HsName
      "@NsTypeConstr"
      "Wait_status_ptr_t")
    (HsPtr (HsPrimType HsPrimCInt))
    (HsName
      "@NsVar"
      "get_wait_status_ptr_t___ip"),
  DeclUnionSetter
    (HsName
      "@NsTypeConstr"
      "Wait_status_ptr_t")
    (HsPtr (HsPrimType HsPrimCInt))
    (HsName
      "@NsVar"
      "set_wait_status_ptr_t___ip"),
  DeclUnionGetter
    (HsName
      "@NsTypeConstr"
      "Wait_status_ptr_t")
    (HsPtr
      (HsTypRef
        (HsName
          "@NsTypeConstr"
          "Wait")))
    (HsName
      "@NsVar"
      "get_wait_status_ptr_t___up"),
  DeclUnionSetter
    (HsName
      "@NsTypeConstr"
      "Wait_status_ptr_t")
    (HsPtr
      (HsTypRef
        (HsName
          "@NsTypeConstr"
          "Wait")))
    (HsName
      "@NsVar"
      "set_wait_status_ptr_t___up"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "T1",
      newtypeConstr = HsName
        "@NsConstr"
        "T1",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_T1",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "type_attributes.h:32:13",
          declId = NamePair {
            nameC = CName "T1",
            nameHsIdent = HsIdentifier
              "T1"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "type_attributes.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "T1",
              newtypeField = HsName
                "@NsVar"
                "un_T1"},
            typedefType = TypePrim
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
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Short_a",
      newtypeConstr = HsName
        "@NsConstr"
        "Short_a",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Short_a",
        fieldType = HsPrimType
          HsPrimCShort,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "type_attributes.h:34:46",
          declId = NamePair {
            nameC = CName "short_a",
            nameHsIdent = HsIdentifier
              "Short_a"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "type_attributes.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Short_a",
              newtypeField = HsName
                "@NsVar"
                "un_Short_a"},
            typedefType = TypePrim
              (PrimIntegral
                PrimShort
                Signed)},
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
      "Short_a"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Short_a"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Short_a"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Short_a"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Short_a"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "Short_a"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "Short_a"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "Short_a"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "Short_a"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "Short_a"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "Short_a"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "Short_a"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "Short_a")]
