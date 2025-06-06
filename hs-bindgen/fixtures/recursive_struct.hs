[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Linked_list_A_s",
      structConstr = HsName
        "@NsConstr"
        "Linked_list_A_s",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "linked_list_A_s_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "recursive_struct.h:2:7",
              structFieldName = NamePair {
                nameC = CName "x",
                nameHsIdent = HsIdentifier
                  "linked_list_A_s_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "linked_list_A_s_next",
          fieldType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Linked_list_A_s")),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "recursive_struct.h:3:27",
              structFieldName = NamePair {
                nameC = CName "next",
                nameHsIdent = HsIdentifier
                  "linked_list_A_s_next"},
              structFieldType = TypePointer
                (TypeStruct
                  NamePair {
                    nameC = CName "linked_list_A_s",
                    nameHsIdent = HsIdentifier
                      "Linked_list_A_s"}),
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "recursive_struct.h:1:16",
            declId = NamePair {
              nameC = CName "linked_list_A_s",
              nameHsIdent = HsIdentifier
                "Linked_list_A_s"}},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Linked_list_A_s"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "recursive_struct.h:2:7",
                  structFieldName = NamePair {
                    nameC = CName "x",
                    nameHsIdent = HsIdentifier
                      "linked_list_A_s_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "recursive_struct.h:3:27",
                  structFieldName = NamePair {
                    nameC = CName "next",
                    nameHsIdent = HsIdentifier
                      "linked_list_A_s_next"},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = CName "linked_list_A_s",
                        nameHsIdent = HsIdentifier
                          "Linked_list_A_s"}),
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
        [Eq, Show, Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Linked_list_A_s",
        structConstr = HsName
          "@NsConstr"
          "Linked_list_A_s",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "linked_list_A_s_x",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "recursive_struct.h:2:7",
                structFieldName = NamePair {
                  nameC = CName "x",
                  nameHsIdent = HsIdentifier
                    "linked_list_A_s_x"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "linked_list_A_s_next",
            fieldType = HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Linked_list_A_s")),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "recursive_struct.h:3:27",
                structFieldName = NamePair {
                  nameC = CName "next",
                  nameHsIdent = HsIdentifier
                    "linked_list_A_s_next"},
                structFieldType = TypePointer
                  (TypeStruct
                    NamePair {
                      nameC = CName "linked_list_A_s",
                      nameHsIdent = HsIdentifier
                        "Linked_list_A_s"}),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "recursive_struct.h:1:16",
              declId = NamePair {
                nameC = CName "linked_list_A_s",
                nameHsIdent = HsIdentifier
                  "Linked_list_A_s"}},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "Linked_list_A_s"),
                structSizeof = 16,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "recursive_struct.h:2:7",
                    structFieldName = NamePair {
                      nameC = CName "x",
                      nameHsIdent = HsIdentifier
                        "linked_list_A_s_x"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "recursive_struct.h:3:27",
                    structFieldName = NamePair {
                      nameC = CName "next",
                      nameHsIdent = HsIdentifier
                        "linked_list_A_s_next"},
                    structFieldType = TypePointer
                      (TypeStruct
                        NamePair {
                          nameC = CName "linked_list_A_s",
                          nameHsIdent = HsIdentifier
                            "Linked_list_A_s"}),
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
          [Eq, Show, Storable]}
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
                  "Linked_list_A_s",
                structConstr = HsName
                  "@NsConstr"
                  "Linked_list_A_s",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "linked_list_A_s_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "recursive_struct.h:2:7",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "linked_list_A_s_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "linked_list_A_s_next",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Linked_list_A_s")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "recursive_struct.h:3:27",
                        structFieldName = NamePair {
                          nameC = CName "next",
                          nameHsIdent = HsIdentifier
                            "linked_list_A_s_next"},
                        structFieldType = TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = CName "linked_list_A_s",
                              nameHsIdent = HsIdentifier
                                "Linked_list_A_s"}),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "recursive_struct.h:1:16",
                      declId = NamePair {
                        nameC = CName "linked_list_A_s",
                        nameHsIdent = HsIdentifier
                          "Linked_list_A_s"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Linked_list_A_s"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "recursive_struct.h:2:7",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "linked_list_A_s_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "recursive_struct.h:3:27",
                            structFieldName = NamePair {
                              nameC = CName "next",
                              nameHsIdent = HsIdentifier
                                "linked_list_A_s_next"},
                            structFieldType = TypePointer
                              (TypeStruct
                                NamePair {
                                  nameC = CName "linked_list_A_s",
                                  nameHsIdent = HsIdentifier
                                    "Linked_list_A_s"}),
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
                  [Eq, Show, Storable]})
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
                  "Linked_list_A_s",
                structConstr = HsName
                  "@NsConstr"
                  "Linked_list_A_s",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "linked_list_A_s_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "recursive_struct.h:2:7",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "linked_list_A_s_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "linked_list_A_s_next",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Linked_list_A_s")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "recursive_struct.h:3:27",
                        structFieldName = NamePair {
                          nameC = CName "next",
                          nameHsIdent = HsIdentifier
                            "linked_list_A_s_next"},
                        structFieldType = TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = CName "linked_list_A_s",
                              nameHsIdent = HsIdentifier
                                "Linked_list_A_s"}),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "recursive_struct.h:1:16",
                      declId = NamePair {
                        nameC = CName "linked_list_A_s",
                        nameHsIdent = HsIdentifier
                          "Linked_list_A_s"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Linked_list_A_s"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "recursive_struct.h:2:7",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "linked_list_A_s_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "recursive_struct.h:3:27",
                            structFieldName = NamePair {
                              nameC = CName "next",
                              nameHsIdent = HsIdentifier
                                "linked_list_A_s_next"},
                            structFieldType = TypePointer
                              (TypeStruct
                                NamePair {
                                  nameC = CName "linked_list_A_s",
                                  nameHsIdent = HsIdentifier
                                    "Linked_list_A_s"}),
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
                  [Eq, Show, Storable]}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    8
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Linked_list_A_s"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Linked_list_A_s"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Linked_list_A_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Linked_list_A_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Linked_list_A_t",
        fieldType = HsTypRef
          (HsName
            "@NsTypeConstr"
            "Linked_list_A_s"),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "recursive_struct.h:4:3",
          declId = NamePair {
            nameC = CName "linked_list_A_t",
            nameHsIdent = HsIdentifier
              "Linked_list_A_t"}},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Linked_list_A_t",
              newtypeField = HsName
                "@NsVar"
                "un_Linked_list_A_t"},
            typedefType = TypeStruct
              NamePair {
                nameC = CName "linked_list_A_s",
                nameHsIdent = HsIdentifier
                  "Linked_list_A_s"}},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Linked_list_A_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Linked_list_A_t"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Linked_list_A_t"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Linked_list_B_t",
      structConstr = HsName
        "@NsConstr"
        "Linked_list_B_t",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "linked_list_B_t_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "recursive_struct.h:10:7",
              structFieldName = NamePair {
                nameC = CName "x",
                nameHsIdent = HsIdentifier
                  "linked_list_B_t_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "linked_list_B_t_next",
          fieldType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Linked_list_B_t")),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "recursive_struct.h:11:20",
              structFieldName = NamePair {
                nameC = CName "next",
                nameHsIdent = HsIdentifier
                  "linked_list_B_t_next"},
              structFieldType = TypePointer
                (TypeTypedef
                  (TypedefSquashed
                    (CName "linked_list_B_t")
                    (TypeStruct
                      NamePair {
                        nameC = CName "linked_list_B_t",
                        nameHsIdent = HsIdentifier
                          "Linked_list_B_t"}))),
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "recursive_struct.h:9:8",
            declId = NamePair {
              nameC = CName "linked_list_B_t",
              nameHsIdent = HsIdentifier
                "Linked_list_B_t"}},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Linked_list_B_t"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "recursive_struct.h:10:7",
                  structFieldName = NamePair {
                    nameC = CName "x",
                    nameHsIdent = HsIdentifier
                      "linked_list_B_t_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "recursive_struct.h:11:20",
                  structFieldName = NamePair {
                    nameC = CName "next",
                    nameHsIdent = HsIdentifier
                      "linked_list_B_t_next"},
                  structFieldType = TypePointer
                    (TypeTypedef
                      (TypedefSquashed
                        (CName "linked_list_B_t")
                        (TypeStruct
                          NamePair {
                            nameC = CName "linked_list_B_t",
                            nameHsIdent = HsIdentifier
                              "Linked_list_B_t"}))),
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
        [Eq, Show, Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Linked_list_B_t",
        structConstr = HsName
          "@NsConstr"
          "Linked_list_B_t",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "linked_list_B_t_x",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "recursive_struct.h:10:7",
                structFieldName = NamePair {
                  nameC = CName "x",
                  nameHsIdent = HsIdentifier
                    "linked_list_B_t_x"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "linked_list_B_t_next",
            fieldType = HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Linked_list_B_t")),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "recursive_struct.h:11:20",
                structFieldName = NamePair {
                  nameC = CName "next",
                  nameHsIdent = HsIdentifier
                    "linked_list_B_t_next"},
                structFieldType = TypePointer
                  (TypeTypedef
                    (TypedefSquashed
                      (CName "linked_list_B_t")
                      (TypeStruct
                        NamePair {
                          nameC = CName "linked_list_B_t",
                          nameHsIdent = HsIdentifier
                            "Linked_list_B_t"}))),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "recursive_struct.h:9:8",
              declId = NamePair {
                nameC = CName "linked_list_B_t",
                nameHsIdent = HsIdentifier
                  "Linked_list_B_t"}},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "Linked_list_B_t"),
                structSizeof = 16,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "recursive_struct.h:10:7",
                    structFieldName = NamePair {
                      nameC = CName "x",
                      nameHsIdent = HsIdentifier
                        "linked_list_B_t_x"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "recursive_struct.h:11:20",
                    structFieldName = NamePair {
                      nameC = CName "next",
                      nameHsIdent = HsIdentifier
                        "linked_list_B_t_next"},
                    structFieldType = TypePointer
                      (TypeTypedef
                        (TypedefSquashed
                          (CName "linked_list_B_t")
                          (TypeStruct
                            NamePair {
                              nameC = CName "linked_list_B_t",
                              nameHsIdent = HsIdentifier
                                "Linked_list_B_t"}))),
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
          [Eq, Show, Storable]}
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
                  "Linked_list_B_t",
                structConstr = HsName
                  "@NsConstr"
                  "Linked_list_B_t",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "linked_list_B_t_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "recursive_struct.h:10:7",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "linked_list_B_t_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "linked_list_B_t_next",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Linked_list_B_t")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "recursive_struct.h:11:20",
                        structFieldName = NamePair {
                          nameC = CName "next",
                          nameHsIdent = HsIdentifier
                            "linked_list_B_t_next"},
                        structFieldType = TypePointer
                          (TypeTypedef
                            (TypedefSquashed
                              (CName "linked_list_B_t")
                              (TypeStruct
                                NamePair {
                                  nameC = CName "linked_list_B_t",
                                  nameHsIdent = HsIdentifier
                                    "Linked_list_B_t"}))),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "recursive_struct.h:9:8",
                      declId = NamePair {
                        nameC = CName "linked_list_B_t",
                        nameHsIdent = HsIdentifier
                          "Linked_list_B_t"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Linked_list_B_t"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "recursive_struct.h:10:7",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "linked_list_B_t_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "recursive_struct.h:11:20",
                            structFieldName = NamePair {
                              nameC = CName "next",
                              nameHsIdent = HsIdentifier
                                "linked_list_B_t_next"},
                            structFieldType = TypePointer
                              (TypeTypedef
                                (TypedefSquashed
                                  (CName "linked_list_B_t")
                                  (TypeStruct
                                    NamePair {
                                      nameC = CName "linked_list_B_t",
                                      nameHsIdent = HsIdentifier
                                        "Linked_list_B_t"}))),
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
                  [Eq, Show, Storable]})
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
                  "Linked_list_B_t",
                structConstr = HsName
                  "@NsConstr"
                  "Linked_list_B_t",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "linked_list_B_t_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "recursive_struct.h:10:7",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "linked_list_B_t_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "linked_list_B_t_next",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Linked_list_B_t")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "recursive_struct.h:11:20",
                        structFieldName = NamePair {
                          nameC = CName "next",
                          nameHsIdent = HsIdentifier
                            "linked_list_B_t_next"},
                        structFieldType = TypePointer
                          (TypeTypedef
                            (TypedefSquashed
                              (CName "linked_list_B_t")
                              (TypeStruct
                                NamePair {
                                  nameC = CName "linked_list_B_t",
                                  nameHsIdent = HsIdentifier
                                    "Linked_list_B_t"}))),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "recursive_struct.h:9:8",
                      declId = NamePair {
                        nameC = CName "linked_list_B_t",
                        nameHsIdent = HsIdentifier
                          "Linked_list_B_t"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Linked_list_B_t"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "recursive_struct.h:10:7",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "linked_list_B_t_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "recursive_struct.h:11:20",
                            structFieldName = NamePair {
                              nameC = CName "next",
                              nameHsIdent = HsIdentifier
                                "linked_list_B_t_next"},
                            structFieldType = TypePointer
                              (TypeTypedef
                                (TypedefSquashed
                                  (CName "linked_list_B_t")
                                  (TypeStruct
                                    NamePair {
                                      nameC = CName "linked_list_B_t",
                                      nameHsIdent = HsIdentifier
                                        "Linked_list_B_t"}))),
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
                  [Eq, Show, Storable]}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    8
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Linked_list_B_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Linked_list_B_t")]
