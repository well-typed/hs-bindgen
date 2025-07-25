[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Linked_list_A_t",
      structConstr = HsName
        "@NsConstr"
        "Linked_list_A_t",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "linked_list_A_t_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "recursive_struct.h:2:7",
              structFieldName = NamePair {
                nameC = Name "x",
                nameHsIdent = HsIdentifier
                  "linked_list_A_t_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing},
        Field {
          fieldName = HsName
            "@NsVar"
            "linked_list_A_t_next",
          fieldType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Linked_list_A_t")),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "recursive_struct.h:3:27",
              structFieldName = NamePair {
                nameC = Name "next",
                nameHsIdent = HsIdentifier
                  "linked_list_A_t_next"},
              structFieldType = TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "linked_list_A_t",
                    nameHsIdent = HsIdentifier
                      "Linked_list_A_t"}
                  (NameOriginRenamedFrom
                    (Name "linked_list_A_s"))),
              structFieldOffset = 64,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "recursive_struct.h:1:16",
            declId = NamePair {
              nameC = Name "linked_list_A_t",
              nameHsIdent = HsIdentifier
                "Linked_list_A_t"},
            declOrigin =
            NameOriginRenamedFrom
              (Name "linked_list_A_s"),
            declAliases = [
              Name "linked_list_A_t"],
            declHeader =
            "recursive_struct.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Linked_list_A_t"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "recursive_struct.h:2:7",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "linked_list_A_t_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
                StructField {
                  structFieldLoc =
                  "recursive_struct.h:3:27",
                  structFieldName = NamePair {
                    nameC = Name "next",
                    nameHsIdent = HsIdentifier
                      "linked_list_A_t_next"},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "linked_list_A_t",
                        nameHsIdent = HsIdentifier
                          "Linked_list_A_t"}
                      (NameOriginRenamedFrom
                        (Name "linked_list_A_s"))),
                  structFieldOffset = 64,
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
            "Linked_list_A_t",
          structConstr = HsName
            "@NsConstr"
            "Linked_list_A_t",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "linked_list_A_t_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc =
                  "recursive_struct.h:2:7",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "linked_list_A_t_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing},
            Field {
              fieldName = HsName
                "@NsVar"
                "linked_list_A_t_next",
              fieldType = HsPtr
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Linked_list_A_t")),
              fieldOrigin = StructField
                StructField {
                  structFieldLoc =
                  "recursive_struct.h:3:27",
                  structFieldName = NamePair {
                    nameC = Name "next",
                    nameHsIdent = HsIdentifier
                      "linked_list_A_t_next"},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "linked_list_A_t",
                        nameHsIdent = HsIdentifier
                          "Linked_list_A_t"}
                      (NameOriginRenamedFrom
                        (Name "linked_list_A_s"))),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "recursive_struct.h:1:16",
                declId = NamePair {
                  nameC = Name "linked_list_A_t",
                  nameHsIdent = HsIdentifier
                    "Linked_list_A_t"},
                declOrigin =
                NameOriginRenamedFrom
                  (Name "linked_list_A_s"),
                declAliases = [
                  Name "linked_list_A_t"],
                declHeader =
                "recursive_struct.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName
                      "@NsConstr"
                      "Linked_list_A_t"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldLoc =
                      "recursive_struct.h:2:7",
                      structFieldName = NamePair {
                        nameC = Name "x",
                        nameHsIdent = HsIdentifier
                          "linked_list_A_t_x"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing},
                    StructField {
                      structFieldLoc =
                      "recursive_struct.h:3:27",
                      structFieldName = NamePair {
                        nameC = Name "next",
                        nameHsIdent = HsIdentifier
                          "linked_list_A_t_next"},
                      structFieldType = TypePointer
                        (TypeStruct
                          NamePair {
                            nameC = Name "linked_list_A_t",
                            nameHsIdent = HsIdentifier
                              "Linked_list_A_t"}
                          (NameOriginRenamedFrom
                            (Name "linked_list_A_s"))),
                      structFieldOffset = 64,
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
          storableSizeOf = 16,
          storableAlignment = 8,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Linked_list_A_t",
                  structConstr = HsName
                    "@NsConstr"
                    "Linked_list_A_t",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "linked_list_A_t_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "recursive_struct.h:2:7",
                          structFieldName = NamePair {
                            nameC = Name "x",
                            nameHsIdent = HsIdentifier
                              "linked_list_A_t_x"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "linked_list_A_t_next",
                      fieldType = HsPtr
                        (HsTypRef
                          (HsName
                            "@NsTypeConstr"
                            "Linked_list_A_t")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "recursive_struct.h:3:27",
                          structFieldName = NamePair {
                            nameC = Name "next",
                            nameHsIdent = HsIdentifier
                              "linked_list_A_t_next"},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "linked_list_A_t",
                                nameHsIdent = HsIdentifier
                                  "Linked_list_A_t"}
                              (NameOriginRenamedFrom
                                (Name "linked_list_A_s"))),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "recursive_struct.h:1:16",
                        declId = NamePair {
                          nameC = Name "linked_list_A_t",
                          nameHsIdent = HsIdentifier
                            "Linked_list_A_t"},
                        declOrigin =
                        NameOriginRenamedFrom
                          (Name "linked_list_A_s"),
                        declAliases = [
                          Name "linked_list_A_t"],
                        declHeader =
                        "recursive_struct.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Linked_list_A_t"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "recursive_struct.h:2:7",
                              structFieldName = NamePair {
                                nameC = Name "x",
                                nameHsIdent = HsIdentifier
                                  "linked_list_A_t_x"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc =
                              "recursive_struct.h:3:27",
                              structFieldName = NamePair {
                                nameC = Name "next",
                                nameHsIdent = HsIdentifier
                                  "linked_list_A_t_next"},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "linked_list_A_t",
                                    nameHsIdent = HsIdentifier
                                      "Linked_list_A_t"}
                                  (NameOriginRenamedFrom
                                    (Name "linked_list_A_s"))),
                              structFieldOffset = 64,
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
                    "Linked_list_A_t",
                  structConstr = HsName
                    "@NsConstr"
                    "Linked_list_A_t",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "linked_list_A_t_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "recursive_struct.h:2:7",
                          structFieldName = NamePair {
                            nameC = Name "x",
                            nameHsIdent = HsIdentifier
                              "linked_list_A_t_x"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "linked_list_A_t_next",
                      fieldType = HsPtr
                        (HsTypRef
                          (HsName
                            "@NsTypeConstr"
                            "Linked_list_A_t")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "recursive_struct.h:3:27",
                          structFieldName = NamePair {
                            nameC = Name "next",
                            nameHsIdent = HsIdentifier
                              "linked_list_A_t_next"},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "linked_list_A_t",
                                nameHsIdent = HsIdentifier
                                  "Linked_list_A_t"}
                              (NameOriginRenamedFrom
                                (Name "linked_list_A_s"))),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "recursive_struct.h:1:16",
                        declId = NamePair {
                          nameC = Name "linked_list_A_t",
                          nameHsIdent = HsIdentifier
                            "Linked_list_A_t"},
                        declOrigin =
                        NameOriginRenamedFrom
                          (Name "linked_list_A_s"),
                        declAliases = [
                          Name "linked_list_A_t"],
                        declHeader =
                        "recursive_struct.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Linked_list_A_t"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "recursive_struct.h:2:7",
                              structFieldName = NamePair {
                                nameC = Name "x",
                                nameHsIdent = HsIdentifier
                                  "linked_list_A_t_x"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc =
                              "recursive_struct.h:3:27",
                              structFieldName = NamePair {
                                nameC = Name "next",
                                nameHsIdent = HsIdentifier
                                  "linked_list_A_t_next"},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "linked_list_A_t",
                                    nameHsIdent = HsIdentifier
                                      "Linked_list_A_t"}
                                  (NameOriginRenamedFrom
                                    (Name "linked_list_A_s"))),
                              structFieldOffset = 64,
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
        "Linked_list_A_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Linked_list_A_t",
      deriveInstanceComment =
      Nothing},
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
                nameC = Name "x",
                nameHsIdent = HsIdentifier
                  "linked_list_B_t_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing},
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
                nameC = Name "next",
                nameHsIdent = HsIdentifier
                  "linked_list_B_t_next"},
              structFieldType = TypePointer
                (TypeTypedef
                  (TypedefSquashed
                    (Name "linked_list_B_t")
                    (TypeStruct
                      NamePair {
                        nameC = Name "linked_list_B_t",
                        nameHsIdent = HsIdentifier
                          "Linked_list_B_t"}
                      NameOriginInSource))),
              structFieldOffset = 64,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "recursive_struct.h:9:8",
            declId = NamePair {
              nameC = Name "linked_list_B_t",
              nameHsIdent = HsIdentifier
                "Linked_list_B_t"},
            declOrigin = NameOriginInSource,
            declAliases = [
              Name "linked_list_B_t"],
            declHeader =
            "recursive_struct.h",
            declComment = Nothing},
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
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "linked_list_B_t_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
                StructField {
                  structFieldLoc =
                  "recursive_struct.h:11:20",
                  structFieldName = NamePair {
                    nameC = Name "next",
                    nameHsIdent = HsIdentifier
                      "linked_list_B_t_next"},
                  structFieldType = TypePointer
                    (TypeTypedef
                      (TypedefSquashed
                        (Name "linked_list_B_t")
                        (TypeStruct
                          NamePair {
                            nameC = Name "linked_list_B_t",
                            nameHsIdent = HsIdentifier
                              "Linked_list_B_t"}
                          NameOriginInSource))),
                  structFieldOffset = 64,
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
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "linked_list_B_t_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing},
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
                    nameC = Name "next",
                    nameHsIdent = HsIdentifier
                      "linked_list_B_t_next"},
                  structFieldType = TypePointer
                    (TypeTypedef
                      (TypedefSquashed
                        (Name "linked_list_B_t")
                        (TypeStruct
                          NamePair {
                            nameC = Name "linked_list_B_t",
                            nameHsIdent = HsIdentifier
                              "Linked_list_B_t"}
                          NameOriginInSource))),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "recursive_struct.h:9:8",
                declId = NamePair {
                  nameC = Name "linked_list_B_t",
                  nameHsIdent = HsIdentifier
                    "Linked_list_B_t"},
                declOrigin = NameOriginInSource,
                declAliases = [
                  Name "linked_list_B_t"],
                declHeader =
                "recursive_struct.h",
                declComment = Nothing},
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
                        nameC = Name "x",
                        nameHsIdent = HsIdentifier
                          "linked_list_B_t_x"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing},
                    StructField {
                      structFieldLoc =
                      "recursive_struct.h:11:20",
                      structFieldName = NamePair {
                        nameC = Name "next",
                        nameHsIdent = HsIdentifier
                          "linked_list_B_t_next"},
                      structFieldType = TypePointer
                        (TypeTypedef
                          (TypedefSquashed
                            (Name "linked_list_B_t")
                            (TypeStruct
                              NamePair {
                                nameC = Name "linked_list_B_t",
                                nameHsIdent = HsIdentifier
                                  "Linked_list_B_t"}
                              NameOriginInSource))),
                      structFieldOffset = 64,
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
                            nameC = Name "x",
                            nameHsIdent = HsIdentifier
                              "linked_list_B_t_x"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
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
                            nameC = Name "next",
                            nameHsIdent = HsIdentifier
                              "linked_list_B_t_next"},
                          structFieldType = TypePointer
                            (TypeTypedef
                              (TypedefSquashed
                                (Name "linked_list_B_t")
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "linked_list_B_t",
                                    nameHsIdent = HsIdentifier
                                      "Linked_list_B_t"}
                                  NameOriginInSource))),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "recursive_struct.h:9:8",
                        declId = NamePair {
                          nameC = Name "linked_list_B_t",
                          nameHsIdent = HsIdentifier
                            "Linked_list_B_t"},
                        declOrigin = NameOriginInSource,
                        declAliases = [
                          Name "linked_list_B_t"],
                        declHeader =
                        "recursive_struct.h",
                        declComment = Nothing},
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
                                nameC = Name "x",
                                nameHsIdent = HsIdentifier
                                  "linked_list_B_t_x"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc =
                              "recursive_struct.h:11:20",
                              structFieldName = NamePair {
                                nameC = Name "next",
                                nameHsIdent = HsIdentifier
                                  "linked_list_B_t_next"},
                              structFieldType = TypePointer
                                (TypeTypedef
                                  (TypedefSquashed
                                    (Name "linked_list_B_t")
                                    (TypeStruct
                                      NamePair {
                                        nameC = Name "linked_list_B_t",
                                        nameHsIdent = HsIdentifier
                                          "Linked_list_B_t"}
                                      NameOriginInSource))),
                              structFieldOffset = 64,
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
                            nameC = Name "x",
                            nameHsIdent = HsIdentifier
                              "linked_list_B_t_x"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
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
                            nameC = Name "next",
                            nameHsIdent = HsIdentifier
                              "linked_list_B_t_next"},
                          structFieldType = TypePointer
                            (TypeTypedef
                              (TypedefSquashed
                                (Name "linked_list_B_t")
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "linked_list_B_t",
                                    nameHsIdent = HsIdentifier
                                      "Linked_list_B_t"}
                                  NameOriginInSource))),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "recursive_struct.h:9:8",
                        declId = NamePair {
                          nameC = Name "linked_list_B_t",
                          nameHsIdent = HsIdentifier
                            "Linked_list_B_t"},
                        declOrigin = NameOriginInSource,
                        declAliases = [
                          Name "linked_list_B_t"],
                        declHeader =
                        "recursive_struct.h",
                        declComment = Nothing},
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
                                nameC = Name "x",
                                nameHsIdent = HsIdentifier
                                  "linked_list_B_t_x"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc =
                              "recursive_struct.h:11:20",
                              structFieldName = NamePair {
                                nameC = Name "next",
                                nameHsIdent = HsIdentifier
                                  "linked_list_B_t_next"},
                              structFieldType = TypePointer
                                (TypeTypedef
                                  (TypedefSquashed
                                    (Name "linked_list_B_t")
                                    (TypeStruct
                                      NamePair {
                                        nameC = Name "linked_list_B_t",
                                        nameHsIdent = HsIdentifier
                                          "Linked_list_B_t"}
                                      NameOriginInSource))),
                              structFieldOffset = 64,
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
        "Linked_list_B_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Linked_list_B_t",
      deriveInstanceComment =
      Nothing}]
