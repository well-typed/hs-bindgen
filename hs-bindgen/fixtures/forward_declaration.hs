[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S1_t",
      structConstr = HsName
        "@NsConstr"
        "S1_t",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s1_t_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "forward_declaration.h:4:7",
              structFieldName = NamePair {
                nameC = Name "a",
                nameHsIdent = HsIdentifier
                  "s1_t_a"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "forward_declaration.h:3:8",
            declId = NamePair {
              nameC = Name "S1_t",
              nameHsIdent = HsIdentifier
                "S1_t"},
            declOrigin =
            NameOriginRenamedFrom
              (Name "S1"),
            declAliases = [Name "S1_t"],
            declHeader =
            "forward_declaration.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "S1_t"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "forward_declaration.h:4:7",
                  structFieldName = NamePair {
                    nameC = Name "a",
                    nameHsIdent = HsIdentifier
                      "s1_t_a"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
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
            "S1_t",
          structConstr = HsName
            "@NsConstr"
            "S1_t",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "s1_t_a",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc =
                  "forward_declaration.h:4:7",
                  structFieldName = NamePair {
                    nameC = Name "a",
                    nameHsIdent = HsIdentifier
                      "s1_t_a"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "forward_declaration.h:3:8",
                declId = NamePair {
                  nameC = Name "S1_t",
                  nameHsIdent = HsIdentifier
                    "S1_t"},
                declOrigin =
                NameOriginRenamedFrom
                  (Name "S1"),
                declAliases = [Name "S1_t"],
                declHeader =
                "forward_declaration.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "S1_t"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldLoc =
                      "forward_declaration.h:4:7",
                      structFieldName = NamePair {
                        nameC = Name "a",
                        nameHsIdent = HsIdentifier
                          "s1_t_a"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
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
          storableSizeOf = 4,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "S1_t",
                  structConstr = HsName
                    "@NsConstr"
                    "S1_t",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "s1_t_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "forward_declaration.h:4:7",
                          structFieldName = NamePair {
                            nameC = Name "a",
                            nameHsIdent = HsIdentifier
                              "s1_t_a"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "forward_declaration.h:3:8",
                        declId = NamePair {
                          nameC = Name "S1_t",
                          nameHsIdent = HsIdentifier
                            "S1_t"},
                        declOrigin =
                        NameOriginRenamedFrom
                          (Name "S1"),
                        declAliases = [Name "S1_t"],
                        declHeader =
                        "forward_declaration.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "S1_t"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "forward_declaration.h:4:7",
                              structFieldName = NamePair {
                                nameC = Name "a",
                                nameHsIdent = HsIdentifier
                                  "s1_t_a"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
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
                    "S1_t",
                  structConstr = HsName
                    "@NsConstr"
                    "S1_t",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "s1_t_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "forward_declaration.h:4:7",
                          structFieldName = NamePair {
                            nameC = Name "a",
                            nameHsIdent = HsIdentifier
                              "s1_t_a"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "forward_declaration.h:3:8",
                        declId = NamePair {
                          nameC = Name "S1_t",
                          nameHsIdent = HsIdentifier
                            "S1_t"},
                        declOrigin =
                        NameOriginRenamedFrom
                          (Name "S1"),
                        declAliases = [Name "S1_t"],
                        declHeader =
                        "forward_declaration.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "S1_t"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "forward_declaration.h:4:7",
                              structFieldName = NamePair {
                                nameC = Name "a",
                                nameHsIdent = HsIdentifier
                                  "s1_t_a"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
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
                (Add 1)
                (Seq
                  [
                    PokeByteOff
                      (Idx 2)
                      0
                      (Idx 0)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S1_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S1_t",
      deriveInstanceComment =
      Nothing},
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
            "s2_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "forward_declaration.h:10:7",
              structFieldName = NamePair {
                nameC = Name "a",
                nameHsIdent = HsIdentifier
                  "s2_a"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "forward_declaration.h:9:8",
            declId = NamePair {
              nameC = Name "S2",
              nameHsIdent = HsIdentifier
                "S2"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "forward_declaration.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "S2"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "forward_declaration.h:10:7",
                  structFieldName = NamePair {
                    nameC = Name "a",
                    nameHsIdent = HsIdentifier
                      "s2_a"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
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
            "S2",
          structConstr = HsName
            "@NsConstr"
            "S2",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "s2_a",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc =
                  "forward_declaration.h:10:7",
                  structFieldName = NamePair {
                    nameC = Name "a",
                    nameHsIdent = HsIdentifier
                      "s2_a"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "forward_declaration.h:9:8",
                declId = NamePair {
                  nameC = Name "S2",
                  nameHsIdent = HsIdentifier
                    "S2"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader =
                "forward_declaration.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "S2"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldLoc =
                      "forward_declaration.h:10:7",
                      structFieldName = NamePair {
                        nameC = Name "a",
                        nameHsIdent = HsIdentifier
                          "s2_a"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
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
          storableSizeOf = 4,
          storableAlignment = 4,
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
                        "s2_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "forward_declaration.h:10:7",
                          structFieldName = NamePair {
                            nameC = Name "a",
                            nameHsIdent = HsIdentifier
                              "s2_a"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "forward_declaration.h:9:8",
                        declId = NamePair {
                          nameC = Name "S2",
                          nameHsIdent = HsIdentifier
                            "S2"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "forward_declaration.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "S2"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "forward_declaration.h:10:7",
                              structFieldName = NamePair {
                                nameC = Name "a",
                                nameHsIdent = HsIdentifier
                                  "s2_a"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
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
                        "s2_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "forward_declaration.h:10:7",
                          structFieldName = NamePair {
                            nameC = Name "a",
                            nameHsIdent = HsIdentifier
                              "s2_a"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "forward_declaration.h:9:8",
                        declId = NamePair {
                          nameC = Name "S2",
                          nameHsIdent = HsIdentifier
                            "S2"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "forward_declaration.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "S2"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "forward_declaration.h:10:7",
                              structFieldName = NamePair {
                                nameC = Name "a",
                                nameHsIdent = HsIdentifier
                                  "s2_a"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
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
                (Add 1)
                (Seq
                  [
                    PokeByteOff
                      (Idx 2)
                      0
                      (Idx 0)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S2",
      deriveInstanceComment =
      Nothing}]
