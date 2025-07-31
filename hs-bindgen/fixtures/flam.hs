[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Pascal",
      structConstr = HsName
        "@NsConstr"
        "Pascal",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "pascal_len",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "flam.h:3:9",
              structFieldName = NamePair {
                nameC = Name "len",
                nameHsIdent = HsIdentifier
                  "pascal_len"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "flam.h:2:8",
            declId = NamePair {
              nameC = Name "pascal",
              nameHsIdent = HsIdentifier
                "Pascal"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "flam.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Pascal"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc = "flam.h:3:9",
                  structFieldName = NamePair {
                    nameC = Name "len",
                    nameHsIdent = HsIdentifier
                      "pascal_len"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing}],
              structFlam = Just
                StructField {
                  structFieldLoc = "flam.h:4:10",
                  structFieldName = NamePair {
                    nameC = Name "data",
                    nameHsIdent = HsIdentifier
                      "pascal_data"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing}},
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
            "Pascal",
          structConstr = HsName
            "@NsConstr"
            "Pascal",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "pascal_len",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc = "flam.h:3:9",
                  structFieldName = NamePair {
                    nameC = Name "len",
                    nameHsIdent = HsIdentifier
                      "pascal_len"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "flam.h:2:8",
                declId = NamePair {
                  nameC = Name "pascal",
                  nameHsIdent = HsIdentifier
                    "Pascal"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "flam.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Pascal"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldLoc = "flam.h:3:9",
                      structFieldName = NamePair {
                        nameC = Name "len",
                        nameHsIdent = HsIdentifier
                          "pascal_len"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing}],
                  structFlam = Just
                    StructField {
                      structFieldLoc = "flam.h:4:10",
                      structFieldName = NamePair {
                        nameC = Name "data",
                        nameHsIdent = HsIdentifier
                          "pascal_data"},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing}},
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
                    "Pascal",
                  structConstr = HsName
                    "@NsConstr"
                    "Pascal",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "pascal_len",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "flam.h:3:9",
                          structFieldName = NamePair {
                            nameC = Name "len",
                            nameHsIdent = HsIdentifier
                              "pascal_len"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "flam.h:2:8",
                        declId = NamePair {
                          nameC = Name "pascal",
                          nameHsIdent = HsIdentifier
                            "Pascal"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "flam.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Pascal"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc = "flam.h:3:9",
                              structFieldName = NamePair {
                                nameC = Name "len",
                                nameHsIdent = HsIdentifier
                                  "pascal_len"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}],
                          structFlam = Just
                            StructField {
                              structFieldLoc = "flam.h:4:10",
                              structFieldName = NamePair {
                                nameC = Name "data",
                                nameHsIdent = HsIdentifier
                                  "pascal_data"},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}},
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
                    "Pascal",
                  structConstr = HsName
                    "@NsConstr"
                    "Pascal",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "pascal_len",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "flam.h:3:9",
                          structFieldName = NamePair {
                            nameC = Name "len",
                            nameHsIdent = HsIdentifier
                              "pascal_len"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "flam.h:2:8",
                        declId = NamePair {
                          nameC = Name "pascal",
                          nameHsIdent = HsIdentifier
                            "Pascal"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "flam.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Pascal"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc = "flam.h:3:9",
                              structFieldName = NamePair {
                                nameC = Name "len",
                                nameHsIdent = HsIdentifier
                                  "pascal_len"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}],
                          structFlam = Just
                            StructField {
                              structFieldLoc = "flam.h:4:10",
                              structFieldName = NamePair {
                                nameC = Name "data",
                                nameHsIdent = HsIdentifier
                                  "pascal_data"},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}},
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
        "Pascal",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Pascal",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasFLAM
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Pascal",
          structConstr = HsName
            "@NsConstr"
            "Pascal",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "pascal_len",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc = "flam.h:3:9",
                  structFieldName = NamePair {
                    nameC = Name "len",
                    nameHsIdent = HsIdentifier
                      "pascal_len"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "flam.h:2:8",
                declId = NamePair {
                  nameC = Name "pascal",
                  nameHsIdent = HsIdentifier
                    "Pascal"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "flam.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Pascal"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldLoc = "flam.h:3:9",
                      structFieldName = NamePair {
                        nameC = Name "len",
                        nameHsIdent = HsIdentifier
                          "pascal_len"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing}],
                  structFlam = Just
                    StructField {
                      structFieldLoc = "flam.h:4:10",
                      structFieldName = NamePair {
                        nameC = Name "data",
                        nameHsIdent = HsIdentifier
                          "pascal_data"},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing}},
              declSpec = DeclSpec
                TypeSpec {
                  typeSpecModule = Nothing,
                  typeSpecIdentifier = Nothing,
                  typeSpecInstances = Map.fromList
                    []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Nothing}
        (HsPrimType HsPrimCChar)
        4,
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Foo_bar",
      structConstr = HsName
        "@NsConstr"
        "Foo_bar",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "foo_bar_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "flam.h:11:7",
              structFieldName = NamePair {
                nameC = Name "x",
                nameHsIdent = HsIdentifier
                  "foo_bar_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing},
        Field {
          fieldName = HsName
            "@NsVar"
            "foo_bar_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "flam.h:12:7",
              structFieldName = NamePair {
                nameC = Name "y",
                nameHsIdent = HsIdentifier
                  "foo_bar_y"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "flam.h:10:2",
            declId = NamePair {
              nameC = Name "foo_bar",
              nameHsIdent = HsIdentifier
                "Foo_bar"},
            declOrigin = NameOriginGenerated
              (AnonId "flam.h:10:2"),
            declAliases = [],
            declHeader = "flam.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Foo_bar"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc = "flam.h:11:7",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "foo_bar_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
                StructField {
                  structFieldLoc = "flam.h:12:7",
                  structFieldName = NamePair {
                    nameC = Name "y",
                    nameHsIdent = HsIdentifier
                      "foo_bar_y"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
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
            "Foo_bar",
          structConstr = HsName
            "@NsConstr"
            "Foo_bar",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "foo_bar_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc = "flam.h:11:7",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "foo_bar_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing},
            Field {
              fieldName = HsName
                "@NsVar"
                "foo_bar_y",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc = "flam.h:12:7",
                  structFieldName = NamePair {
                    nameC = Name "y",
                    nameHsIdent = HsIdentifier
                      "foo_bar_y"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "flam.h:10:2",
                declId = NamePair {
                  nameC = Name "foo_bar",
                  nameHsIdent = HsIdentifier
                    "Foo_bar"},
                declOrigin = NameOriginGenerated
                  (AnonId "flam.h:10:2"),
                declAliases = [],
                declHeader = "flam.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Foo_bar"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldLoc = "flam.h:11:7",
                      structFieldName = NamePair {
                        nameC = Name "x",
                        nameHsIdent = HsIdentifier
                          "foo_bar_x"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing},
                    StructField {
                      structFieldLoc = "flam.h:12:7",
                      structFieldName = NamePair {
                        nameC = Name "y",
                        nameHsIdent = HsIdentifier
                          "foo_bar_y"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 32,
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
          storableSizeOf = 8,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Foo_bar",
                  structConstr = HsName
                    "@NsConstr"
                    "Foo_bar",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "foo_bar_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "flam.h:11:7",
                          structFieldName = NamePair {
                            nameC = Name "x",
                            nameHsIdent = HsIdentifier
                              "foo_bar_x"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "foo_bar_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "flam.h:12:7",
                          structFieldName = NamePair {
                            nameC = Name "y",
                            nameHsIdent = HsIdentifier
                              "foo_bar_y"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "flam.h:10:2",
                        declId = NamePair {
                          nameC = Name "foo_bar",
                          nameHsIdent = HsIdentifier
                            "Foo_bar"},
                        declOrigin = NameOriginGenerated
                          (AnonId "flam.h:10:2"),
                        declAliases = [],
                        declHeader = "flam.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Foo_bar"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc = "flam.h:11:7",
                              structFieldName = NamePair {
                                nameC = Name "x",
                                nameHsIdent = HsIdentifier
                                  "foo_bar_x"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc = "flam.h:12:7",
                              structFieldName = NamePair {
                                nameC = Name "y",
                                nameHsIdent = HsIdentifier
                                  "foo_bar_y"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
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
                    "Foo_bar",
                  structConstr = HsName
                    "@NsConstr"
                    "Foo_bar",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "foo_bar_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "flam.h:11:7",
                          structFieldName = NamePair {
                            nameC = Name "x",
                            nameHsIdent = HsIdentifier
                              "foo_bar_x"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "foo_bar_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "flam.h:12:7",
                          structFieldName = NamePair {
                            nameC = Name "y",
                            nameHsIdent = HsIdentifier
                              "foo_bar_y"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "flam.h:10:2",
                        declId = NamePair {
                          nameC = Name "foo_bar",
                          nameHsIdent = HsIdentifier
                            "Foo_bar"},
                        declOrigin = NameOriginGenerated
                          (AnonId "flam.h:10:2"),
                        declAliases = [],
                        declHeader = "flam.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Foo_bar"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc = "flam.h:11:7",
                              structFieldName = NamePair {
                                nameC = Name "x",
                                nameHsIdent = HsIdentifier
                                  "foo_bar_x"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc = "flam.h:12:7",
                              structFieldName = NamePair {
                                nameC = Name "y",
                                nameHsIdent = HsIdentifier
                                  "foo_bar_y"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
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
        "Foo_bar",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Foo_bar",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Foo",
      structConstr = HsName
        "@NsConstr"
        "Foo",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "foo_len",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "flam.h:9:6",
              structFieldName = NamePair {
                nameC = Name "len",
                nameHsIdent = HsIdentifier
                  "foo_len"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "flam.h:8:8",
            declId = NamePair {
              nameC = Name "foo",
              nameHsIdent = HsIdentifier
                "Foo"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "flam.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Foo"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc = "flam.h:9:6",
                  structFieldName = NamePair {
                    nameC = Name "len",
                    nameHsIdent = HsIdentifier
                      "foo_len"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing}],
              structFlam = Just
                StructField {
                  structFieldLoc = "flam.h:13:4",
                  structFieldName = NamePair {
                    nameC = Name "bar",
                    nameHsIdent = HsIdentifier
                      "foo_bar"},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "foo_bar",
                      nameHsIdent = HsIdentifier
                        "Foo_bar"}
                    (NameOriginGenerated
                      (AnonId "flam.h:10:2")),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing}},
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
            "Foo",
          structConstr = HsName
            "@NsConstr"
            "Foo",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "foo_len",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc = "flam.h:9:6",
                  structFieldName = NamePair {
                    nameC = Name "len",
                    nameHsIdent = HsIdentifier
                      "foo_len"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "flam.h:8:8",
                declId = NamePair {
                  nameC = Name "foo",
                  nameHsIdent = HsIdentifier
                    "Foo"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "flam.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Foo"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldLoc = "flam.h:9:6",
                      structFieldName = NamePair {
                        nameC = Name "len",
                        nameHsIdent = HsIdentifier
                          "foo_len"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing}],
                  structFlam = Just
                    StructField {
                      structFieldLoc = "flam.h:13:4",
                      structFieldName = NamePair {
                        nameC = Name "bar",
                        nameHsIdent = HsIdentifier
                          "foo_bar"},
                      structFieldType = TypeStruct
                        NamePair {
                          nameC = Name "foo_bar",
                          nameHsIdent = HsIdentifier
                            "Foo_bar"}
                        (NameOriginGenerated
                          (AnonId "flam.h:10:2")),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing}},
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
                    "Foo",
                  structConstr = HsName
                    "@NsConstr"
                    "Foo",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "foo_len",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "flam.h:9:6",
                          structFieldName = NamePair {
                            nameC = Name "len",
                            nameHsIdent = HsIdentifier
                              "foo_len"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "flam.h:8:8",
                        declId = NamePair {
                          nameC = Name "foo",
                          nameHsIdent = HsIdentifier
                            "Foo"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "flam.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Foo"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc = "flam.h:9:6",
                              structFieldName = NamePair {
                                nameC = Name "len",
                                nameHsIdent = HsIdentifier
                                  "foo_len"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}],
                          structFlam = Just
                            StructField {
                              structFieldLoc = "flam.h:13:4",
                              structFieldName = NamePair {
                                nameC = Name "bar",
                                nameHsIdent = HsIdentifier
                                  "foo_bar"},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "foo_bar",
                                  nameHsIdent = HsIdentifier
                                    "Foo_bar"}
                                (NameOriginGenerated
                                  (AnonId "flam.h:10:2")),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}},
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
                    "Foo",
                  structConstr = HsName
                    "@NsConstr"
                    "Foo",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "foo_len",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "flam.h:9:6",
                          structFieldName = NamePair {
                            nameC = Name "len",
                            nameHsIdent = HsIdentifier
                              "foo_len"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "flam.h:8:8",
                        declId = NamePair {
                          nameC = Name "foo",
                          nameHsIdent = HsIdentifier
                            "Foo"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "flam.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Foo"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc = "flam.h:9:6",
                              structFieldName = NamePair {
                                nameC = Name "len",
                                nameHsIdent = HsIdentifier
                                  "foo_len"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}],
                          structFlam = Just
                            StructField {
                              structFieldLoc = "flam.h:13:4",
                              structFieldName = NamePair {
                                nameC = Name "bar",
                                nameHsIdent = HsIdentifier
                                  "foo_bar"},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "foo_bar",
                                  nameHsIdent = HsIdentifier
                                    "Foo_bar"}
                                (NameOriginGenerated
                                  (AnonId "flam.h:10:2")),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}},
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
        "Foo",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Foo",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasFLAM
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Foo",
          structConstr = HsName
            "@NsConstr"
            "Foo",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "foo_len",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc = "flam.h:9:6",
                  structFieldName = NamePair {
                    nameC = Name "len",
                    nameHsIdent = HsIdentifier
                      "foo_len"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "flam.h:8:8",
                declId = NamePair {
                  nameC = Name "foo",
                  nameHsIdent = HsIdentifier
                    "Foo"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "flam.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Foo"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldLoc = "flam.h:9:6",
                      structFieldName = NamePair {
                        nameC = Name "len",
                        nameHsIdent = HsIdentifier
                          "foo_len"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing}],
                  structFlam = Just
                    StructField {
                      structFieldLoc = "flam.h:13:4",
                      structFieldName = NamePair {
                        nameC = Name "bar",
                        nameHsIdent = HsIdentifier
                          "foo_bar"},
                      structFieldType = TypeStruct
                        NamePair {
                          nameC = Name "foo_bar",
                          nameHsIdent = HsIdentifier
                            "Foo_bar"}
                        (NameOriginGenerated
                          (AnonId "flam.h:10:2")),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing}},
              declSpec = DeclSpec
                TypeSpec {
                  typeSpecModule = Nothing,
                  typeSpecIdentifier = Nothing,
                  typeSpecInstances = Map.fromList
                    []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Nothing}
        (HsTypRef
          (HsName
            "@NsTypeConstr"
            "Foo_bar"))
        4,
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Diff",
      structConstr = HsName
        "@NsConstr"
        "Diff",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "diff_first",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "flam.h:18:7",
              structFieldName = NamePair {
                nameC = Name "first",
                nameHsIdent = HsIdentifier
                  "diff_first"},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing},
        Field {
          fieldName = HsName
            "@NsVar"
            "diff_second",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "flam.h:19:7",
              structFieldName = NamePair {
                nameC = Name "second",
                nameHsIdent = HsIdentifier
                  "diff_second"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 64,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "flam.h:17:8",
            declId = NamePair {
              nameC = Name "diff",
              nameHsIdent = HsIdentifier
                "Diff"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "flam.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Diff"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc = "flam.h:18:7",
                  structFieldName = NamePair {
                    nameC = Name "first",
                    nameHsIdent = HsIdentifier
                      "diff_first"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
                StructField {
                  structFieldLoc = "flam.h:19:7",
                  structFieldName = NamePair {
                    nameC = Name "second",
                    nameHsIdent = HsIdentifier
                      "diff_second"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing}],
              structFlam = Just
                StructField {
                  structFieldLoc = "flam.h:20:7",
                  structFieldName = NamePair {
                    nameC = Name "flam",
                    nameHsIdent = HsIdentifier
                      "diff_flam"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 72,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing}},
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
            "Diff",
          structConstr = HsName
            "@NsConstr"
            "Diff",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "diff_first",
              fieldType = HsPrimType
                HsPrimCLong,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc = "flam.h:18:7",
                  structFieldName = NamePair {
                    nameC = Name "first",
                    nameHsIdent = HsIdentifier
                      "diff_first"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing},
            Field {
              fieldName = HsName
                "@NsVar"
                "diff_second",
              fieldType = HsPrimType
                HsPrimCChar,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc = "flam.h:19:7",
                  structFieldName = NamePair {
                    nameC = Name "second",
                    nameHsIdent = HsIdentifier
                      "diff_second"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "flam.h:17:8",
                declId = NamePair {
                  nameC = Name "diff",
                  nameHsIdent = HsIdentifier
                    "Diff"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "flam.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Diff"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldLoc = "flam.h:18:7",
                      structFieldName = NamePair {
                        nameC = Name "first",
                        nameHsIdent = HsIdentifier
                          "diff_first"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimLong Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing},
                    StructField {
                      structFieldLoc = "flam.h:19:7",
                      structFieldName = NamePair {
                        nameC = Name "second",
                        nameHsIdent = HsIdentifier
                          "diff_second"},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 64,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing}],
                  structFlam = Just
                    StructField {
                      structFieldLoc = "flam.h:20:7",
                      structFieldName = NamePair {
                        nameC = Name "flam",
                        nameHsIdent = HsIdentifier
                          "diff_flam"},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 72,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing}},
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
                    "Diff",
                  structConstr = HsName
                    "@NsConstr"
                    "Diff",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "diff_first",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "flam.h:18:7",
                          structFieldName = NamePair {
                            nameC = Name "first",
                            nameHsIdent = HsIdentifier
                              "diff_first"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "diff_second",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "flam.h:19:7",
                          structFieldName = NamePair {
                            nameC = Name "second",
                            nameHsIdent = HsIdentifier
                              "diff_second"},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "flam.h:17:8",
                        declId = NamePair {
                          nameC = Name "diff",
                          nameHsIdent = HsIdentifier
                            "Diff"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "flam.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Diff"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldLoc = "flam.h:18:7",
                              structFieldName = NamePair {
                                nameC = Name "first",
                                nameHsIdent = HsIdentifier
                                  "diff_first"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc = "flam.h:19:7",
                              structFieldName = NamePair {
                                nameC = Name "second",
                                nameHsIdent = HsIdentifier
                                  "diff_second"},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}],
                          structFlam = Just
                            StructField {
                              structFieldLoc = "flam.h:20:7",
                              structFieldName = NamePair {
                                nameC = Name "flam",
                                nameHsIdent = HsIdentifier
                                  "diff_flam"},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 72,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}},
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
                    "Diff",
                  structConstr = HsName
                    "@NsConstr"
                    "Diff",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "diff_first",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "flam.h:18:7",
                          structFieldName = NamePair {
                            nameC = Name "first",
                            nameHsIdent = HsIdentifier
                              "diff_first"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "diff_second",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "flam.h:19:7",
                          structFieldName = NamePair {
                            nameC = Name "second",
                            nameHsIdent = HsIdentifier
                              "diff_second"},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "flam.h:17:8",
                        declId = NamePair {
                          nameC = Name "diff",
                          nameHsIdent = HsIdentifier
                            "Diff"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "flam.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Diff"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldLoc = "flam.h:18:7",
                              structFieldName = NamePair {
                                nameC = Name "first",
                                nameHsIdent = HsIdentifier
                                  "diff_first"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc = "flam.h:19:7",
                              structFieldName = NamePair {
                                nameC = Name "second",
                                nameHsIdent = HsIdentifier
                                  "diff_second"},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}],
                          structFlam = Just
                            StructField {
                              structFieldLoc = "flam.h:20:7",
                              structFieldName = NamePair {
                                nameC = Name "flam",
                                nameHsIdent = HsIdentifier
                                  "diff_flam"},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 72,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}},
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
        "Diff",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Diff",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasFLAM
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Diff",
          structConstr = HsName
            "@NsConstr"
            "Diff",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "diff_first",
              fieldType = HsPrimType
                HsPrimCLong,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc = "flam.h:18:7",
                  structFieldName = NamePair {
                    nameC = Name "first",
                    nameHsIdent = HsIdentifier
                      "diff_first"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing},
            Field {
              fieldName = HsName
                "@NsVar"
                "diff_second",
              fieldType = HsPrimType
                HsPrimCChar,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc = "flam.h:19:7",
                  structFieldName = NamePair {
                    nameC = Name "second",
                    nameHsIdent = HsIdentifier
                      "diff_second"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "flam.h:17:8",
                declId = NamePair {
                  nameC = Name "diff",
                  nameHsIdent = HsIdentifier
                    "Diff"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "flam.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Diff"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldLoc = "flam.h:18:7",
                      structFieldName = NamePair {
                        nameC = Name "first",
                        nameHsIdent = HsIdentifier
                          "diff_first"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimLong Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing},
                    StructField {
                      structFieldLoc = "flam.h:19:7",
                      structFieldName = NamePair {
                        nameC = Name "second",
                        nameHsIdent = HsIdentifier
                          "diff_second"},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 64,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing}],
                  structFlam = Just
                    StructField {
                      structFieldLoc = "flam.h:20:7",
                      structFieldName = NamePair {
                        nameC = Name "flam",
                        nameHsIdent = HsIdentifier
                          "diff_flam"},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 72,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing}},
              declSpec = DeclSpec
                TypeSpec {
                  typeSpecModule = Nothing,
                  typeSpecIdentifier = Nothing,
                  typeSpecInstances = Map.fromList
                    []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Nothing}
        (HsPrimType HsPrimCChar)
        9,
      defineInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Triplets",
      structConstr = HsName
        "@NsConstr"
        "Triplets",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "triplets_len",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "flam.h:27:7",
              structFieldName = NamePair {
                nameC = Name "len",
                nameHsIdent = HsIdentifier
                  "triplets_len"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
      structOrigin =
      Just
        Decl {
          declInfo =
          DeclInfo {
            declLoc = "flam.h:26:8",
            declId = NamePair {
              nameC = Name "triplets",
              nameHsIdent = HsIdentifier
                "Triplets"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "flam.h",
            declComment =
            Just
              Comment {
                commentCName = "triplets",
                commentChildren =
                [
                  Paragraph
                    [
                      TextContent
                        "The flexible array member is a multi-dimensional array of unknown size. In",
                      TextContent
                        "particular, it is a is an array of unknown size, where each element is of",
                      TextContent
                        "type length-3-array-of-int."]]}},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Triplets"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc = "flam.h:27:7",
                  structFieldName = NamePair {
                    nameC = Name "len",
                    nameHsIdent = HsIdentifier
                      "triplets_len"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing}],
              structFlam = Just
                StructField {
                  structFieldLoc = "flam.h:28:7",
                  structFieldName = NamePair {
                    nameC = Name "flam",
                    nameHsIdent = HsIdentifier
                      "triplets_flam"},
                  structFieldType = TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral PrimInt Signed)),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing}},
          declSpec = DeclSpec
            TypeSpec {
              typeSpecModule = Nothing,
              typeSpecIdentifier = Nothing,
              typeSpecInstances = Map.fromList
                []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment =
      Just
        Comment {
          commentTitle =
          Just
            [
              TextContent
                "The flexible array member is a multi-dimensional array of unknown size. In",
              TextContent
                "particular, it is a is an array of unknown size, where each element is of",
              TextContent
                "type length-3-array-of-int."],
          commentOrigin = Just "triplets",
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Triplets",
          structConstr = HsName
            "@NsConstr"
            "Triplets",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "triplets_len",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc = "flam.h:27:7",
                  structFieldName = NamePair {
                    nameC = Name "len",
                    nameHsIdent = HsIdentifier
                      "triplets_len"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin =
          Just
            Decl {
              declInfo =
              DeclInfo {
                declLoc = "flam.h:26:8",
                declId = NamePair {
                  nameC = Name "triplets",
                  nameHsIdent = HsIdentifier
                    "Triplets"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "flam.h",
                declComment =
                Just
                  Comment {
                    commentCName = "triplets",
                    commentChildren =
                    [
                      Paragraph
                        [
                          TextContent
                            "The flexible array member is a multi-dimensional array of unknown size. In",
                          TextContent
                            "particular, it is a is an array of unknown size, where each element is of",
                          TextContent
                            "type length-3-array-of-int."]]}},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Triplets"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldLoc = "flam.h:27:7",
                      structFieldName = NamePair {
                        nameC = Name "len",
                        nameHsIdent = HsIdentifier
                          "triplets_len"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing}],
                  structFlam = Just
                    StructField {
                      structFieldLoc = "flam.h:28:7",
                      structFieldName = NamePair {
                        nameC = Name "flam",
                        nameHsIdent = HsIdentifier
                          "triplets_flam"},
                      structFieldType = TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral PrimInt Signed)),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing}},
              declSpec = DeclSpec
                TypeSpec {
                  typeSpecModule = Nothing,
                  typeSpecIdentifier = Nothing,
                  typeSpecInstances = Map.fromList
                    []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment =
          Just
            Comment {
              commentTitle =
              Just
                [
                  TextContent
                    "The flexible array member is a multi-dimensional array of unknown size. In",
                  TextContent
                    "particular, it is a is an array of unknown size, where each element is of",
                  TextContent
                    "type length-3-array-of-int."],
              commentOrigin = Just "triplets",
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 4,
          storableAlignment = 4,
          storablePeek =
          Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Triplets",
                  structConstr = HsName
                    "@NsConstr"
                    "Triplets",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "triplets_len",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "flam.h:27:7",
                          structFieldName = NamePair {
                            nameC = Name "len",
                            nameHsIdent = HsIdentifier
                              "triplets_len"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin =
                  Just
                    Decl {
                      declInfo =
                      DeclInfo {
                        declLoc = "flam.h:26:8",
                        declId = NamePair {
                          nameC = Name "triplets",
                          nameHsIdent = HsIdentifier
                            "Triplets"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "flam.h",
                        declComment =
                        Just
                          Comment {
                            commentCName = "triplets",
                            commentChildren =
                            [
                              Paragraph
                                [
                                  TextContent
                                    "The flexible array member is a multi-dimensional array of unknown size. In",
                                  TextContent
                                    "particular, it is a is an array of unknown size, where each element is of",
                                  TextContent
                                    "type length-3-array-of-int."]]}},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Triplets"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc = "flam.h:27:7",
                              structFieldName = NamePair {
                                nameC = Name "len",
                                nameHsIdent = HsIdentifier
                                  "triplets_len"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}],
                          structFlam = Just
                            StructField {
                              structFieldLoc = "flam.h:28:7",
                              structFieldName = NamePair {
                                nameC = Name "flam",
                                nameHsIdent = HsIdentifier
                                  "triplets_flam"},
                              structFieldType = TypeConstArray
                                3
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment =
                  Just
                    Comment {
                      commentTitle =
                      Just
                        [
                          TextContent
                            "The flexible array member is a multi-dimensional array of unknown size. In",
                          TextContent
                            "particular, it is a is an array of unknown size, where each element is of",
                          TextContent
                            "type length-3-array-of-int."],
                      commentOrigin = Just "triplets",
                      commentChildren = []}})
              [PeekByteOff (Idx 0) 0]),
          storablePoke =
          Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Triplets",
                  structConstr = HsName
                    "@NsConstr"
                    "Triplets",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "triplets_len",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc = "flam.h:27:7",
                          structFieldName = NamePair {
                            nameC = Name "len",
                            nameHsIdent = HsIdentifier
                              "triplets_len"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin =
                  Just
                    Decl {
                      declInfo =
                      DeclInfo {
                        declLoc = "flam.h:26:8",
                        declId = NamePair {
                          nameC = Name "triplets",
                          nameHsIdent = HsIdentifier
                            "Triplets"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "flam.h",
                        declComment =
                        Just
                          Comment {
                            commentCName = "triplets",
                            commentChildren =
                            [
                              Paragraph
                                [
                                  TextContent
                                    "The flexible array member is a multi-dimensional array of unknown size. In",
                                  TextContent
                                    "particular, it is a is an array of unknown size, where each element is of",
                                  TextContent
                                    "type length-3-array-of-int."]]}},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Triplets"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc = "flam.h:27:7",
                              structFieldName = NamePair {
                                nameC = Name "len",
                                nameHsIdent = HsIdentifier
                                  "triplets_len"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}],
                          structFlam = Just
                            StructField {
                              structFieldLoc = "flam.h:28:7",
                              structFieldName = NamePair {
                                nameC = Name "flam",
                                nameHsIdent = HsIdentifier
                                  "triplets_flam"},
                              structFieldType = TypeConstArray
                                3
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment =
                  Just
                    Comment {
                      commentTitle =
                      Just
                        [
                          TextContent
                            "The flexible array member is a multi-dimensional array of unknown size. In",
                          TextContent
                            "particular, it is a is an array of unknown size, where each element is of",
                          TextContent
                            "type length-3-array-of-int."],
                      commentOrigin = Just "triplets",
                      commentChildren = []}}
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
        "Triplets",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Triplets",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasFLAM
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Triplets",
          structConstr = HsName
            "@NsConstr"
            "Triplets",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "triplets_len",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc = "flam.h:27:7",
                  structFieldName = NamePair {
                    nameC = Name "len",
                    nameHsIdent = HsIdentifier
                      "triplets_len"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin =
          Just
            Decl {
              declInfo =
              DeclInfo {
                declLoc = "flam.h:26:8",
                declId = NamePair {
                  nameC = Name "triplets",
                  nameHsIdent = HsIdentifier
                    "Triplets"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "flam.h",
                declComment =
                Just
                  Comment {
                    commentCName = "triplets",
                    commentChildren =
                    [
                      Paragraph
                        [
                          TextContent
                            "The flexible array member is a multi-dimensional array of unknown size. In",
                          TextContent
                            "particular, it is a is an array of unknown size, where each element is of",
                          TextContent
                            "type length-3-array-of-int."]]}},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Triplets"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldLoc = "flam.h:27:7",
                      structFieldName = NamePair {
                        nameC = Name "len",
                        nameHsIdent = HsIdentifier
                          "triplets_len"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing}],
                  structFlam = Just
                    StructField {
                      structFieldLoc = "flam.h:28:7",
                      structFieldName = NamePair {
                        nameC = Name "flam",
                        nameHsIdent = HsIdentifier
                          "triplets_flam"},
                      structFieldType = TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral PrimInt Signed)),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing}},
              declSpec = DeclSpec
                TypeSpec {
                  typeSpecModule = Nothing,
                  typeSpecIdentifier = Nothing,
                  typeSpecInstances = Map.fromList
                    []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment =
          Just
            Comment {
              commentTitle =
              Just
                [
                  TextContent
                    "The flexible array member is a multi-dimensional array of unknown size. In",
                  TextContent
                    "particular, it is a is an array of unknown size, where each element is of",
                  TextContent
                    "type length-3-array-of-int."],
              commentOrigin = Just "triplets",
              commentChildren = []}}
        (HsConstArray
          3
          (HsPrimType HsPrimCInt))
        4,
      defineInstanceComment =
      Nothing}]
