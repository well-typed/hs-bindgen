[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "EnumA",
      newtypeConstr = HsName
        "@NsConstr"
        "EnumA",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_EnumA",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "nested_enums.h:2:14",
          declId = NamePair {
            nameC = CName "enumA",
            nameHsIdent = HsIdentifier
              "EnumA"},
          declOrigin = NameOriginInSource,
          declAliases = []},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "EnumA",
              newtypeField = HsName
                "@NsVar"
                "un_EnumA"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantLoc =
                "nested_enums.h:3:17",
                enumConstantName = NamePair {
                  nameC = CName "VALA_1",
                  nameHsIdent = HsIdentifier
                    "VALA_1"},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantLoc =
                "nested_enums.h:4:17",
                enumConstantName = NamePair {
                  nameC = CName "VALA_2",
                  nameHsIdent = HsIdentifier
                    "VALA_2"},
                enumConstantValue = 1}]},
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
          Read,
          Show,
          Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "EnumA",
        structConstr = HsName
          "@NsConstr"
          "EnumA",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumA",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
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
                  "EnumA",
                structConstr = HsName
                  "@NsConstr"
                  "EnumA",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_EnumA",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
                structInstances = Set.fromList
                  [
                    Eq,
                    Ord,
                    Read,
                    Show,
                    Storable]})
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
                  "EnumA",
                structConstr = HsName
                  "@NsConstr"
                  "EnumA",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_EnumA",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
                structInstances = Set.fromList
                  [Eq, Ord, Read, Show, Storable]}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "EnumA"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "EnumA"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "EnumA",
        structConstr = HsName
          "@NsConstr"
          "EnumA",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumA",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_ 0 (NE.fromList ["VALA_1"]),
          _×_ 1 (NE.fromList ["VALA_2"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "EnumA",
        structConstr = HsName
          "@NsConstr"
          "EnumA",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumA",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsName "@NsConstr" "VALA_1")
      (HsName "@NsConstr" "VALA_2")),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "EnumA",
        structConstr = HsName
          "@NsConstr"
          "EnumA",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumA",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclInstance
    (InstanceCEnumRead
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "EnumA",
        structConstr = HsName
          "@NsConstr"
          "EnumA",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumA",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "VALA_1",
      patSynType = HsName
        "@NsTypeConstr"
        "EnumA",
      patSynConstr = HsName
        "@NsConstr"
        "EnumA",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "nested_enums.h:3:17",
          enumConstantName = NamePair {
            nameC = CName "VALA_1",
            nameHsIdent = HsIdentifier
              "VALA_1"},
          enumConstantValue = 0}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "VALA_2",
      patSynType = HsName
        "@NsTypeConstr"
        "EnumA",
      patSynConstr = HsName
        "@NsConstr"
        "EnumA",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "nested_enums.h:4:17",
          enumConstantName = NamePair {
            nameC = CName "VALA_2",
            nameHsIdent = HsIdentifier
              "VALA_2"},
          enumConstantValue = 1}},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "ExA",
      structConstr = HsName
        "@NsConstr"
        "ExA",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "exA_fieldA1",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "EnumA"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "nested_enums.h:5:11",
              structFieldName = NamePair {
                nameC = CName "fieldA1",
                nameHsIdent = HsIdentifier
                  "exA_fieldA1"},
              structFieldType = TypeEnum
                NamePair {
                  nameC = CName "enumA",
                  nameHsIdent = HsIdentifier
                    "EnumA"}
                NameOriginInSource,
              structFieldOffset = 0,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "nested_enums.h:1:8",
            declId = NamePair {
              nameC = CName "exA",
              nameHsIdent = HsIdentifier
                "ExA"},
            declOrigin = NameOriginInSource,
            declAliases = []},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "ExA"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "nested_enums.h:5:11",
                  structFieldName = NamePair {
                    nameC = CName "fieldA1",
                    nameHsIdent = HsIdentifier
                      "exA_fieldA1"},
                  structFieldType = TypeEnum
                    NamePair {
                      nameC = CName "enumA",
                      nameHsIdent = HsIdentifier
                        "EnumA"}
                    NameOriginInSource,
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
          "ExA",
        structConstr = HsName
          "@NsConstr"
          "ExA",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "exA_fieldA1",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "EnumA"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "nested_enums.h:5:11",
                structFieldName = NamePair {
                  nameC = CName "fieldA1",
                  nameHsIdent = HsIdentifier
                    "exA_fieldA1"},
                structFieldType = TypeEnum
                  NamePair {
                    nameC = CName "enumA",
                    nameHsIdent = HsIdentifier
                      "EnumA"}
                  NameOriginInSource,
                structFieldOffset = 0,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "nested_enums.h:1:8",
              declId = NamePair {
                nameC = CName "exA",
                nameHsIdent = HsIdentifier
                  "ExA"},
              declOrigin = NameOriginInSource,
              declAliases = []},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "ExA"),
                structSizeof = 4,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "nested_enums.h:5:11",
                    structFieldName = NamePair {
                      nameC = CName "fieldA1",
                      nameHsIdent = HsIdentifier
                        "exA_fieldA1"},
                    structFieldType = TypeEnum
                      NamePair {
                        nameC = CName "enumA",
                        nameHsIdent = HsIdentifier
                          "EnumA"}
                      NameOriginInSource,
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
        storableSizeOf = 4,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "ExA",
                structConstr = HsName
                  "@NsConstr"
                  "ExA",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "exA_fieldA1",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "EnumA"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_enums.h:5:11",
                        structFieldName = NamePair {
                          nameC = CName "fieldA1",
                          nameHsIdent = HsIdentifier
                            "exA_fieldA1"},
                        structFieldType = TypeEnum
                          NamePair {
                            nameC = CName "enumA",
                            nameHsIdent = HsIdentifier
                              "EnumA"}
                          NameOriginInSource,
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "nested_enums.h:1:8",
                      declId = NamePair {
                        nameC = CName "exA",
                        nameHsIdent = HsIdentifier
                          "ExA"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "ExA"),
                        structSizeof = 4,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "nested_enums.h:5:11",
                            structFieldName = NamePair {
                              nameC = CName "fieldA1",
                              nameHsIdent = HsIdentifier
                                "exA_fieldA1"},
                            structFieldType = TypeEnum
                              NamePair {
                                nameC = CName "enumA",
                                nameHsIdent = HsIdentifier
                                  "EnumA"}
                              NameOriginInSource,
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
                  "ExA",
                structConstr = HsName
                  "@NsConstr"
                  "ExA",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "exA_fieldA1",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "EnumA"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_enums.h:5:11",
                        structFieldName = NamePair {
                          nameC = CName "fieldA1",
                          nameHsIdent = HsIdentifier
                            "exA_fieldA1"},
                        structFieldType = TypeEnum
                          NamePair {
                            nameC = CName "enumA",
                            nameHsIdent = HsIdentifier
                              "EnumA"}
                          NameOriginInSource,
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "nested_enums.h:1:8",
                      declId = NamePair {
                        nameC = CName "exA",
                        nameHsIdent = HsIdentifier
                          "ExA"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "ExA"),
                        structSizeof = 4,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "nested_enums.h:5:11",
                            structFieldName = NamePair {
                              nameC = CName "fieldA1",
                              nameHsIdent = HsIdentifier
                                "exA_fieldA1"},
                            structFieldType = TypeEnum
                              NamePair {
                                nameC = CName "enumA",
                                nameHsIdent = HsIdentifier
                                  "EnumA"}
                              NameOriginInSource,
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
    (HsName "@NsTypeConstr" "ExA"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "ExA"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "ExB_fieldB1",
      newtypeConstr = HsName
        "@NsConstr"
        "ExB_fieldB1",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_ExB_fieldB1",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "nested_enums.h:9:9",
          declId = NamePair {
            nameC = CName "exB_fieldB1",
            nameHsIdent = HsIdentifier
              "ExB_fieldB1"},
          declOrigin =
          NameOriginGenerated,
          declAliases = []},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "ExB_fieldB1",
              newtypeField = HsName
                "@NsVar"
                "un_ExB_fieldB1"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantLoc =
                "nested_enums.h:10:17",
                enumConstantName = NamePair {
                  nameC = CName "VALB_1",
                  nameHsIdent = HsIdentifier
                    "VALB_1"},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantLoc =
                "nested_enums.h:11:17",
                enumConstantName = NamePair {
                  nameC = CName "VALB_2",
                  nameHsIdent = HsIdentifier
                    "VALB_2"},
                enumConstantValue = 1}]},
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
          Read,
          Show,
          Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "ExB_fieldB1",
        structConstr = HsName
          "@NsConstr"
          "ExB_fieldB1",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_ExB_fieldB1",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
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
                  "ExB_fieldB1",
                structConstr = HsName
                  "@NsConstr"
                  "ExB_fieldB1",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_ExB_fieldB1",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
                structInstances = Set.fromList
                  [
                    Eq,
                    Ord,
                    Read,
                    Show,
                    Storable]})
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
                  "ExB_fieldB1",
                structConstr = HsName
                  "@NsConstr"
                  "ExB_fieldB1",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_ExB_fieldB1",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
                structInstances = Set.fromList
                  [Eq, Ord, Read, Show, Storable]}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "ExB_fieldB1"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "ExB_fieldB1"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "ExB_fieldB1",
        structConstr = HsName
          "@NsConstr"
          "ExB_fieldB1",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_ExB_fieldB1",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_ 0 (NE.fromList ["VALB_1"]),
          _×_ 1 (NE.fromList ["VALB_2"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "ExB_fieldB1",
        structConstr = HsName
          "@NsConstr"
          "ExB_fieldB1",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_ExB_fieldB1",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsName "@NsConstr" "VALB_1")
      (HsName "@NsConstr" "VALB_2")),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "ExB_fieldB1",
        structConstr = HsName
          "@NsConstr"
          "ExB_fieldB1",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_ExB_fieldB1",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclInstance
    (InstanceCEnumRead
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "ExB_fieldB1",
        structConstr = HsName
          "@NsConstr"
          "ExB_fieldB1",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_ExB_fieldB1",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "VALB_1",
      patSynType = HsName
        "@NsTypeConstr"
        "ExB_fieldB1",
      patSynConstr = HsName
        "@NsConstr"
        "ExB_fieldB1",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "nested_enums.h:10:17",
          enumConstantName = NamePair {
            nameC = CName "VALB_1",
            nameHsIdent = HsIdentifier
              "VALB_1"},
          enumConstantValue = 0}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "VALB_2",
      patSynType = HsName
        "@NsTypeConstr"
        "ExB_fieldB1",
      patSynConstr = HsName
        "@NsConstr"
        "ExB_fieldB1",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "nested_enums.h:11:17",
          enumConstantName = NamePair {
            nameC = CName "VALB_2",
            nameHsIdent = HsIdentifier
              "VALB_2"},
          enumConstantValue = 1}},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "ExB",
      structConstr = HsName
        "@NsConstr"
        "ExB",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "exB_fieldB1",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "ExB_fieldB1"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "nested_enums.h:12:11",
              structFieldName = NamePair {
                nameC = CName "fieldB1",
                nameHsIdent = HsIdentifier
                  "exB_fieldB1"},
              structFieldType = TypeEnum
                NamePair {
                  nameC = CName "exB_fieldB1",
                  nameHsIdent = HsIdentifier
                    "ExB_fieldB1"}
                NameOriginGenerated,
              structFieldOffset = 0,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "nested_enums.h:8:8",
            declId = NamePair {
              nameC = CName "exB",
              nameHsIdent = HsIdentifier
                "ExB"},
            declOrigin = NameOriginInSource,
            declAliases = []},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "ExB"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "nested_enums.h:12:11",
                  structFieldName = NamePair {
                    nameC = CName "fieldB1",
                    nameHsIdent = HsIdentifier
                      "exB_fieldB1"},
                  structFieldType = TypeEnum
                    NamePair {
                      nameC = CName "exB_fieldB1",
                      nameHsIdent = HsIdentifier
                        "ExB_fieldB1"}
                    NameOriginGenerated,
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
          "ExB",
        structConstr = HsName
          "@NsConstr"
          "ExB",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "exB_fieldB1",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "ExB_fieldB1"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "nested_enums.h:12:11",
                structFieldName = NamePair {
                  nameC = CName "fieldB1",
                  nameHsIdent = HsIdentifier
                    "exB_fieldB1"},
                structFieldType = TypeEnum
                  NamePair {
                    nameC = CName "exB_fieldB1",
                    nameHsIdent = HsIdentifier
                      "ExB_fieldB1"}
                  NameOriginGenerated,
                structFieldOffset = 0,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "nested_enums.h:8:8",
              declId = NamePair {
                nameC = CName "exB",
                nameHsIdent = HsIdentifier
                  "ExB"},
              declOrigin = NameOriginInSource,
              declAliases = []},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "ExB"),
                structSizeof = 4,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "nested_enums.h:12:11",
                    structFieldName = NamePair {
                      nameC = CName "fieldB1",
                      nameHsIdent = HsIdentifier
                        "exB_fieldB1"},
                    structFieldType = TypeEnum
                      NamePair {
                        nameC = CName "exB_fieldB1",
                        nameHsIdent = HsIdentifier
                          "ExB_fieldB1"}
                      NameOriginGenerated,
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
        storableSizeOf = 4,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "ExB",
                structConstr = HsName
                  "@NsConstr"
                  "ExB",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "exB_fieldB1",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "ExB_fieldB1"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_enums.h:12:11",
                        structFieldName = NamePair {
                          nameC = CName "fieldB1",
                          nameHsIdent = HsIdentifier
                            "exB_fieldB1"},
                        structFieldType = TypeEnum
                          NamePair {
                            nameC = CName "exB_fieldB1",
                            nameHsIdent = HsIdentifier
                              "ExB_fieldB1"}
                          NameOriginGenerated,
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "nested_enums.h:8:8",
                      declId = NamePair {
                        nameC = CName "exB",
                        nameHsIdent = HsIdentifier
                          "ExB"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "ExB"),
                        structSizeof = 4,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "nested_enums.h:12:11",
                            structFieldName = NamePair {
                              nameC = CName "fieldB1",
                              nameHsIdent = HsIdentifier
                                "exB_fieldB1"},
                            structFieldType = TypeEnum
                              NamePair {
                                nameC = CName "exB_fieldB1",
                                nameHsIdent = HsIdentifier
                                  "ExB_fieldB1"}
                              NameOriginGenerated,
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
                  "ExB",
                structConstr = HsName
                  "@NsConstr"
                  "ExB",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "exB_fieldB1",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "ExB_fieldB1"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "nested_enums.h:12:11",
                        structFieldName = NamePair {
                          nameC = CName "fieldB1",
                          nameHsIdent = HsIdentifier
                            "exB_fieldB1"},
                        structFieldType = TypeEnum
                          NamePair {
                            nameC = CName "exB_fieldB1",
                            nameHsIdent = HsIdentifier
                              "ExB_fieldB1"}
                          NameOriginGenerated,
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "nested_enums.h:8:8",
                      declId = NamePair {
                        nameC = CName "exB",
                        nameHsIdent = HsIdentifier
                          "ExB"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "ExB"),
                        structSizeof = 4,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "nested_enums.h:12:11",
                            structFieldName = NamePair {
                              nameC = CName "fieldB1",
                              nameHsIdent = HsIdentifier
                                "exB_fieldB1"},
                            structFieldType = TypeEnum
                              NamePair {
                                nameC = CName "exB_fieldB1",
                                nameHsIdent = HsIdentifier
                                  "ExB_fieldB1"}
                              NameOriginGenerated,
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
    (HsName "@NsTypeConstr" "ExB"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "ExB")]
