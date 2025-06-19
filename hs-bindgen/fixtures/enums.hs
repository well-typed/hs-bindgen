[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "First",
      newtypeConstr = HsName
        "@NsConstr"
        "First",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_First",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "enums.h:4:6",
          declId = NamePair {
            nameC = CName "first",
            nameHsIdent = HsIdentifier
              "First"},
          declOrigin = NameOriginInSource,
          declAliases = []},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "First",
              newtypeField = HsName
                "@NsVar"
                "un_First"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantLoc = "enums.h:5:5",
                enumConstantName = NamePair {
                  nameC = CName "FIRST1",
                  nameHsIdent = HsIdentifier
                    "FIRST1"},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantLoc = "enums.h:6:5",
                enumConstantName = NamePair {
                  nameC = CName "FIRST2",
                  nameHsIdent = HsIdentifier
                    "FIRST2"},
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
          "First",
        structConstr = HsName
          "@NsConstr"
          "First",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_First",
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
                  "First",
                structConstr = HsName
                  "@NsConstr"
                  "First",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_First",
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
                  "First",
                structConstr = HsName
                  "@NsConstr"
                  "First",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_First",
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
      "First"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "First"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "First",
        structConstr = HsName
          "@NsConstr"
          "First",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_First",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_ 0 (NE.fromList ["FIRST1"]),
          _×_ 1 (NE.fromList ["FIRST2"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "First",
        structConstr = HsName
          "@NsConstr"
          "First",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_First",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsName "@NsConstr" "FIRST1")
      (HsName "@NsConstr" "FIRST2")),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "First",
        structConstr = HsName
          "@NsConstr"
          "First",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_First",
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
          "First",
        structConstr = HsName
          "@NsConstr"
          "First",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_First",
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
        "FIRST1",
      patSynType = HsName
        "@NsTypeConstr"
        "First",
      patSynConstr = HsName
        "@NsConstr"
        "First",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc = "enums.h:5:5",
          enumConstantName = NamePair {
            nameC = CName "FIRST1",
            nameHsIdent = HsIdentifier
              "FIRST1"},
          enumConstantValue = 0}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "FIRST2",
      patSynType = HsName
        "@NsTypeConstr"
        "First",
      patSynConstr = HsName
        "@NsConstr"
        "First",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc = "enums.h:6:5",
          enumConstantName = NamePair {
            nameC = CName "FIRST2",
            nameHsIdent = HsIdentifier
              "FIRST2"},
          enumConstantValue = 1}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Second",
      newtypeConstr = HsName
        "@NsConstr"
        "Second",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Second",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "enums.h:9:6",
          declId = NamePair {
            nameC = CName "second",
            nameHsIdent = HsIdentifier
              "Second"},
          declOrigin = NameOriginInSource,
          declAliases = []},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Second",
              newtypeField = HsName
                "@NsVar"
                "un_Second"},
            enumType = TypePrim
              (PrimIntegral PrimInt Signed),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantLoc =
                "enums.h:10:5",
                enumConstantName = NamePair {
                  nameC = CName "SECOND_A",
                  nameHsIdent = HsIdentifier
                    "SECOND_A"},
                enumConstantValue = `-1`},
              EnumConstant {
                enumConstantLoc =
                "enums.h:11:5",
                enumConstantName = NamePair {
                  nameC = CName "SECOND_B",
                  nameHsIdent = HsIdentifier
                    "SECOND_B"},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantLoc =
                "enums.h:12:5",
                enumConstantName = NamePair {
                  nameC = CName "SECOND_C",
                  nameHsIdent = HsIdentifier
                    "SECOND_C"},
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
          "Second",
        structConstr = HsName
          "@NsConstr"
          "Second",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Second",
            fieldType = HsPrimType
              HsPrimCInt,
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
                  "Second",
                structConstr = HsName
                  "@NsConstr"
                  "Second",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Second",
                    fieldType = HsPrimType
                      HsPrimCInt,
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
                  "Second",
                structConstr = HsName
                  "@NsConstr"
                  "Second",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Second",
                    fieldType = HsPrimType
                      HsPrimCInt,
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
      "Second"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Second"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Second",
        structConstr = HsName
          "@NsConstr"
          "Second",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Second",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsPrimType HsPrimCInt)
      (Map.fromList
        [
          _×_
            `-1`
            (NE.fromList ["SECOND_A"]),
          _×_
            0
            (NE.fromList ["SECOND_B"]),
          _×_
            1
            (NE.fromList ["SECOND_C"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Second",
        structConstr = HsName
          "@NsConstr"
          "Second",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Second",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsName "@NsConstr" "SECOND_A")
      (HsName
        "@NsConstr"
        "SECOND_C")),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Second",
        structConstr = HsName
          "@NsConstr"
          "Second",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Second",
            fieldType = HsPrimType
              HsPrimCInt,
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
          "Second",
        structConstr = HsName
          "@NsConstr"
          "Second",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Second",
            fieldType = HsPrimType
              HsPrimCInt,
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
        "SECOND_A",
      patSynType = HsName
        "@NsTypeConstr"
        "Second",
      patSynConstr = HsName
        "@NsConstr"
        "Second",
      patSynValue = `-1`,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "enums.h:10:5",
          enumConstantName = NamePair {
            nameC = CName "SECOND_A",
            nameHsIdent = HsIdentifier
              "SECOND_A"},
          enumConstantValue = `-1`}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "SECOND_B",
      patSynType = HsName
        "@NsTypeConstr"
        "Second",
      patSynConstr = HsName
        "@NsConstr"
        "Second",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "enums.h:11:5",
          enumConstantName = NamePair {
            nameC = CName "SECOND_B",
            nameHsIdent = HsIdentifier
              "SECOND_B"},
          enumConstantValue = 0}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "SECOND_C",
      patSynType = HsName
        "@NsTypeConstr"
        "Second",
      patSynConstr = HsName
        "@NsConstr"
        "Second",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "enums.h:12:5",
          enumConstantName = NamePair {
            nameC = CName "SECOND_C",
            nameHsIdent = HsIdentifier
              "SECOND_C"},
          enumConstantValue = 1}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Same",
      newtypeConstr = HsName
        "@NsConstr"
        "Same",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Same",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "enums.h:15:6",
          declId = NamePair {
            nameC = CName "same",
            nameHsIdent = HsIdentifier
              "Same"},
          declOrigin = NameOriginInSource,
          declAliases = []},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Same",
              newtypeField = HsName
                "@NsVar"
                "un_Same"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantLoc =
                "enums.h:16:5",
                enumConstantName = NamePair {
                  nameC = CName "SAME_A",
                  nameHsIdent = HsIdentifier
                    "SAME_A"},
                enumConstantValue = 1},
              EnumConstant {
                enumConstantLoc =
                "enums.h:17:5",
                enumConstantName = NamePair {
                  nameC = CName "SAME_B",
                  nameHsIdent = HsIdentifier
                    "SAME_B"},
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
          "Same",
        structConstr = HsName
          "@NsConstr"
          "Same",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Same",
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
                  "Same",
                structConstr = HsName
                  "@NsConstr"
                  "Same",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Same",
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
                  "Same",
                structConstr = HsName
                  "@NsConstr"
                  "Same",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Same",
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
    (HsName "@NsTypeConstr" "Same"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "Same"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Same",
        structConstr = HsName
          "@NsConstr"
          "Same",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Same",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_
            1
            (NE.fromList
              ["SAME_A", "SAME_B"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Same",
        structConstr = HsName
          "@NsConstr"
          "Same",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Same",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsName "@NsConstr" "SAME_A")
      (HsName "@NsConstr" "SAME_A")),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Same",
        structConstr = HsName
          "@NsConstr"
          "Same",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Same",
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
          "Same",
        structConstr = HsName
          "@NsConstr"
          "Same",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Same",
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
        "SAME_A",
      patSynType = HsName
        "@NsTypeConstr"
        "Same",
      patSynConstr = HsName
        "@NsConstr"
        "Same",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "enums.h:16:5",
          enumConstantName = NamePair {
            nameC = CName "SAME_A",
            nameHsIdent = HsIdentifier
              "SAME_A"},
          enumConstantValue = 1}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "SAME_B",
      patSynType = HsName
        "@NsTypeConstr"
        "Same",
      patSynConstr = HsName
        "@NsConstr"
        "Same",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "enums.h:17:5",
          enumConstantName = NamePair {
            nameC = CName "SAME_B",
            nameHsIdent = HsIdentifier
              "SAME_B"},
          enumConstantValue = 1}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Nonseq",
      newtypeConstr = HsName
        "@NsConstr"
        "Nonseq",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Nonseq",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "enums.h:20:6",
          declId = NamePair {
            nameC = CName "nonseq",
            nameHsIdent = HsIdentifier
              "Nonseq"},
          declOrigin = NameOriginInSource,
          declAliases = []},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Nonseq",
              newtypeField = HsName
                "@NsVar"
                "un_Nonseq"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantLoc =
                "enums.h:21:5",
                enumConstantName = NamePair {
                  nameC = CName "NONSEQ_A",
                  nameHsIdent = HsIdentifier
                    "NONSEQ_A"},
                enumConstantValue = 200},
              EnumConstant {
                enumConstantLoc =
                "enums.h:22:5",
                enumConstantName = NamePair {
                  nameC = CName "NONSEQ_B",
                  nameHsIdent = HsIdentifier
                    "NONSEQ_B"},
                enumConstantValue = 301},
              EnumConstant {
                enumConstantLoc =
                "enums.h:23:5",
                enumConstantName = NamePair {
                  nameC = CName "NONSEQ_C",
                  nameHsIdent = HsIdentifier
                    "NONSEQ_C"},
                enumConstantValue = 404}]},
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
          "Nonseq",
        structConstr = HsName
          "@NsConstr"
          "Nonseq",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Nonseq",
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
                  "Nonseq",
                structConstr = HsName
                  "@NsConstr"
                  "Nonseq",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Nonseq",
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
                  "Nonseq",
                structConstr = HsName
                  "@NsConstr"
                  "Nonseq",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Nonseq",
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
      "Nonseq"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Nonseq"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Nonseq",
        structConstr = HsName
          "@NsConstr"
          "Nonseq",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Nonseq",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_
            200
            (NE.fromList ["NONSEQ_A"]),
          _×_
            301
            (NE.fromList ["NONSEQ_B"]),
          _×_
            404
            (NE.fromList ["NONSEQ_C"])])
      False),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Nonseq",
        structConstr = HsName
          "@NsConstr"
          "Nonseq",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Nonseq",
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
          "Nonseq",
        structConstr = HsName
          "@NsConstr"
          "Nonseq",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Nonseq",
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
        "NONSEQ_A",
      patSynType = HsName
        "@NsTypeConstr"
        "Nonseq",
      patSynConstr = HsName
        "@NsConstr"
        "Nonseq",
      patSynValue = 200,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "enums.h:21:5",
          enumConstantName = NamePair {
            nameC = CName "NONSEQ_A",
            nameHsIdent = HsIdentifier
              "NONSEQ_A"},
          enumConstantValue = 200}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "NONSEQ_B",
      patSynType = HsName
        "@NsTypeConstr"
        "Nonseq",
      patSynConstr = HsName
        "@NsConstr"
        "Nonseq",
      patSynValue = 301,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "enums.h:22:5",
          enumConstantName = NamePair {
            nameC = CName "NONSEQ_B",
            nameHsIdent = HsIdentifier
              "NONSEQ_B"},
          enumConstantValue = 301}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "NONSEQ_C",
      patSynType = HsName
        "@NsTypeConstr"
        "Nonseq",
      patSynConstr = HsName
        "@NsConstr"
        "Nonseq",
      patSynValue = 404,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "enums.h:23:5",
          enumConstantName = NamePair {
            nameC = CName "NONSEQ_C",
            nameHsIdent = HsIdentifier
              "NONSEQ_C"},
          enumConstantValue = 404}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Packed",
      newtypeConstr = HsName
        "@NsConstr"
        "Packed",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Packed",
        fieldType = HsPrimType
          HsPrimCUChar,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "enums.h:26:6",
          declId = NamePair {
            nameC = CName "packed",
            nameHsIdent = HsIdentifier
              "Packed"},
          declOrigin = NameOriginInSource,
          declAliases = []},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Packed",
              newtypeField = HsName
                "@NsVar"
                "un_Packed"},
            enumType = TypePrim
              (PrimChar
                (PrimSignExplicit Unsigned)),
            enumSizeof = 1,
            enumAlignment = 1,
            enumConstants = [
              EnumConstant {
                enumConstantLoc =
                "enums.h:27:5",
                enumConstantName = NamePair {
                  nameC = CName "PACKED_A",
                  nameHsIdent = HsIdentifier
                    "PACKED_A"},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantLoc =
                "enums.h:27:15",
                enumConstantName = NamePair {
                  nameC = CName "PACKED_B",
                  nameHsIdent = HsIdentifier
                    "PACKED_B"},
                enumConstantValue = 1},
              EnumConstant {
                enumConstantLoc =
                "enums.h:27:25",
                enumConstantName = NamePair {
                  nameC = CName "PACKED_C",
                  nameHsIdent = HsIdentifier
                    "PACKED_C"},
                enumConstantValue = 2}]},
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
          "Packed",
        structConstr = HsName
          "@NsConstr"
          "Packed",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Packed",
            fieldType = HsPrimType
              HsPrimCUChar,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      StorableInstance {
        storableSizeOf = 1,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Packed",
                structConstr = HsName
                  "@NsConstr"
                  "Packed",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Packed",
                    fieldType = HsPrimType
                      HsPrimCUChar,
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
                  "Packed",
                structConstr = HsName
                  "@NsConstr"
                  "Packed",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Packed",
                    fieldType = HsPrimType
                      HsPrimCUChar,
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
      "Packed"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Packed"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Packed",
        structConstr = HsName
          "@NsConstr"
          "Packed",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Packed",
            fieldType = HsPrimType
              HsPrimCUChar,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsPrimType HsPrimCUChar)
      (Map.fromList
        [
          _×_
            0
            (NE.fromList ["PACKED_A"]),
          _×_
            1
            (NE.fromList ["PACKED_B"]),
          _×_
            2
            (NE.fromList ["PACKED_C"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Packed",
        structConstr = HsName
          "@NsConstr"
          "Packed",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Packed",
            fieldType = HsPrimType
              HsPrimCUChar,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsName "@NsConstr" "PACKED_A")
      (HsName
        "@NsConstr"
        "PACKED_C")),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Packed",
        structConstr = HsName
          "@NsConstr"
          "Packed",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Packed",
            fieldType = HsPrimType
              HsPrimCUChar,
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
          "Packed",
        structConstr = HsName
          "@NsConstr"
          "Packed",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Packed",
            fieldType = HsPrimType
              HsPrimCUChar,
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
        "PACKED_A",
      patSynType = HsName
        "@NsTypeConstr"
        "Packed",
      patSynConstr = HsName
        "@NsConstr"
        "Packed",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "enums.h:27:5",
          enumConstantName = NamePair {
            nameC = CName "PACKED_A",
            nameHsIdent = HsIdentifier
              "PACKED_A"},
          enumConstantValue = 0}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "PACKED_B",
      patSynType = HsName
        "@NsTypeConstr"
        "Packed",
      patSynConstr = HsName
        "@NsConstr"
        "Packed",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "enums.h:27:15",
          enumConstantName = NamePair {
            nameC = CName "PACKED_B",
            nameHsIdent = HsIdentifier
              "PACKED_B"},
          enumConstantValue = 1}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "PACKED_C",
      patSynType = HsName
        "@NsTypeConstr"
        "Packed",
      patSynConstr = HsName
        "@NsConstr"
        "Packed",
      patSynValue = 2,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "enums.h:27:25",
          enumConstantName = NamePair {
            nameC = CName "PACKED_C",
            nameHsIdent = HsIdentifier
              "PACKED_C"},
          enumConstantValue = 2}},
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
          declLoc = "enums.h:30:9",
          declId = NamePair {
            nameC = CName "enumA",
            nameHsIdent = HsIdentifier
              "EnumA"},
          declOrigin = NameOriginGenerated
            (AnonId "enums.h:30:9"),
          declAliases = [CName "enumA"]},
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
                "enums.h:30:16",
                enumConstantName = NamePair {
                  nameC = CName "A_FOO",
                  nameHsIdent = HsIdentifier
                    "A_FOO"},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantLoc =
                "enums.h:30:23",
                enumConstantName = NamePair {
                  nameC = CName "A_BAR",
                  nameHsIdent = HsIdentifier
                    "A_BAR"},
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
          _×_ 0 (NE.fromList ["A_FOO"]),
          _×_ 1 (NE.fromList ["A_BAR"])])
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
      (HsName "@NsConstr" "A_FOO")
      (HsName "@NsConstr" "A_BAR")),
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
        "A_FOO",
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
          "enums.h:30:16",
          enumConstantName = NamePair {
            nameC = CName "A_FOO",
            nameHsIdent = HsIdentifier
              "A_FOO"},
          enumConstantValue = 0}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "A_BAR",
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
          "enums.h:30:23",
          enumConstantName = NamePair {
            nameC = CName "A_BAR",
            nameHsIdent = HsIdentifier
              "A_BAR"},
          enumConstantValue = 1}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "EnumB",
      newtypeConstr = HsName
        "@NsConstr"
        "EnumB",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_EnumB",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "enums.h:32:14",
          declId = NamePair {
            nameC = CName "enumB",
            nameHsIdent = HsIdentifier
              "EnumB"},
          declOrigin = NameOriginInSource,
          declAliases = [CName "enumB"]},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "EnumB",
              newtypeField = HsName
                "@NsVar"
                "un_EnumB"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantLoc =
                "enums.h:32:22",
                enumConstantName = NamePair {
                  nameC = CName "B_FOO",
                  nameHsIdent = HsIdentifier
                    "B_FOO"},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantLoc =
                "enums.h:32:29",
                enumConstantName = NamePair {
                  nameC = CName "B_BAR",
                  nameHsIdent = HsIdentifier
                    "B_BAR"},
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
          "EnumB",
        structConstr = HsName
          "@NsConstr"
          "EnumB",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumB",
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
                  "EnumB",
                structConstr = HsName
                  "@NsConstr"
                  "EnumB",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_EnumB",
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
                  "EnumB",
                structConstr = HsName
                  "@NsConstr"
                  "EnumB",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_EnumB",
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
      "EnumB"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "EnumB"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "EnumB",
        structConstr = HsName
          "@NsConstr"
          "EnumB",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumB",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_ 0 (NE.fromList ["B_FOO"]),
          _×_ 1 (NE.fromList ["B_BAR"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "EnumB",
        structConstr = HsName
          "@NsConstr"
          "EnumB",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumB",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsName "@NsConstr" "B_FOO")
      (HsName "@NsConstr" "B_BAR")),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "EnumB",
        structConstr = HsName
          "@NsConstr"
          "EnumB",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumB",
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
          "EnumB",
        structConstr = HsName
          "@NsConstr"
          "EnumB",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumB",
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
        "B_FOO",
      patSynType = HsName
        "@NsTypeConstr"
        "EnumB",
      patSynConstr = HsName
        "@NsConstr"
        "EnumB",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "enums.h:32:22",
          enumConstantName = NamePair {
            nameC = CName "B_FOO",
            nameHsIdent = HsIdentifier
              "B_FOO"},
          enumConstantValue = 0}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "B_BAR",
      patSynType = HsName
        "@NsTypeConstr"
        "EnumB",
      patSynConstr = HsName
        "@NsConstr"
        "EnumB",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "enums.h:32:29",
          enumConstantName = NamePair {
            nameC = CName "B_BAR",
            nameHsIdent = HsIdentifier
              "B_BAR"},
          enumConstantValue = 1}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "EnumC",
      newtypeConstr = HsName
        "@NsConstr"
        "EnumC",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_EnumC",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "enums.h:34:6",
          declId = NamePair {
            nameC = CName "enumC",
            nameHsIdent = HsIdentifier
              "EnumC"},
          declOrigin = NameOriginInSource,
          declAliases = [CName "enumC"]},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "EnumC",
              newtypeField = HsName
                "@NsVar"
                "un_EnumC"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantLoc =
                "enums.h:34:14",
                enumConstantName = NamePair {
                  nameC = CName "C_FOO",
                  nameHsIdent = HsIdentifier
                    "C_FOO"},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantLoc =
                "enums.h:34:21",
                enumConstantName = NamePair {
                  nameC = CName "C_BAR",
                  nameHsIdent = HsIdentifier
                    "C_BAR"},
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
          "EnumC",
        structConstr = HsName
          "@NsConstr"
          "EnumC",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumC",
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
                  "EnumC",
                structConstr = HsName
                  "@NsConstr"
                  "EnumC",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_EnumC",
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
                  "EnumC",
                structConstr = HsName
                  "@NsConstr"
                  "EnumC",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_EnumC",
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
      "EnumC"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "EnumC"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "EnumC",
        structConstr = HsName
          "@NsConstr"
          "EnumC",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumC",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_ 0 (NE.fromList ["C_FOO"]),
          _×_ 1 (NE.fromList ["C_BAR"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "EnumC",
        structConstr = HsName
          "@NsConstr"
          "EnumC",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumC",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsName "@NsConstr" "C_FOO")
      (HsName "@NsConstr" "C_BAR")),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "EnumC",
        structConstr = HsName
          "@NsConstr"
          "EnumC",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumC",
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
          "EnumC",
        structConstr = HsName
          "@NsConstr"
          "EnumC",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumC",
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
        "C_FOO",
      patSynType = HsName
        "@NsTypeConstr"
        "EnumC",
      patSynConstr = HsName
        "@NsConstr"
        "EnumC",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "enums.h:34:14",
          enumConstantName = NamePair {
            nameC = CName "C_FOO",
            nameHsIdent = HsIdentifier
              "C_FOO"},
          enumConstantValue = 0}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "C_BAR",
      patSynType = HsName
        "@NsTypeConstr"
        "EnumC",
      patSynConstr = HsName
        "@NsConstr"
        "EnumC",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "enums.h:34:21",
          enumConstantName = NamePair {
            nameC = CName "C_BAR",
            nameHsIdent = HsIdentifier
              "C_BAR"},
          enumConstantValue = 1}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "EnumD",
      newtypeConstr = HsName
        "@NsConstr"
        "EnumD",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_EnumD",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "enums.h:37:6",
          declId = NamePair {
            nameC = CName "enumD",
            nameHsIdent = HsIdentifier
              "EnumD"},
          declOrigin = NameOriginInSource,
          declAliases = [
            CName "enumD_t"]},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "EnumD",
              newtypeField = HsName
                "@NsVar"
                "un_EnumD"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantLoc =
                "enums.h:37:14",
                enumConstantName = NamePair {
                  nameC = CName "D_FOO",
                  nameHsIdent = HsIdentifier
                    "D_FOO"},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantLoc =
                "enums.h:37:21",
                enumConstantName = NamePair {
                  nameC = CName "D_BAR",
                  nameHsIdent = HsIdentifier
                    "D_BAR"},
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
          "EnumD",
        structConstr = HsName
          "@NsConstr"
          "EnumD",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumD",
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
                  "EnumD",
                structConstr = HsName
                  "@NsConstr"
                  "EnumD",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_EnumD",
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
                  "EnumD",
                structConstr = HsName
                  "@NsConstr"
                  "EnumD",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_EnumD",
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
      "EnumD"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "EnumD"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "EnumD",
        structConstr = HsName
          "@NsConstr"
          "EnumD",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumD",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_ 0 (NE.fromList ["D_FOO"]),
          _×_ 1 (NE.fromList ["D_BAR"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "EnumD",
        structConstr = HsName
          "@NsConstr"
          "EnumD",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumD",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsName "@NsConstr" "D_FOO")
      (HsName "@NsConstr" "D_BAR")),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "EnumD",
        structConstr = HsName
          "@NsConstr"
          "EnumD",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumD",
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
          "EnumD",
        structConstr = HsName
          "@NsConstr"
          "EnumD",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumD",
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
        "D_FOO",
      patSynType = HsName
        "@NsTypeConstr"
        "EnumD",
      patSynConstr = HsName
        "@NsConstr"
        "EnumD",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "enums.h:37:14",
          enumConstantName = NamePair {
            nameC = CName "D_FOO",
            nameHsIdent = HsIdentifier
              "D_FOO"},
          enumConstantValue = 0}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "D_BAR",
      patSynType = HsName
        "@NsTypeConstr"
        "EnumD",
      patSynConstr = HsName
        "@NsConstr"
        "EnumD",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "enums.h:37:21",
          enumConstantName = NamePair {
            nameC = CName "D_BAR",
            nameHsIdent = HsIdentifier
              "D_BAR"},
          enumConstantValue = 1}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "EnumD_t",
      newtypeConstr = HsName
        "@NsConstr"
        "EnumD_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_EnumD_t",
        fieldType = HsTypRef
          (HsName
            "@NsTypeConstr"
            "EnumD"),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "enums.h:38:20",
          declId = NamePair {
            nameC = CName "enumD_t",
            nameHsIdent = HsIdentifier
              "EnumD_t"},
          declOrigin = NameOriginInSource,
          declAliases = []},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "EnumD_t",
              newtypeField = HsName
                "@NsVar"
                "un_EnumD_t"},
            typedefType = TypeEnum
              NamePair {
                nameC = CName "enumD",
                nameHsIdent = HsIdentifier
                  "EnumD"}
              NameOriginInSource},
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
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "EnumD_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "EnumD_t"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "EnumD_t"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "EnumD_t"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "EnumD_t")]
