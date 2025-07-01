[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Foo",
      newtypeConstr = HsName
        "@NsConstr"
        "Foo",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Foo",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typenames.h:14:6",
          declId = NamePair {
            nameC = CName "foo",
            nameHsIdent = HsIdentifier
              "Foo"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "typenames.h"},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Foo",
              newtypeField = HsName
                "@NsVar"
                "un_Foo"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantLoc =
                "typenames.h:15:2",
                enumConstantName = NamePair {
                  nameC = CName "FOO1",
                  nameHsIdent = HsIdentifier
                    "FOO1"},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantLoc =
                "typenames.h:16:2",
                enumConstantName = NamePair {
                  nameC = CName "FOO2",
                  nameHsIdent = HsIdentifier
                    "FOO2"},
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
          "Foo",
        structConstr = HsName
          "@NsConstr"
          "Foo",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Foo",
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
                  "Foo",
                structConstr = HsName
                  "@NsConstr"
                  "Foo",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Foo",
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
                  "Foo",
                structConstr = HsName
                  "@NsConstr"
                  "Foo",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Foo",
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
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "Foo"),
  DeclInstance
    (InstanceCEnum
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
              "un_Foo",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_ 0 (NE.fromList ["FOO1"]),
          _×_ 1 (NE.fromList ["FOO2"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
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
              "un_Foo",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsName "@NsConstr" "FOO1")
      (HsName "@NsConstr" "FOO2")),
  DeclInstance
    (InstanceCEnumShow
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
              "un_Foo",
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
          "Foo",
        structConstr = HsName
          "@NsConstr"
          "Foo",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Foo",
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
        "FOO1",
      patSynType = HsName
        "@NsTypeConstr"
        "Foo",
      patSynConstr = HsName
        "@NsConstr"
        "Foo",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "typenames.h:15:2",
          enumConstantName = NamePair {
            nameC = CName "FOO1",
            nameHsIdent = HsIdentifier
              "FOO1"},
          enumConstantValue = 0}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "FOO2",
      patSynType = HsName
        "@NsTypeConstr"
        "Foo",
      patSynConstr = HsName
        "@NsConstr"
        "Foo",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "typenames.h:16:2",
          enumConstantName = NamePair {
            nameC = CName "FOO2",
            nameHsIdent = HsIdentifier
              "FOO2"},
          enumConstantValue = 1}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Foo",
      newtypeConstr = HsName
        "@NsConstr"
        "Foo",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Foo",
        fieldType = HsPrimType
          HsPrimCDouble,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typenames.h:19:16",
          declId = NamePair {
            nameC = CName "foo",
            nameHsIdent = HsIdentifier
              "Foo"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "typenames.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Foo",
              newtypeField = HsName
                "@NsVar"
                "un_Foo"},
            typedefType = TypePrim
              (PrimFloating PrimDouble)},
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
          Read,
          Show,
          Floating,
          Fractional,
          Num,
          Real,
          RealFloat,
          RealFrac,
          Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveNewtype
    Floating
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveNewtype
    Fractional
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveNewtype
    RealFloat
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveNewtype
    RealFrac
    (HsName "@NsTypeConstr" "Foo")]
