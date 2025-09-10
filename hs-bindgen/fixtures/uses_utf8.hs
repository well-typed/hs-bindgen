[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "MyEnum",
      newtypeConstr = HsName
        "@NsConstr"
        "MyEnum",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_MyEnum",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "uses_utf8.h:4:6",
          declId = NamePair {
            nameC = Name "MyEnum",
            nameHsIdent = HsIdentifier
              "MyEnum"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "uses_utf8.h",
          declComment = Nothing},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "MyEnum",
              newtypeField = HsName
                "@NsVar"
                "un_MyEnum"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "uses_utf8.h:5:9",
                  fieldName = NamePair {
                    nameC = Name "Say\20320\22909",
                    nameHsIdent = HsIdentifier
                      "Say\20320\22909"},
                  fieldComment = Nothing},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "uses_utf8.h:6:9",
                  fieldName = NamePair {
                    nameC = Name "Say\25308\25308",
                    nameHsIdent = HsIdentifier
                      "Say\25308\25308"},
                  fieldComment = Nothing},
                enumConstantValue = 1}]},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Read, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "MyEnum",
          commentLocation = Just
            "uses_utf8.h:4:6",
          commentHeader = Just
            "uses_utf8.h",
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "MyEnum",
          structConstr = HsName
            "@NsConstr"
            "MyEnum",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_MyEnum",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
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
                    "MyEnum",
                  structConstr = HsName
                    "@NsConstr"
                    "MyEnum",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "un_MyEnum",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = GeneratedField,
                      fieldComment = Nothing}],
                  structOrigin = Nothing,
                  structInstances = Set.fromList
                    [Eq, Ord, Read, Show, Storable],
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
                    "MyEnum",
                  structConstr = HsName
                    "@NsConstr"
                    "MyEnum",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "un_MyEnum",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = GeneratedField,
                      fieldComment = Nothing}],
                  structOrigin = Nothing,
                  structInstances = Set.fromList
                    [Eq, Ord, Read, Show, Storable],
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
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MyEnum",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MyEnum",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "MyEnum",
          structConstr = HsName
            "@NsConstr"
            "MyEnum",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_MyEnum",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (HsPrimType HsPrimCUInt)
        (Map.fromList
          [
            _×_
              0
              (NE.fromList
                ["Say\20320\22909"]),
            _×_
              1
              (NE.fromList
                ["Say\25308\25308"])])
        True,
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceSequentialCEnum
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "MyEnum",
          structConstr = HsName
            "@NsConstr"
            "MyEnum",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_MyEnum",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (HsName
          "@NsConstr"
          "Say\20320\22909")
        (HsName
          "@NsConstr"
          "Say\25308\25308"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "MyEnum",
          structConstr = HsName
            "@NsConstr"
            "MyEnum",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_MyEnum",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumRead
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "MyEnum",
          structConstr = HsName
            "@NsConstr"
            "MyEnum",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_MyEnum",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing},
      defineInstanceComment =
      Nothing},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Say\20320\22909",
      patSynType = HsName
        "@NsTypeConstr"
        "MyEnum",
      patSynConstr = HsName
        "@NsConstr"
        "MyEnum",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "uses_utf8.h:5:9",
            fieldName = NamePair {
              nameC = Name "Say\20320\22909",
              nameHsIdent = HsIdentifier
                "Say\20320\22909"},
            fieldComment = Nothing},
          enumConstantValue = 0},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "Say\20320\22909",
          commentLocation = Just
            "uses_utf8.h:5:9",
          commentHeader = Just
            "uses_utf8.h",
          commentChildren = []}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Say\25308\25308",
      patSynType = HsName
        "@NsTypeConstr"
        "MyEnum",
      patSynConstr = HsName
        "@NsConstr"
        "MyEnum",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "uses_utf8.h:6:9",
            fieldName = NamePair {
              nameC = Name "Say\25308\25308",
              nameHsIdent = HsIdentifier
                "Say\25308\25308"},
            fieldComment = Nothing},
          enumConstantValue = 1},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "Say\25308\25308",
          commentLocation = Just
            "uses_utf8.h:6:9",
          commentHeader = Just
            "uses_utf8.h",
          commentChildren = []}}]
