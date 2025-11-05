[
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "MyEnum",
      newtypeConstr = Name
        "@NsConstr"
        "MyEnum",
      newtypeField = Field {
        fieldName = Name
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
            nameHsIdent = Identifier
              "MyEnum"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["uses_utf8.h"],
              headerInclude = "uses_utf8.h"},
          declComment = Nothing},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "MyEnum",
              newtypeField = Name
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
                    nameHsIdent = Identifier
                      "Say\20320\22909"},
                  fieldComment = Nothing},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "uses_utf8.h:6:9",
                  fieldName = NamePair {
                    nameC = Name "Say\25308\25308",
                    nameHsIdent = Identifier
                      "Say\25308\25308"},
                  fieldComment = Nothing},
                enumConstantValue = 1}]},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Read, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "MyEnum",
          commentLocation = Just
            "uses_utf8.h:4:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["uses_utf8.h"],
              headerInclude = "uses_utf8.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "MyEnum",
          structConstr = Name
            "@NsConstr"
            "MyEnum",
          structFields = [
            Field {
              fieldName = Name
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
                  structName = Name
                    "@NsTypeConstr"
                    "MyEnum",
                  structConstr = Name
                    "@NsConstr"
                    "MyEnum",
                  structFields = [
                    Field {
                      fieldName = Name
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
                  structName = Name
                    "@NsTypeConstr"
                    "MyEnum",
                  structConstr = Name
                    "@NsConstr"
                    "MyEnum",
                  structFields = [
                    Field {
                      fieldName = Name
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
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MyEnum",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MyEnum",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
        Struct {
          structName = Name
            "@NsTypeConstr"
            "MyEnum",
          structConstr = Name
            "@NsConstr"
            "MyEnum",
          structFields = [
            Field {
              fieldName = Name
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
          structName = Name
            "@NsTypeConstr"
            "MyEnum",
          structConstr = Name
            "@NsConstr"
            "MyEnum",
          structFields = [
            Field {
              fieldName = Name
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
        (Name
          "@NsConstr"
          "Say\20320\22909")
        (Name
          "@NsConstr"
          "Say\25308\25308"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
        Struct {
          structName = Name
            "@NsTypeConstr"
            "MyEnum",
          structConstr = Name
            "@NsConstr"
            "MyEnum",
          structFields = [
            Field {
              fieldName = Name
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
          structName = Name
            "@NsTypeConstr"
            "MyEnum",
          structConstr = Name
            "@NsConstr"
            "MyEnum",
          structFields = [
            Field {
              fieldName = Name
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
      patSynName = Name
        "@NsConstr"
        "Say\20320\22909",
      patSynType = Name
        "@NsTypeConstr"
        "MyEnum",
      patSynConstr = Name
        "@NsConstr"
        "MyEnum",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "uses_utf8.h:5:9",
            fieldName = NamePair {
              nameC = Name "Say\20320\22909",
              nameHsIdent = Identifier
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["uses_utf8.h"],
              headerInclude = "uses_utf8.h"},
          commentChildren = []}},
  DeclPatSyn
    PatSyn {
      patSynName = Name
        "@NsConstr"
        "Say\25308\25308",
      patSynType = Name
        "@NsTypeConstr"
        "MyEnum",
      patSynConstr = Name
        "@NsConstr"
        "MyEnum",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "uses_utf8.h:6:9",
            fieldName = NamePair {
              nameC = Name "Say\25308\25308",
              nameHsIdent = Identifier
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["uses_utf8.h"],
              headerInclude = "uses_utf8.h"},
          commentChildren = []}}]
