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
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "enums.h:4:6",
          declId = NamePair {
            nameC = Name "first",
            nameHsIdent = HsIdentifier
              "First"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          declComment = Nothing},
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
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:5:5",
                  fieldName = NamePair {
                    nameC = Name "FIRST1",
                    nameHsIdent = HsIdentifier
                      "FIRST1"},
                  fieldComment = Nothing},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:6:5",
                  fieldName = NamePair {
                    nameC = Name "FIRST2",
                    nameHsIdent = HsIdentifier
                      "FIRST2"},
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
          commentOrigin = Just "first",
          commentLocation = Just
            "enums.h:4:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
        "First",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "First",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
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
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (HsPrimType HsPrimCUInt)
        (Map.fromList
          [
            _×_ 0 (NE.fromList ["FIRST1"]),
            _×_ 1 (NE.fromList ["FIRST2"])])
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
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (HsName "@NsConstr" "FIRST1")
        (HsName "@NsConstr" "FIRST2"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
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
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:5:5",
            fieldName = NamePair {
              nameC = Name "FIRST1",
              nameHsIdent = HsIdentifier
                "FIRST1"},
            fieldComment = Nothing},
          enumConstantValue = 0},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "FIRST1",
          commentLocation = Just
            "enums.h:5:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
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
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:6:5",
            fieldName = NamePair {
              nameC = Name "FIRST2",
              nameHsIdent = HsIdentifier
                "FIRST2"},
            fieldComment = Nothing},
          enumConstantValue = 1},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "FIRST2",
          commentLocation = Just
            "enums.h:6:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
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
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "enums.h:9:6",
          declId = NamePair {
            nameC = Name "second",
            nameHsIdent = HsIdentifier
              "Second"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          declComment = Nothing},
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
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:10:5",
                  fieldName = NamePair {
                    nameC = Name "SECOND_A",
                    nameHsIdent = HsIdentifier
                      "SECOND_A"},
                  fieldComment = Nothing},
                enumConstantValue = `-1`},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:11:5",
                  fieldName = NamePair {
                    nameC = Name "SECOND_B",
                    nameHsIdent = HsIdentifier
                      "SECOND_B"},
                  fieldComment = Nothing},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:12:5",
                  fieldName = NamePair {
                    nameC = Name "SECOND_C",
                    nameHsIdent = HsIdentifier
                      "SECOND_C"},
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
          commentOrigin = Just "second",
          commentLocation = Just
            "enums.h:9:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
        "Second",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Second",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
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
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
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
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (HsName "@NsConstr" "SECOND_A")
        (HsName "@NsConstr" "SECOND_C"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
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
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:10:5",
            fieldName = NamePair {
              nameC = Name "SECOND_A",
              nameHsIdent = HsIdentifier
                "SECOND_A"},
            fieldComment = Nothing},
          enumConstantValue = `-1`},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "SECOND_A",
          commentLocation = Just
            "enums.h:10:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
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
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:11:5",
            fieldName = NamePair {
              nameC = Name "SECOND_B",
              nameHsIdent = HsIdentifier
                "SECOND_B"},
            fieldComment = Nothing},
          enumConstantValue = 0},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "SECOND_B",
          commentLocation = Just
            "enums.h:11:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
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
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:12:5",
            fieldName = NamePair {
              nameC = Name "SECOND_C",
              nameHsIdent = HsIdentifier
                "SECOND_C"},
            fieldComment = Nothing},
          enumConstantValue = 1},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "SECOND_C",
          commentLocation = Just
            "enums.h:12:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
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
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "enums.h:15:6",
          declId = NamePair {
            nameC = Name "same",
            nameHsIdent = HsIdentifier
              "Same"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          declComment = Nothing},
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
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:16:5",
                  fieldName = NamePair {
                    nameC = Name "SAME_A",
                    nameHsIdent = HsIdentifier
                      "SAME_A"},
                  fieldComment = Nothing},
                enumConstantValue = 1},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:17:5",
                  fieldName = NamePair {
                    nameC = Name "SAME_B",
                    nameHsIdent = HsIdentifier
                      "SAME_B"},
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
          commentOrigin = Just "same",
          commentLocation = Just
            "enums.h:15:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
        "Same",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Same",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
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
              1
              (NE.fromList
                ["SAME_A", "SAME_B"])])
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
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (HsName "@NsConstr" "SAME_A")
        (HsName "@NsConstr" "SAME_A"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
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
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:16:5",
            fieldName = NamePair {
              nameC = Name "SAME_A",
              nameHsIdent = HsIdentifier
                "SAME_A"},
            fieldComment = Nothing},
          enumConstantValue = 1},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "SAME_A",
          commentLocation = Just
            "enums.h:16:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
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
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:17:5",
            fieldName = NamePair {
              nameC = Name "SAME_B",
              nameHsIdent = HsIdentifier
                "SAME_B"},
            fieldComment = Nothing},
          enumConstantValue = 1},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "SAME_B",
          commentLocation = Just
            "enums.h:17:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
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
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "enums.h:20:6",
          declId = NamePair {
            nameC = Name "nonseq",
            nameHsIdent = HsIdentifier
              "Nonseq"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          declComment = Nothing},
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
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:21:5",
                  fieldName = NamePair {
                    nameC = Name "NONSEQ_A",
                    nameHsIdent = HsIdentifier
                      "NONSEQ_A"},
                  fieldComment = Nothing},
                enumConstantValue = 200},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:22:5",
                  fieldName = NamePair {
                    nameC = Name "NONSEQ_B",
                    nameHsIdent = HsIdentifier
                      "NONSEQ_B"},
                  fieldComment = Nothing},
                enumConstantValue = 301},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:23:5",
                  fieldName = NamePair {
                    nameC = Name "NONSEQ_C",
                    nameHsIdent = HsIdentifier
                      "NONSEQ_C"},
                  fieldComment = Nothing},
                enumConstantValue = 404}]},
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
          commentOrigin = Just "nonseq",
          commentLocation = Just
            "enums.h:20:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
        "Nonseq",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Nonseq",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
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
              200
              (NE.fromList ["NONSEQ_A"]),
            _×_
              301
              (NE.fromList ["NONSEQ_B"]),
            _×_
              404
              (NE.fromList ["NONSEQ_C"])])
        False,
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
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
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:21:5",
            fieldName = NamePair {
              nameC = Name "NONSEQ_A",
              nameHsIdent = HsIdentifier
                "NONSEQ_A"},
            fieldComment = Nothing},
          enumConstantValue = 200},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "NONSEQ_A",
          commentLocation = Just
            "enums.h:21:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
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
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:22:5",
            fieldName = NamePair {
              nameC = Name "NONSEQ_B",
              nameHsIdent = HsIdentifier
                "NONSEQ_B"},
            fieldComment = Nothing},
          enumConstantValue = 301},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "NONSEQ_B",
          commentLocation = Just
            "enums.h:22:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
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
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:23:5",
            fieldName = NamePair {
              nameC = Name "NONSEQ_C",
              nameHsIdent = HsIdentifier
                "NONSEQ_C"},
            fieldComment = Nothing},
          enumConstantValue = 404},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "NONSEQ_C",
          commentLocation = Just
            "enums.h:23:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
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
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "enums.h:26:6",
          declId = NamePair {
            nameC = Name "packed",
            nameHsIdent = HsIdentifier
              "Packed"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          declComment = Nothing},
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
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:27:5",
                  fieldName = NamePair {
                    nameC = Name "PACKED_A",
                    nameHsIdent = HsIdentifier
                      "PACKED_A"},
                  fieldComment = Nothing},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:27:15",
                  fieldName = NamePair {
                    nameC = Name "PACKED_B",
                    nameHsIdent = HsIdentifier
                      "PACKED_B"},
                  fieldComment = Nothing},
                enumConstantValue = 1},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:27:25",
                  fieldName = NamePair {
                    nameC = Name "PACKED_C",
                    nameHsIdent = HsIdentifier
                      "PACKED_C"},
                  fieldComment = Nothing},
                enumConstantValue = 2}]},
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
          commentOrigin = Just "packed",
          commentLocation = Just
            "enums.h:26:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
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
        "Packed",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Packed",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
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
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
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
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (HsName "@NsConstr" "PACKED_A")
        (HsName "@NsConstr" "PACKED_C"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
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
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:27:5",
            fieldName = NamePair {
              nameC = Name "PACKED_A",
              nameHsIdent = HsIdentifier
                "PACKED_A"},
            fieldComment = Nothing},
          enumConstantValue = 0},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "PACKED_A",
          commentLocation = Just
            "enums.h:27:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
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
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:27:15",
            fieldName = NamePair {
              nameC = Name "PACKED_B",
              nameHsIdent = HsIdentifier
                "PACKED_B"},
            fieldComment = Nothing},
          enumConstantValue = 1},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "PACKED_B",
          commentLocation = Just
            "enums.h:27:15",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
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
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:27:25",
            fieldName = NamePair {
              nameC = Name "PACKED_C",
              nameHsIdent = HsIdentifier
                "PACKED_C"},
            fieldComment = Nothing},
          enumConstantValue = 2},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "PACKED_C",
          commentLocation = Just
            "enums.h:27:25",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
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
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "enums.h:30:9",
          declId = NamePair {
            nameC = Name "enumA",
            nameHsIdent = HsIdentifier
              "EnumA"},
          declOrigin = NameOriginGenerated
            (AnonId "enums.h:30:9"),
          declAliases = [Name "enumA"],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          declComment = Nothing},
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
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:30:16",
                  fieldName = NamePair {
                    nameC = Name "A_FOO",
                    nameHsIdent = HsIdentifier
                      "A_FOO"},
                  fieldComment = Nothing},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:30:23",
                  fieldName = NamePair {
                    nameC = Name "A_BAR",
                    nameHsIdent = HsIdentifier
                      "A_BAR"},
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
          commentOrigin = Just "enumA",
          commentLocation = Just
            "enums.h:30:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
        "EnumA",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "EnumA",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
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
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (HsPrimType HsPrimCUInt)
        (Map.fromList
          [
            _×_ 0 (NE.fromList ["A_FOO"]),
            _×_ 1 (NE.fromList ["A_BAR"])])
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
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (HsName "@NsConstr" "A_FOO")
        (HsName "@NsConstr" "A_BAR"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
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
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:30:16",
            fieldName = NamePair {
              nameC = Name "A_FOO",
              nameHsIdent = HsIdentifier
                "A_FOO"},
            fieldComment = Nothing},
          enumConstantValue = 0},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "A_FOO",
          commentLocation = Just
            "enums.h:30:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
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
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:30:23",
            fieldName = NamePair {
              nameC = Name "A_BAR",
              nameHsIdent = HsIdentifier
                "A_BAR"},
            fieldComment = Nothing},
          enumConstantValue = 1},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "A_BAR",
          commentLocation = Just
            "enums.h:30:23",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
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
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "enums.h:32:14",
          declId = NamePair {
            nameC = Name "enumB",
            nameHsIdent = HsIdentifier
              "EnumB"},
          declOrigin = NameOriginInSource,
          declAliases = [Name "enumB"],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          declComment = Nothing},
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
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:32:22",
                  fieldName = NamePair {
                    nameC = Name "B_FOO",
                    nameHsIdent = HsIdentifier
                      "B_FOO"},
                  fieldComment = Nothing},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:32:29",
                  fieldName = NamePair {
                    nameC = Name "B_BAR",
                    nameHsIdent = HsIdentifier
                      "B_BAR"},
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
          commentOrigin = Just "enumB",
          commentLocation = Just
            "enums.h:32:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
        "EnumB",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "EnumB",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
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
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (HsPrimType HsPrimCUInt)
        (Map.fromList
          [
            _×_ 0 (NE.fromList ["B_FOO"]),
            _×_ 1 (NE.fromList ["B_BAR"])])
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
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (HsName "@NsConstr" "B_FOO")
        (HsName "@NsConstr" "B_BAR"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
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
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:32:22",
            fieldName = NamePair {
              nameC = Name "B_FOO",
              nameHsIdent = HsIdentifier
                "B_FOO"},
            fieldComment = Nothing},
          enumConstantValue = 0},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "B_FOO",
          commentLocation = Just
            "enums.h:32:22",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
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
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:32:29",
            fieldName = NamePair {
              nameC = Name "B_BAR",
              nameHsIdent = HsIdentifier
                "B_BAR"},
            fieldComment = Nothing},
          enumConstantValue = 1},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "B_BAR",
          commentLocation = Just
            "enums.h:32:29",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
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
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "enums.h:34:6",
          declId = NamePair {
            nameC = Name "enumC",
            nameHsIdent = HsIdentifier
              "EnumC"},
          declOrigin = NameOriginInSource,
          declAliases = [Name "enumC"],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          declComment = Nothing},
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
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:34:14",
                  fieldName = NamePair {
                    nameC = Name "C_FOO",
                    nameHsIdent = HsIdentifier
                      "C_FOO"},
                  fieldComment = Nothing},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:34:21",
                  fieldName = NamePair {
                    nameC = Name "C_BAR",
                    nameHsIdent = HsIdentifier
                      "C_BAR"},
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
          commentOrigin = Just "enumC",
          commentLocation = Just
            "enums.h:34:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
        "EnumC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "EnumC",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
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
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (HsPrimType HsPrimCUInt)
        (Map.fromList
          [
            _×_ 0 (NE.fromList ["C_FOO"]),
            _×_ 1 (NE.fromList ["C_BAR"])])
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
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (HsName "@NsConstr" "C_FOO")
        (HsName "@NsConstr" "C_BAR"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
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
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:34:14",
            fieldName = NamePair {
              nameC = Name "C_FOO",
              nameHsIdent = HsIdentifier
                "C_FOO"},
            fieldComment = Nothing},
          enumConstantValue = 0},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "C_FOO",
          commentLocation = Just
            "enums.h:34:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
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
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:34:21",
            fieldName = NamePair {
              nameC = Name "C_BAR",
              nameHsIdent = HsIdentifier
                "C_BAR"},
            fieldComment = Nothing},
          enumConstantValue = 1},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "C_BAR",
          commentLocation = Just
            "enums.h:34:21",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
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
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "enums.h:37:6",
          declId = NamePair {
            nameC = Name "enumD_t",
            nameHsIdent = HsIdentifier
              "EnumD_t"},
          declOrigin =
          NameOriginRenamedFrom
            (Name "enumD"),
          declAliases = [Name "enumD_t"],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          declComment = Nothing},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "EnumD_t",
              newtypeField = HsName
                "@NsVar"
                "un_EnumD_t"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:37:14",
                  fieldName = NamePair {
                    nameC = Name "D_FOO",
                    nameHsIdent = HsIdentifier
                      "D_FOO"},
                  fieldComment = Nothing},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:37:21",
                  fieldName = NamePair {
                    nameC = Name "D_BAR",
                    nameHsIdent = HsIdentifier
                      "D_BAR"},
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
          commentOrigin = Just "enumD_t",
          commentLocation = Just
            "enums.h:37:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "EnumD_t",
          structConstr = HsName
            "@NsConstr"
            "EnumD_t",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_EnumD_t",
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
                    "EnumD_t",
                  structConstr = HsName
                    "@NsConstr"
                    "EnumD_t",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "un_EnumD_t",
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
                    "EnumD_t",
                  structConstr = HsName
                    "@NsConstr"
                    "EnumD_t",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "un_EnumD_t",
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
        "EnumD_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "EnumD_t",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "EnumD_t",
          structConstr = HsName
            "@NsConstr"
            "EnumD_t",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_EnumD_t",
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
            _×_ 0 (NE.fromList ["D_FOO"]),
            _×_ 1 (NE.fromList ["D_BAR"])])
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
            "EnumD_t",
          structConstr = HsName
            "@NsConstr"
            "EnumD_t",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_EnumD_t",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (HsName "@NsConstr" "D_FOO")
        (HsName "@NsConstr" "D_BAR"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "EnumD_t",
          structConstr = HsName
            "@NsConstr"
            "EnumD_t",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_EnumD_t",
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
            "EnumD_t",
          structConstr = HsName
            "@NsConstr"
            "EnumD_t",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_EnumD_t",
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
        "D_FOO",
      patSynType = HsName
        "@NsTypeConstr"
        "EnumD_t",
      patSynConstr = HsName
        "@NsConstr"
        "EnumD_t",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:37:14",
            fieldName = NamePair {
              nameC = Name "D_FOO",
              nameHsIdent = HsIdentifier
                "D_FOO"},
            fieldComment = Nothing},
          enumConstantValue = 0},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "D_FOO",
          commentLocation = Just
            "enums.h:37:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "D_BAR",
      patSynType = HsName
        "@NsTypeConstr"
        "EnumD_t",
      patSynConstr = HsName
        "@NsConstr"
        "EnumD_t",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:37:21",
            fieldName = NamePair {
              nameC = Name "D_BAR",
              nameHsIdent = HsIdentifier
                "D_BAR"},
            fieldComment = Nothing},
          enumConstantValue = 1},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "D_BAR",
          commentLocation = Just
            "enums.h:37:21",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["enums.h"],
              headerInclude = "enums.h"},
          commentChildren = []}}]
