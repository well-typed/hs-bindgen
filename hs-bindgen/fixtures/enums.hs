[
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "First",
      newtypeConstr = Name
        "@NsConstr"
        "First",
      newtypeField = Field {
        fieldName = Name
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
            nameHsIdent = Identifier
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
              newtypeConstr = Name
                "@NsConstr"
                "First",
              newtypeField = Name
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
                    nameHsIdent = Identifier
                      "FIRST1"},
                  fieldComment = Nothing},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:6:5",
                  fieldName = NamePair {
                    nameC = Name "FIRST2",
                    nameHsIdent = Identifier
                      "FIRST2"},
                  fieldComment = Nothing},
                enumConstantValue = 1}]},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
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
          structName = Name
            "@NsTypeConstr"
            "First",
          structConstr = Name
            "@NsConstr"
            "First",
          structFields = [
            Field {
              fieldName = Name
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
                  structName = Name
                    "@NsTypeConstr"
                    "First",
                  structConstr = Name
                    "@NsConstr"
                    "First",
                  structFields = [
                    Field {
                      fieldName = Name
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
                  structName = Name
                    "@NsTypeConstr"
                    "First",
                  structConstr = Name
                    "@NsConstr"
                    "First",
                  structFields = [
                    Field {
                      fieldName = Name
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
      deriveInstanceName = Name
        "@NsTypeConstr"
        "First",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "First",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
        Struct {
          structName = Name
            "@NsTypeConstr"
            "First",
          structConstr = Name
            "@NsConstr"
            "First",
          structFields = [
            Field {
              fieldName = Name
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
          structName = Name
            "@NsTypeConstr"
            "First",
          structConstr = Name
            "@NsConstr"
            "First",
          structFields = [
            Field {
              fieldName = Name
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
        (Name "@NsConstr" "FIRST1")
        (Name "@NsConstr" "FIRST2"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
        Struct {
          structName = Name
            "@NsTypeConstr"
            "First",
          structConstr = Name
            "@NsConstr"
            "First",
          structFields = [
            Field {
              fieldName = Name
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
          structName = Name
            "@NsTypeConstr"
            "First",
          structConstr = Name
            "@NsConstr"
            "First",
          structFields = [
            Field {
              fieldName = Name
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
      patSynName = Name
        "@NsConstr"
        "FIRST1",
      patSynType = Name
        "@NsTypeConstr"
        "First",
      patSynConstr = Name
        "@NsConstr"
        "First",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:5:5",
            fieldName = NamePair {
              nameC = Name "FIRST1",
              nameHsIdent = Identifier
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
      patSynName = Name
        "@NsConstr"
        "FIRST2",
      patSynType = Name
        "@NsTypeConstr"
        "First",
      patSynConstr = Name
        "@NsConstr"
        "First",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:6:5",
            fieldName = NamePair {
              nameC = Name "FIRST2",
              nameHsIdent = Identifier
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
      newtypeName = Name
        "@NsTypeConstr"
        "Second",
      newtypeConstr = Name
        "@NsConstr"
        "Second",
      newtypeField = Field {
        fieldName = Name
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
            nameHsIdent = Identifier
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
              newtypeConstr = Name
                "@NsConstr"
                "Second",
              newtypeField = Name
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
                    nameHsIdent = Identifier
                      "SECOND_A"},
                  fieldComment = Nothing},
                enumConstantValue = `-1`},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:11:5",
                  fieldName = NamePair {
                    nameC = Name "SECOND_B",
                    nameHsIdent = Identifier
                      "SECOND_B"},
                  fieldComment = Nothing},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:12:5",
                  fieldName = NamePair {
                    nameC = Name "SECOND_C",
                    nameHsIdent = Identifier
                      "SECOND_C"},
                  fieldComment = Nothing},
                enumConstantValue = 1}]},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
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
          structName = Name
            "@NsTypeConstr"
            "Second",
          structConstr = Name
            "@NsConstr"
            "Second",
          structFields = [
            Field {
              fieldName = Name
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
                  structName = Name
                    "@NsTypeConstr"
                    "Second",
                  structConstr = Name
                    "@NsConstr"
                    "Second",
                  structFields = [
                    Field {
                      fieldName = Name
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
                  structName = Name
                    "@NsTypeConstr"
                    "Second",
                  structConstr = Name
                    "@NsConstr"
                    "Second",
                  structFields = [
                    Field {
                      fieldName = Name
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
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Second",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Second",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Second",
          structConstr = Name
            "@NsConstr"
            "Second",
          structFields = [
            Field {
              fieldName = Name
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
          structName = Name
            "@NsTypeConstr"
            "Second",
          structConstr = Name
            "@NsConstr"
            "Second",
          structFields = [
            Field {
              fieldName = Name
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
        (Name "@NsConstr" "SECOND_A")
        (Name "@NsConstr" "SECOND_C"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Second",
          structConstr = Name
            "@NsConstr"
            "Second",
          structFields = [
            Field {
              fieldName = Name
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
          structName = Name
            "@NsTypeConstr"
            "Second",
          structConstr = Name
            "@NsConstr"
            "Second",
          structFields = [
            Field {
              fieldName = Name
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
      patSynName = Name
        "@NsConstr"
        "SECOND_A",
      patSynType = Name
        "@NsTypeConstr"
        "Second",
      patSynConstr = Name
        "@NsConstr"
        "Second",
      patSynValue = `-1`,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:10:5",
            fieldName = NamePair {
              nameC = Name "SECOND_A",
              nameHsIdent = Identifier
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
      patSynName = Name
        "@NsConstr"
        "SECOND_B",
      patSynType = Name
        "@NsTypeConstr"
        "Second",
      patSynConstr = Name
        "@NsConstr"
        "Second",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:11:5",
            fieldName = NamePair {
              nameC = Name "SECOND_B",
              nameHsIdent = Identifier
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
      patSynName = Name
        "@NsConstr"
        "SECOND_C",
      patSynType = Name
        "@NsTypeConstr"
        "Second",
      patSynConstr = Name
        "@NsConstr"
        "Second",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:12:5",
            fieldName = NamePair {
              nameC = Name "SECOND_C",
              nameHsIdent = Identifier
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
      newtypeName = Name
        "@NsTypeConstr"
        "Same",
      newtypeConstr = Name
        "@NsConstr"
        "Same",
      newtypeField = Field {
        fieldName = Name
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
            nameHsIdent = Identifier
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
              newtypeConstr = Name
                "@NsConstr"
                "Same",
              newtypeField = Name
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
                    nameHsIdent = Identifier
                      "SAME_A"},
                  fieldComment = Nothing},
                enumConstantValue = 1},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:17:5",
                  fieldName = NamePair {
                    nameC = Name "SAME_B",
                    nameHsIdent = Identifier
                      "SAME_B"},
                  fieldComment = Nothing},
                enumConstantValue = 1}]},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
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
          structName = Name
            "@NsTypeConstr"
            "Same",
          structConstr = Name
            "@NsConstr"
            "Same",
          structFields = [
            Field {
              fieldName = Name
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
                  structName = Name
                    "@NsTypeConstr"
                    "Same",
                  structConstr = Name
                    "@NsConstr"
                    "Same",
                  structFields = [
                    Field {
                      fieldName = Name
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
                  structName = Name
                    "@NsTypeConstr"
                    "Same",
                  structConstr = Name
                    "@NsConstr"
                    "Same",
                  structFields = [
                    Field {
                      fieldName = Name
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
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Same",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Same",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Same",
          structConstr = Name
            "@NsConstr"
            "Same",
          structFields = [
            Field {
              fieldName = Name
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
          structName = Name
            "@NsTypeConstr"
            "Same",
          structConstr = Name
            "@NsConstr"
            "Same",
          structFields = [
            Field {
              fieldName = Name
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
        (Name "@NsConstr" "SAME_A")
        (Name "@NsConstr" "SAME_A"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Same",
          structConstr = Name
            "@NsConstr"
            "Same",
          structFields = [
            Field {
              fieldName = Name
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
          structName = Name
            "@NsTypeConstr"
            "Same",
          structConstr = Name
            "@NsConstr"
            "Same",
          structFields = [
            Field {
              fieldName = Name
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
      patSynName = Name
        "@NsConstr"
        "SAME_A",
      patSynType = Name
        "@NsTypeConstr"
        "Same",
      patSynConstr = Name
        "@NsConstr"
        "Same",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:16:5",
            fieldName = NamePair {
              nameC = Name "SAME_A",
              nameHsIdent = Identifier
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
      patSynName = Name
        "@NsConstr"
        "SAME_B",
      patSynType = Name
        "@NsTypeConstr"
        "Same",
      patSynConstr = Name
        "@NsConstr"
        "Same",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:17:5",
            fieldName = NamePair {
              nameC = Name "SAME_B",
              nameHsIdent = Identifier
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
      newtypeName = Name
        "@NsTypeConstr"
        "Nonseq",
      newtypeConstr = Name
        "@NsConstr"
        "Nonseq",
      newtypeField = Field {
        fieldName = Name
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
            nameHsIdent = Identifier
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
              newtypeConstr = Name
                "@NsConstr"
                "Nonseq",
              newtypeField = Name
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
                    nameHsIdent = Identifier
                      "NONSEQ_A"},
                  fieldComment = Nothing},
                enumConstantValue = 200},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:22:5",
                  fieldName = NamePair {
                    nameC = Name "NONSEQ_B",
                    nameHsIdent = Identifier
                      "NONSEQ_B"},
                  fieldComment = Nothing},
                enumConstantValue = 301},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:23:5",
                  fieldName = NamePair {
                    nameC = Name "NONSEQ_C",
                    nameHsIdent = Identifier
                      "NONSEQ_C"},
                  fieldComment = Nothing},
                enumConstantValue = 404}]},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
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
          structName = Name
            "@NsTypeConstr"
            "Nonseq",
          structConstr = Name
            "@NsConstr"
            "Nonseq",
          structFields = [
            Field {
              fieldName = Name
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
                  structName = Name
                    "@NsTypeConstr"
                    "Nonseq",
                  structConstr = Name
                    "@NsConstr"
                    "Nonseq",
                  structFields = [
                    Field {
                      fieldName = Name
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
                  structName = Name
                    "@NsTypeConstr"
                    "Nonseq",
                  structConstr = Name
                    "@NsConstr"
                    "Nonseq",
                  structFields = [
                    Field {
                      fieldName = Name
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
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Nonseq",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Nonseq",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Nonseq",
          structConstr = Name
            "@NsConstr"
            "Nonseq",
          structFields = [
            Field {
              fieldName = Name
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
          structName = Name
            "@NsTypeConstr"
            "Nonseq",
          structConstr = Name
            "@NsConstr"
            "Nonseq",
          structFields = [
            Field {
              fieldName = Name
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
          structName = Name
            "@NsTypeConstr"
            "Nonseq",
          structConstr = Name
            "@NsConstr"
            "Nonseq",
          structFields = [
            Field {
              fieldName = Name
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
      patSynName = Name
        "@NsConstr"
        "NONSEQ_A",
      patSynType = Name
        "@NsTypeConstr"
        "Nonseq",
      patSynConstr = Name
        "@NsConstr"
        "Nonseq",
      patSynValue = 200,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:21:5",
            fieldName = NamePair {
              nameC = Name "NONSEQ_A",
              nameHsIdent = Identifier
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
      patSynName = Name
        "@NsConstr"
        "NONSEQ_B",
      patSynType = Name
        "@NsTypeConstr"
        "Nonseq",
      patSynConstr = Name
        "@NsConstr"
        "Nonseq",
      patSynValue = 301,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:22:5",
            fieldName = NamePair {
              nameC = Name "NONSEQ_B",
              nameHsIdent = Identifier
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
      patSynName = Name
        "@NsConstr"
        "NONSEQ_C",
      patSynType = Name
        "@NsTypeConstr"
        "Nonseq",
      patSynConstr = Name
        "@NsConstr"
        "Nonseq",
      patSynValue = 404,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:23:5",
            fieldName = NamePair {
              nameC = Name "NONSEQ_C",
              nameHsIdent = Identifier
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
      newtypeName = Name
        "@NsTypeConstr"
        "Packed",
      newtypeConstr = Name
        "@NsConstr"
        "Packed",
      newtypeField = Field {
        fieldName = Name
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
            nameHsIdent = Identifier
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
              newtypeConstr = Name
                "@NsConstr"
                "Packed",
              newtypeField = Name
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
                    nameHsIdent = Identifier
                      "PACKED_A"},
                  fieldComment = Nothing},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:27:15",
                  fieldName = NamePair {
                    nameC = Name "PACKED_B",
                    nameHsIdent = Identifier
                      "PACKED_B"},
                  fieldComment = Nothing},
                enumConstantValue = 1},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:27:25",
                  fieldName = NamePair {
                    nameC = Name "PACKED_C",
                    nameHsIdent = Identifier
                      "PACKED_C"},
                  fieldComment = Nothing},
                enumConstantValue = 2}]},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
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
          structName = Name
            "@NsTypeConstr"
            "Packed",
          structConstr = Name
            "@NsConstr"
            "Packed",
          structFields = [
            Field {
              fieldName = Name
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
                  structName = Name
                    "@NsTypeConstr"
                    "Packed",
                  structConstr = Name
                    "@NsConstr"
                    "Packed",
                  structFields = [
                    Field {
                      fieldName = Name
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
                  structName = Name
                    "@NsTypeConstr"
                    "Packed",
                  structConstr = Name
                    "@NsConstr"
                    "Packed",
                  structFields = [
                    Field {
                      fieldName = Name
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
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Packed",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Packed",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Packed",
          structConstr = Name
            "@NsConstr"
            "Packed",
          structFields = [
            Field {
              fieldName = Name
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
          structName = Name
            "@NsTypeConstr"
            "Packed",
          structConstr = Name
            "@NsConstr"
            "Packed",
          structFields = [
            Field {
              fieldName = Name
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
        (Name "@NsConstr" "PACKED_A")
        (Name "@NsConstr" "PACKED_C"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Packed",
          structConstr = Name
            "@NsConstr"
            "Packed",
          structFields = [
            Field {
              fieldName = Name
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
          structName = Name
            "@NsTypeConstr"
            "Packed",
          structConstr = Name
            "@NsConstr"
            "Packed",
          structFields = [
            Field {
              fieldName = Name
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
      patSynName = Name
        "@NsConstr"
        "PACKED_A",
      patSynType = Name
        "@NsTypeConstr"
        "Packed",
      patSynConstr = Name
        "@NsConstr"
        "Packed",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:27:5",
            fieldName = NamePair {
              nameC = Name "PACKED_A",
              nameHsIdent = Identifier
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
      patSynName = Name
        "@NsConstr"
        "PACKED_B",
      patSynType = Name
        "@NsTypeConstr"
        "Packed",
      patSynConstr = Name
        "@NsConstr"
        "Packed",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:27:15",
            fieldName = NamePair {
              nameC = Name "PACKED_B",
              nameHsIdent = Identifier
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
      patSynName = Name
        "@NsConstr"
        "PACKED_C",
      patSynType = Name
        "@NsTypeConstr"
        "Packed",
      patSynConstr = Name
        "@NsConstr"
        "Packed",
      patSynValue = 2,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:27:25",
            fieldName = NamePair {
              nameC = Name "PACKED_C",
              nameHsIdent = Identifier
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
      newtypeName = Name
        "@NsTypeConstr"
        "EnumA",
      newtypeConstr = Name
        "@NsConstr"
        "EnumA",
      newtypeField = Field {
        fieldName = Name
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
            nameHsIdent = Identifier
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
              newtypeConstr = Name
                "@NsConstr"
                "EnumA",
              newtypeField = Name
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
                    nameHsIdent = Identifier
                      "A_FOO"},
                  fieldComment = Nothing},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:30:23",
                  fieldName = NamePair {
                    nameC = Name "A_BAR",
                    nameHsIdent = Identifier
                      "A_BAR"},
                  fieldComment = Nothing},
                enumConstantValue = 1}]},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Read, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
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
          structName = Name
            "@NsTypeConstr"
            "EnumA",
          structConstr = Name
            "@NsConstr"
            "EnumA",
          structFields = [
            Field {
              fieldName = Name
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
                  structName = Name
                    "@NsTypeConstr"
                    "EnumA",
                  structConstr = Name
                    "@NsConstr"
                    "EnumA",
                  structFields = [
                    Field {
                      fieldName = Name
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
                  structName = Name
                    "@NsTypeConstr"
                    "EnumA",
                  structConstr = Name
                    "@NsConstr"
                    "EnumA",
                  structFields = [
                    Field {
                      fieldName = Name
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
      deriveInstanceName = Name
        "@NsTypeConstr"
        "EnumA",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "EnumA",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
        Struct {
          structName = Name
            "@NsTypeConstr"
            "EnumA",
          structConstr = Name
            "@NsConstr"
            "EnumA",
          structFields = [
            Field {
              fieldName = Name
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
          structName = Name
            "@NsTypeConstr"
            "EnumA",
          structConstr = Name
            "@NsConstr"
            "EnumA",
          structFields = [
            Field {
              fieldName = Name
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
        (Name "@NsConstr" "A_FOO")
        (Name "@NsConstr" "A_BAR"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
        Struct {
          structName = Name
            "@NsTypeConstr"
            "EnumA",
          structConstr = Name
            "@NsConstr"
            "EnumA",
          structFields = [
            Field {
              fieldName = Name
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
          structName = Name
            "@NsTypeConstr"
            "EnumA",
          structConstr = Name
            "@NsConstr"
            "EnumA",
          structFields = [
            Field {
              fieldName = Name
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
      patSynName = Name
        "@NsConstr"
        "A_FOO",
      patSynType = Name
        "@NsTypeConstr"
        "EnumA",
      patSynConstr = Name
        "@NsConstr"
        "EnumA",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:30:16",
            fieldName = NamePair {
              nameC = Name "A_FOO",
              nameHsIdent = Identifier
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
      patSynName = Name
        "@NsConstr"
        "A_BAR",
      patSynType = Name
        "@NsTypeConstr"
        "EnumA",
      patSynConstr = Name
        "@NsConstr"
        "EnumA",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:30:23",
            fieldName = NamePair {
              nameC = Name "A_BAR",
              nameHsIdent = Identifier
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
      newtypeName = Name
        "@NsTypeConstr"
        "EnumB",
      newtypeConstr = Name
        "@NsConstr"
        "EnumB",
      newtypeField = Field {
        fieldName = Name
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
            nameHsIdent = Identifier
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
              newtypeConstr = Name
                "@NsConstr"
                "EnumB",
              newtypeField = Name
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
                    nameHsIdent = Identifier
                      "B_FOO"},
                  fieldComment = Nothing},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:32:29",
                  fieldName = NamePair {
                    nameC = Name "B_BAR",
                    nameHsIdent = Identifier
                      "B_BAR"},
                  fieldComment = Nothing},
                enumConstantValue = 1}]},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
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
          structName = Name
            "@NsTypeConstr"
            "EnumB",
          structConstr = Name
            "@NsConstr"
            "EnumB",
          structFields = [
            Field {
              fieldName = Name
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
                  structName = Name
                    "@NsTypeConstr"
                    "EnumB",
                  structConstr = Name
                    "@NsConstr"
                    "EnumB",
                  structFields = [
                    Field {
                      fieldName = Name
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
                  structName = Name
                    "@NsTypeConstr"
                    "EnumB",
                  structConstr = Name
                    "@NsConstr"
                    "EnumB",
                  structFields = [
                    Field {
                      fieldName = Name
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
      deriveInstanceName = Name
        "@NsTypeConstr"
        "EnumB",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "EnumB",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
        Struct {
          structName = Name
            "@NsTypeConstr"
            "EnumB",
          structConstr = Name
            "@NsConstr"
            "EnumB",
          structFields = [
            Field {
              fieldName = Name
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
          structName = Name
            "@NsTypeConstr"
            "EnumB",
          structConstr = Name
            "@NsConstr"
            "EnumB",
          structFields = [
            Field {
              fieldName = Name
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
        (Name "@NsConstr" "B_FOO")
        (Name "@NsConstr" "B_BAR"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
        Struct {
          structName = Name
            "@NsTypeConstr"
            "EnumB",
          structConstr = Name
            "@NsConstr"
            "EnumB",
          structFields = [
            Field {
              fieldName = Name
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
          structName = Name
            "@NsTypeConstr"
            "EnumB",
          structConstr = Name
            "@NsConstr"
            "EnumB",
          structFields = [
            Field {
              fieldName = Name
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
      patSynName = Name
        "@NsConstr"
        "B_FOO",
      patSynType = Name
        "@NsTypeConstr"
        "EnumB",
      patSynConstr = Name
        "@NsConstr"
        "EnumB",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:32:22",
            fieldName = NamePair {
              nameC = Name "B_FOO",
              nameHsIdent = Identifier
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
      patSynName = Name
        "@NsConstr"
        "B_BAR",
      patSynType = Name
        "@NsTypeConstr"
        "EnumB",
      patSynConstr = Name
        "@NsConstr"
        "EnumB",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:32:29",
            fieldName = NamePair {
              nameC = Name "B_BAR",
              nameHsIdent = Identifier
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
      newtypeName = Name
        "@NsTypeConstr"
        "EnumC",
      newtypeConstr = Name
        "@NsConstr"
        "EnumC",
      newtypeField = Field {
        fieldName = Name
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
            nameHsIdent = Identifier
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
              newtypeConstr = Name
                "@NsConstr"
                "EnumC",
              newtypeField = Name
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
                    nameHsIdent = Identifier
                      "C_FOO"},
                  fieldComment = Nothing},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:34:21",
                  fieldName = NamePair {
                    nameC = Name "C_BAR",
                    nameHsIdent = Identifier
                      "C_BAR"},
                  fieldComment = Nothing},
                enumConstantValue = 1}]},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
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
          structName = Name
            "@NsTypeConstr"
            "EnumC",
          structConstr = Name
            "@NsConstr"
            "EnumC",
          structFields = [
            Field {
              fieldName = Name
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
                  structName = Name
                    "@NsTypeConstr"
                    "EnumC",
                  structConstr = Name
                    "@NsConstr"
                    "EnumC",
                  structFields = [
                    Field {
                      fieldName = Name
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
                  structName = Name
                    "@NsTypeConstr"
                    "EnumC",
                  structConstr = Name
                    "@NsConstr"
                    "EnumC",
                  structFields = [
                    Field {
                      fieldName = Name
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
      deriveInstanceName = Name
        "@NsTypeConstr"
        "EnumC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "EnumC",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
        Struct {
          structName = Name
            "@NsTypeConstr"
            "EnumC",
          structConstr = Name
            "@NsConstr"
            "EnumC",
          structFields = [
            Field {
              fieldName = Name
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
          structName = Name
            "@NsTypeConstr"
            "EnumC",
          structConstr = Name
            "@NsConstr"
            "EnumC",
          structFields = [
            Field {
              fieldName = Name
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
        (Name "@NsConstr" "C_FOO")
        (Name "@NsConstr" "C_BAR"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
        Struct {
          structName = Name
            "@NsTypeConstr"
            "EnumC",
          structConstr = Name
            "@NsConstr"
            "EnumC",
          structFields = [
            Field {
              fieldName = Name
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
          structName = Name
            "@NsTypeConstr"
            "EnumC",
          structConstr = Name
            "@NsConstr"
            "EnumC",
          structFields = [
            Field {
              fieldName = Name
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
      patSynName = Name
        "@NsConstr"
        "C_FOO",
      patSynType = Name
        "@NsTypeConstr"
        "EnumC",
      patSynConstr = Name
        "@NsConstr"
        "EnumC",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:34:14",
            fieldName = NamePair {
              nameC = Name "C_FOO",
              nameHsIdent = Identifier
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
      patSynName = Name
        "@NsConstr"
        "C_BAR",
      patSynType = Name
        "@NsTypeConstr"
        "EnumC",
      patSynConstr = Name
        "@NsConstr"
        "EnumC",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:34:21",
            fieldName = NamePair {
              nameC = Name "C_BAR",
              nameHsIdent = Identifier
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
      newtypeName = Name
        "@NsTypeConstr"
        "EnumD_t",
      newtypeConstr = Name
        "@NsConstr"
        "EnumD_t",
      newtypeField = Field {
        fieldName = Name
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
            nameHsIdent = Identifier
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
              newtypeConstr = Name
                "@NsConstr"
                "EnumD_t",
              newtypeField = Name
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
                    nameHsIdent = Identifier
                      "D_FOO"},
                  fieldComment = Nothing},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "enums.h:37:21",
                  fieldName = NamePair {
                    nameC = Name "D_BAR",
                    nameHsIdent = Identifier
                      "D_BAR"},
                  fieldComment = Nothing},
                enumConstantValue = 1}]},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
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
          structName = Name
            "@NsTypeConstr"
            "EnumD_t",
          structConstr = Name
            "@NsConstr"
            "EnumD_t",
          structFields = [
            Field {
              fieldName = Name
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
                  structName = Name
                    "@NsTypeConstr"
                    "EnumD_t",
                  structConstr = Name
                    "@NsConstr"
                    "EnumD_t",
                  structFields = [
                    Field {
                      fieldName = Name
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
                  structName = Name
                    "@NsTypeConstr"
                    "EnumD_t",
                  structConstr = Name
                    "@NsConstr"
                    "EnumD_t",
                  structFields = [
                    Field {
                      fieldName = Name
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
      deriveInstanceName = Name
        "@NsTypeConstr"
        "EnumD_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "EnumD_t",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
        Struct {
          structName = Name
            "@NsTypeConstr"
            "EnumD_t",
          structConstr = Name
            "@NsConstr"
            "EnumD_t",
          structFields = [
            Field {
              fieldName = Name
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
          structName = Name
            "@NsTypeConstr"
            "EnumD_t",
          structConstr = Name
            "@NsConstr"
            "EnumD_t",
          structFields = [
            Field {
              fieldName = Name
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
        (Name "@NsConstr" "D_FOO")
        (Name "@NsConstr" "D_BAR"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
        Struct {
          structName = Name
            "@NsTypeConstr"
            "EnumD_t",
          structConstr = Name
            "@NsConstr"
            "EnumD_t",
          structFields = [
            Field {
              fieldName = Name
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
          structName = Name
            "@NsTypeConstr"
            "EnumD_t",
          structConstr = Name
            "@NsConstr"
            "EnumD_t",
          structFields = [
            Field {
              fieldName = Name
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
      patSynName = Name
        "@NsConstr"
        "D_FOO",
      patSynType = Name
        "@NsTypeConstr"
        "EnumD_t",
      patSynConstr = Name
        "@NsConstr"
        "EnumD_t",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:37:14",
            fieldName = NamePair {
              nameC = Name "D_FOO",
              nameHsIdent = Identifier
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
      patSynName = Name
        "@NsConstr"
        "D_BAR",
      patSynType = Name
        "@NsTypeConstr"
        "EnumD_t",
      patSynConstr = Name
        "@NsConstr"
        "EnumD_t",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "enums.h:37:21",
            fieldName = NamePair {
              nameC = Name "D_BAR",
              nameHsIdent = Identifier
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
