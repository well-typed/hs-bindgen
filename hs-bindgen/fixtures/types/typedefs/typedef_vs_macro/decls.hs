[
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "T1",
      newtypeConstr = Name
        "@NsConstr"
        "T1",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_T1",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "typedef_vs_macro.h:1:13",
          declId = NamePair {
            nameC = Name "T1",
            nameHsIdent = Identifier "T1"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "types/typedefs/typedef_vs_macro.h"],
              headerInclude =
              "types/typedefs/typedef_vs_macro.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "T1",
              newtypeField = Name
                "@NsVar"
                "un_T1"},
            typedefType = TypePrim
              (PrimIntegral PrimInt Signed)},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [
          Bits,
          Bounded,
          Enum,
          Eq,
          FiniteBits,
          Integral,
          Ix,
          Num,
          Ord,
          Read,
          Real,
          Show,
          Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "T1",
          commentLocation = Just
            "typedef_vs_macro.h:1:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "types/typedefs/typedef_vs_macro.h"],
              headerInclude =
              "types/typedefs/typedef_vs_macro.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass =
      FiniteBits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "T1"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_T1",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "T1"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_T1",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCInt,
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "T2",
      newtypeConstr = Name
        "@NsConstr"
        "T2",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_T2",
        fieldType = HsPrimType
          HsPrimCChar,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "typedef_vs_macro.h:2:14",
          declId = NamePair {
            nameC = Name "T2",
            nameHsIdent = Identifier "T2"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "types/typedefs/typedef_vs_macro.h"],
              headerInclude =
              "types/typedefs/typedef_vs_macro.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "T2",
              newtypeField = Name
                "@NsVar"
                "un_T2"},
            typedefType = TypePrim
              (PrimChar
                (PrimSignImplicit
                  (Just Signed)))},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [
          Bits,
          Bounded,
          Enum,
          Eq,
          FiniteBits,
          Integral,
          Ix,
          Num,
          Ord,
          Read,
          Real,
          Show,
          Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "T2",
          commentLocation = Just
            "typedef_vs_macro.h:2:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "types/typedefs/typedef_vs_macro.h"],
              headerInclude =
              "types/typedefs/typedef_vs_macro.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass =
      FiniteBits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "T2"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_T2",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCChar,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "T2"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_T2",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCChar,
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "M1",
      newtypeConstr = Name
        "@NsConstr"
        "M1",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_M1",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "typedef_vs_macro.h:4:9",
          declId = NamePair {
            nameC = Name "M1",
            nameHsIdent = Identifier "M1"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "types/typedefs/typedef_vs_macro.h"],
              headerInclude =
              "types/typedefs/typedef_vs_macro.h"},
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "M1",
              newtypeField = Name
                "@NsVar"
                "un_M1"},
            macroType = TypePrim
              (PrimIntegral PrimInt Signed)},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [
          Bits,
          Bounded,
          Enum,
          Eq,
          FiniteBits,
          Integral,
          Ix,
          Num,
          Ord,
          Read,
          Real,
          Show,
          Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "M1",
          commentLocation = Just
            "typedef_vs_macro.h:4:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "types/typedefs/typedef_vs_macro.h"],
              headerInclude =
              "types/typedefs/typedef_vs_macro.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass =
      FiniteBits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "M2",
      newtypeConstr = Name
        "@NsConstr"
        "M2",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_M2",
        fieldType = HsPrimType
          HsPrimCChar,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "typedef_vs_macro.h:5:9",
          declId = NamePair {
            nameC = Name "M2",
            nameHsIdent = Identifier "M2"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "types/typedefs/typedef_vs_macro.h"],
              headerInclude =
              "types/typedefs/typedef_vs_macro.h"},
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "M2",
              newtypeField = Name
                "@NsVar"
                "un_M2"},
            macroType = TypePrim
              (PrimChar
                (PrimSignImplicit Nothing))},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [
          Bits,
          Bounded,
          Enum,
          Eq,
          FiniteBits,
          Integral,
          Ix,
          Num,
          Ord,
          Read,
          Real,
          Show,
          Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "M2",
          commentLocation = Just
            "typedef_vs_macro.h:5:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "types/typedefs/typedef_vs_macro.h"],
              headerInclude =
              "types/typedefs/typedef_vs_macro.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass =
      FiniteBits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "M3",
      newtypeConstr = Name
        "@NsConstr"
        "M3",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_M3",
        fieldType = HsPtr
          (HsPrimType HsPrimCInt),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "typedef_vs_macro.h:6:9",
          declId = NamePair {
            nameC = Name "M3",
            nameHsIdent = Identifier "M3"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "types/typedefs/typedef_vs_macro.h"],
              headerInclude =
              "types/typedefs/typedef_vs_macro.h"},
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "M3",
              newtypeField = Name
                "@NsVar"
                "un_M3"},
            macroType = TypePointer
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "M3",
          commentLocation = Just
            "typedef_vs_macro.h:6:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "types/typedefs/typedef_vs_macro.h"],
              headerInclude =
              "types/typedefs/typedef_vs_macro.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "M3",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "ExampleStruct",
      structConstr = Name
        "@NsConstr"
        "ExampleStruct",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "exampleStruct_t1",
          fieldType = HsTypRef
            (Name "@NsTypeConstr" "T1"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "typedef_vs_macro.h:9:6",
                fieldName = NamePair {
                  nameC = Name "t1",
                  nameHsIdent = Identifier
                    "exampleStruct_t1"},
                fieldComment = Nothing},
              structFieldType = TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "T1",
                    nameHsIdent = Identifier "T1"}
                  (TypePrim
                    (PrimIntegral PrimInt Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "t1",
              commentLocation = Just
                "typedef_vs_macro.h:9:6",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/typedefs/typedef_vs_macro.h"],
                  headerInclude =
                  "types/typedefs/typedef_vs_macro.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "exampleStruct_t2",
          fieldType = HsTypRef
            (Name "@NsTypeConstr" "T2"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "typedef_vs_macro.h:10:6",
                fieldName = NamePair {
                  nameC = Name "t2",
                  nameHsIdent = Identifier
                    "exampleStruct_t2"},
                fieldComment = Nothing},
              structFieldType = TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "T2",
                    nameHsIdent = Identifier "T2"}
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))))),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "t2",
              commentLocation = Just
                "typedef_vs_macro.h:10:6",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/typedefs/typedef_vs_macro.h"],
                  headerInclude =
                  "types/typedefs/typedef_vs_macro.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "exampleStruct_m1",
          fieldType = HsTypRef
            (Name "@NsTypeConstr" "M1"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "typedef_vs_macro.h:11:6",
                fieldName = NamePair {
                  nameC = Name "m1",
                  nameHsIdent = Identifier
                    "exampleStruct_m1"},
                fieldComment = Nothing},
              structFieldType =
              TypeMacroTypedef
                NamePair {
                  nameC = Name "M1",
                  nameHsIdent = Identifier "M1"}
                NameOriginInSource,
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "m1",
              commentLocation = Just
                "typedef_vs_macro.h:11:6",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/typedefs/typedef_vs_macro.h"],
                  headerInclude =
                  "types/typedefs/typedef_vs_macro.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "exampleStruct_m2",
          fieldType = HsTypRef
            (Name "@NsTypeConstr" "M2"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "typedef_vs_macro.h:12:6",
                fieldName = NamePair {
                  nameC = Name "m2",
                  nameHsIdent = Identifier
                    "exampleStruct_m2"},
                fieldComment = Nothing},
              structFieldType =
              TypeMacroTypedef
                NamePair {
                  nameC = Name "M2",
                  nameHsIdent = Identifier "M2"}
                NameOriginInSource,
              structFieldOffset = 96,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "m2",
              commentLocation = Just
                "typedef_vs_macro.h:12:6",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/typedefs/typedef_vs_macro.h"],
                  headerInclude =
                  "types/typedefs/typedef_vs_macro.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "typedef_vs_macro.h:8:8",
            declId = NamePair {
              nameC = Name "ExampleStruct",
              nameHsIdent = Identifier
                "ExampleStruct"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  [
                    "types/typedefs/typedef_vs_macro.h"],
                headerInclude =
                "types/typedefs/typedef_vs_macro.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name
                  "@NsConstr"
                  "ExampleStruct"),
              structSizeof = 16,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "typedef_vs_macro.h:9:6",
                    fieldName = NamePair {
                      nameC = Name "t1",
                      nameHsIdent = Identifier
                        "exampleStruct_t1"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "T1",
                        nameHsIdent = Identifier "T1"}
                      (TypePrim
                        (PrimIntegral PrimInt Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "typedef_vs_macro.h:10:6",
                    fieldName = NamePair {
                      nameC = Name "t2",
                      nameHsIdent = Identifier
                        "exampleStruct_t2"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "T2",
                        nameHsIdent = Identifier "T2"}
                      (TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))))),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "typedef_vs_macro.h:11:6",
                    fieldName = NamePair {
                      nameC = Name "m1",
                      nameHsIdent = Identifier
                        "exampleStruct_m1"},
                    fieldComment = Nothing},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = Name "M1",
                      nameHsIdent = Identifier "M1"}
                    NameOriginInSource,
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "typedef_vs_macro.h:12:6",
                    fieldName = NamePair {
                      nameC = Name "m2",
                      nameHsIdent = Identifier
                        "exampleStruct_m2"},
                    fieldComment = Nothing},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = Name "M2",
                      nameHsIdent = Identifier "M2"}
                    NameOriginInSource,
                  structFieldOffset = 96,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec {
            declSpecC = Nothing,
            declSpecHs = Nothing}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ExampleStruct",
          commentLocation = Just
            "typedef_vs_macro.h:8:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "types/typedefs/typedef_vs_macro.h"],
              headerInclude =
              "types/typedefs/typedef_vs_macro.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "ExampleStruct",
          structConstr = Name
            "@NsConstr"
            "ExampleStruct",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "exampleStruct_t1",
              fieldType = HsTypRef
                (Name "@NsTypeConstr" "T1"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "typedef_vs_macro.h:9:6",
                    fieldName = NamePair {
                      nameC = Name "t1",
                      nameHsIdent = Identifier
                        "exampleStruct_t1"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "T1",
                        nameHsIdent = Identifier "T1"}
                      (TypePrim
                        (PrimIntegral PrimInt Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "t1",
                  commentLocation = Just
                    "typedef_vs_macro.h:9:6",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/typedefs/typedef_vs_macro.h"],
                      headerInclude =
                      "types/typedefs/typedef_vs_macro.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "exampleStruct_t2",
              fieldType = HsTypRef
                (Name "@NsTypeConstr" "T2"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "typedef_vs_macro.h:10:6",
                    fieldName = NamePair {
                      nameC = Name "t2",
                      nameHsIdent = Identifier
                        "exampleStruct_t2"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "T2",
                        nameHsIdent = Identifier "T2"}
                      (TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))))),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "t2",
                  commentLocation = Just
                    "typedef_vs_macro.h:10:6",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/typedefs/typedef_vs_macro.h"],
                      headerInclude =
                      "types/typedefs/typedef_vs_macro.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "exampleStruct_m1",
              fieldType = HsTypRef
                (Name "@NsTypeConstr" "M1"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "typedef_vs_macro.h:11:6",
                    fieldName = NamePair {
                      nameC = Name "m1",
                      nameHsIdent = Identifier
                        "exampleStruct_m1"},
                    fieldComment = Nothing},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = Name "M1",
                      nameHsIdent = Identifier "M1"}
                    NameOriginInSource,
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "m1",
                  commentLocation = Just
                    "typedef_vs_macro.h:11:6",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/typedefs/typedef_vs_macro.h"],
                      headerInclude =
                      "types/typedefs/typedef_vs_macro.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "exampleStruct_m2",
              fieldType = HsTypRef
                (Name "@NsTypeConstr" "M2"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "typedef_vs_macro.h:12:6",
                    fieldName = NamePair {
                      nameC = Name "m2",
                      nameHsIdent = Identifier
                        "exampleStruct_m2"},
                    fieldComment = Nothing},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = Name "M2",
                      nameHsIdent = Identifier "M2"}
                    NameOriginInSource,
                  structFieldOffset = 96,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "m2",
                  commentLocation = Just
                    "typedef_vs_macro.h:12:6",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/typedefs/typedef_vs_macro.h"],
                      headerInclude =
                      "types/typedefs/typedef_vs_macro.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "typedef_vs_macro.h:8:8",
                declId = NamePair {
                  nameC = Name "ExampleStruct",
                  nameHsIdent = Identifier
                    "ExampleStruct"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      [
                        "types/typedefs/typedef_vs_macro.h"],
                    headerInclude =
                    "types/typedefs/typedef_vs_macro.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name
                      "@NsConstr"
                      "ExampleStruct"),
                  structSizeof = 16,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "typedef_vs_macro.h:9:6",
                        fieldName = NamePair {
                          nameC = Name "t1",
                          nameHsIdent = Identifier
                            "exampleStruct_t1"},
                        fieldComment = Nothing},
                      structFieldType = TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "T1",
                            nameHsIdent = Identifier "T1"}
                          (TypePrim
                            (PrimIntegral PrimInt Signed))),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "typedef_vs_macro.h:10:6",
                        fieldName = NamePair {
                          nameC = Name "t2",
                          nameHsIdent = Identifier
                            "exampleStruct_t2"},
                        fieldComment = Nothing},
                      structFieldType = TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "T2",
                            nameHsIdent = Identifier "T2"}
                          (TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))))),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "typedef_vs_macro.h:11:6",
                        fieldName = NamePair {
                          nameC = Name "m1",
                          nameHsIdent = Identifier
                            "exampleStruct_m1"},
                        fieldComment = Nothing},
                      structFieldType =
                      TypeMacroTypedef
                        NamePair {
                          nameC = Name "M1",
                          nameHsIdent = Identifier "M1"}
                        NameOriginInSource,
                      structFieldOffset = 64,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "typedef_vs_macro.h:12:6",
                        fieldName = NamePair {
                          nameC = Name "m2",
                          nameHsIdent = Identifier
                            "exampleStruct_m2"},
                        fieldComment = Nothing},
                      structFieldType =
                      TypeMacroTypedef
                        NamePair {
                          nameC = Name "M2",
                          nameHsIdent = Identifier "M2"}
                        NameOriginInSource,
                      structFieldOffset = 96,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec {
                declSpecC = Nothing,
                declSpecHs = Nothing}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "ExampleStruct",
              commentLocation = Just
                "typedef_vs_macro.h:8:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/typedefs/typedef_vs_macro.h"],
                  headerInclude =
                  "types/typedefs/typedef_vs_macro.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 16,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "ExampleStruct",
                  structConstr = Name
                    "@NsConstr"
                    "ExampleStruct",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "exampleStruct_t1",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "T1"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "typedef_vs_macro.h:9:6",
                            fieldName = NamePair {
                              nameC = Name "t1",
                              nameHsIdent = Identifier
                                "exampleStruct_t1"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefRegular
                              NamePair {
                                nameC = Name "T1",
                                nameHsIdent = Identifier "T1"}
                              (TypePrim
                                (PrimIntegral PrimInt Signed))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "t1",
                          commentLocation = Just
                            "typedef_vs_macro.h:9:6",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/typedefs/typedef_vs_macro.h"],
                              headerInclude =
                              "types/typedefs/typedef_vs_macro.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "exampleStruct_t2",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "T2"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "typedef_vs_macro.h:10:6",
                            fieldName = NamePair {
                              nameC = Name "t2",
                              nameHsIdent = Identifier
                                "exampleStruct_t2"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefRegular
                              NamePair {
                                nameC = Name "T2",
                                nameHsIdent = Identifier "T2"}
                              (TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))))),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "t2",
                          commentLocation = Just
                            "typedef_vs_macro.h:10:6",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/typedefs/typedef_vs_macro.h"],
                              headerInclude =
                              "types/typedefs/typedef_vs_macro.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "exampleStruct_m1",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "M1"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "typedef_vs_macro.h:11:6",
                            fieldName = NamePair {
                              nameC = Name "m1",
                              nameHsIdent = Identifier
                                "exampleStruct_m1"},
                            fieldComment = Nothing},
                          structFieldType =
                          TypeMacroTypedef
                            NamePair {
                              nameC = Name "M1",
                              nameHsIdent = Identifier "M1"}
                            NameOriginInSource,
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "m1",
                          commentLocation = Just
                            "typedef_vs_macro.h:11:6",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/typedefs/typedef_vs_macro.h"],
                              headerInclude =
                              "types/typedefs/typedef_vs_macro.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "exampleStruct_m2",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "M2"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "typedef_vs_macro.h:12:6",
                            fieldName = NamePair {
                              nameC = Name "m2",
                              nameHsIdent = Identifier
                                "exampleStruct_m2"},
                            fieldComment = Nothing},
                          structFieldType =
                          TypeMacroTypedef
                            NamePair {
                              nameC = Name "M2",
                              nameHsIdent = Identifier "M2"}
                            NameOriginInSource,
                          structFieldOffset = 96,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "m2",
                          commentLocation = Just
                            "typedef_vs_macro.h:12:6",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/typedefs/typedef_vs_macro.h"],
                              headerInclude =
                              "types/typedefs/typedef_vs_macro.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "typedef_vs_macro.h:8:8",
                        declId = NamePair {
                          nameC = Name "ExampleStruct",
                          nameHsIdent = Identifier
                            "ExampleStruct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              [
                                "types/typedefs/typedef_vs_macro.h"],
                            headerInclude =
                            "types/typedefs/typedef_vs_macro.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "ExampleStruct"),
                          structSizeof = 16,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "typedef_vs_macro.h:9:6",
                                fieldName = NamePair {
                                  nameC = Name "t1",
                                  nameHsIdent = Identifier
                                    "exampleStruct_t1"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefRegular
                                  NamePair {
                                    nameC = Name "T1",
                                    nameHsIdent = Identifier "T1"}
                                  (TypePrim
                                    (PrimIntegral PrimInt Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "typedef_vs_macro.h:10:6",
                                fieldName = NamePair {
                                  nameC = Name "t2",
                                  nameHsIdent = Identifier
                                    "exampleStruct_t2"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefRegular
                                  NamePair {
                                    nameC = Name "T2",
                                    nameHsIdent = Identifier "T2"}
                                  (TypePrim
                                    (PrimChar
                                      (PrimSignImplicit
                                        (Just Signed))))),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "typedef_vs_macro.h:11:6",
                                fieldName = NamePair {
                                  nameC = Name "m1",
                                  nameHsIdent = Identifier
                                    "exampleStruct_m1"},
                                fieldComment = Nothing},
                              structFieldType =
                              TypeMacroTypedef
                                NamePair {
                                  nameC = Name "M1",
                                  nameHsIdent = Identifier "M1"}
                                NameOriginInSource,
                              structFieldOffset = 64,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "typedef_vs_macro.h:12:6",
                                fieldName = NamePair {
                                  nameC = Name "m2",
                                  nameHsIdent = Identifier
                                    "exampleStruct_m2"},
                                fieldComment = Nothing},
                              structFieldType =
                              TypeMacroTypedef
                                NamePair {
                                  nameC = Name "M2",
                                  nameHsIdent = Identifier "M2"}
                                NameOriginInSource,
                              structFieldOffset = 96,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "ExampleStruct",
                      commentLocation = Just
                        "typedef_vs_macro.h:8:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            [
                              "types/typedefs/typedef_vs_macro.h"],
                          headerInclude =
                          "types/typedefs/typedef_vs_macro.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "exampleStruct_t1")
                  (Idx 0),
                PeekCField
                  (HsStrLit "exampleStruct_t2")
                  (Idx 0),
                PeekCField
                  (HsStrLit "exampleStruct_m1")
                  (Idx 0),
                PeekCField
                  (HsStrLit "exampleStruct_m2")
                  (Idx 0)]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "ExampleStruct",
                  structConstr = Name
                    "@NsConstr"
                    "ExampleStruct",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "exampleStruct_t1",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "T1"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "typedef_vs_macro.h:9:6",
                            fieldName = NamePair {
                              nameC = Name "t1",
                              nameHsIdent = Identifier
                                "exampleStruct_t1"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefRegular
                              NamePair {
                                nameC = Name "T1",
                                nameHsIdent = Identifier "T1"}
                              (TypePrim
                                (PrimIntegral PrimInt Signed))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "t1",
                          commentLocation = Just
                            "typedef_vs_macro.h:9:6",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/typedefs/typedef_vs_macro.h"],
                              headerInclude =
                              "types/typedefs/typedef_vs_macro.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "exampleStruct_t2",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "T2"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "typedef_vs_macro.h:10:6",
                            fieldName = NamePair {
                              nameC = Name "t2",
                              nameHsIdent = Identifier
                                "exampleStruct_t2"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefRegular
                              NamePair {
                                nameC = Name "T2",
                                nameHsIdent = Identifier "T2"}
                              (TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))))),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "t2",
                          commentLocation = Just
                            "typedef_vs_macro.h:10:6",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/typedefs/typedef_vs_macro.h"],
                              headerInclude =
                              "types/typedefs/typedef_vs_macro.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "exampleStruct_m1",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "M1"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "typedef_vs_macro.h:11:6",
                            fieldName = NamePair {
                              nameC = Name "m1",
                              nameHsIdent = Identifier
                                "exampleStruct_m1"},
                            fieldComment = Nothing},
                          structFieldType =
                          TypeMacroTypedef
                            NamePair {
                              nameC = Name "M1",
                              nameHsIdent = Identifier "M1"}
                            NameOriginInSource,
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "m1",
                          commentLocation = Just
                            "typedef_vs_macro.h:11:6",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/typedefs/typedef_vs_macro.h"],
                              headerInclude =
                              "types/typedefs/typedef_vs_macro.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "exampleStruct_m2",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "M2"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "typedef_vs_macro.h:12:6",
                            fieldName = NamePair {
                              nameC = Name "m2",
                              nameHsIdent = Identifier
                                "exampleStruct_m2"},
                            fieldComment = Nothing},
                          structFieldType =
                          TypeMacroTypedef
                            NamePair {
                              nameC = Name "M2",
                              nameHsIdent = Identifier "M2"}
                            NameOriginInSource,
                          structFieldOffset = 96,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "m2",
                          commentLocation = Just
                            "typedef_vs_macro.h:12:6",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/typedefs/typedef_vs_macro.h"],
                              headerInclude =
                              "types/typedefs/typedef_vs_macro.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "typedef_vs_macro.h:8:8",
                        declId = NamePair {
                          nameC = Name "ExampleStruct",
                          nameHsIdent = Identifier
                            "ExampleStruct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              [
                                "types/typedefs/typedef_vs_macro.h"],
                            headerInclude =
                            "types/typedefs/typedef_vs_macro.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "ExampleStruct"),
                          structSizeof = 16,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "typedef_vs_macro.h:9:6",
                                fieldName = NamePair {
                                  nameC = Name "t1",
                                  nameHsIdent = Identifier
                                    "exampleStruct_t1"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefRegular
                                  NamePair {
                                    nameC = Name "T1",
                                    nameHsIdent = Identifier "T1"}
                                  (TypePrim
                                    (PrimIntegral PrimInt Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "typedef_vs_macro.h:10:6",
                                fieldName = NamePair {
                                  nameC = Name "t2",
                                  nameHsIdent = Identifier
                                    "exampleStruct_t2"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefRegular
                                  NamePair {
                                    nameC = Name "T2",
                                    nameHsIdent = Identifier "T2"}
                                  (TypePrim
                                    (PrimChar
                                      (PrimSignImplicit
                                        (Just Signed))))),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "typedef_vs_macro.h:11:6",
                                fieldName = NamePair {
                                  nameC = Name "m1",
                                  nameHsIdent = Identifier
                                    "exampleStruct_m1"},
                                fieldComment = Nothing},
                              structFieldType =
                              TypeMacroTypedef
                                NamePair {
                                  nameC = Name "M1",
                                  nameHsIdent = Identifier "M1"}
                                NameOriginInSource,
                              structFieldOffset = 64,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "typedef_vs_macro.h:12:6",
                                fieldName = NamePair {
                                  nameC = Name "m2",
                                  nameHsIdent = Identifier
                                    "exampleStruct_m2"},
                                fieldComment = Nothing},
                              structFieldType =
                              TypeMacroTypedef
                                NamePair {
                                  nameC = Name "M2",
                                  nameHsIdent = Identifier "M2"}
                                NameOriginInSource,
                              structFieldOffset = 96,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "ExampleStruct",
                      commentLocation = Just
                        "typedef_vs_macro.h:8:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            [
                              "types/typedefs/typedef_vs_macro.h"],
                          headerInclude =
                          "types/typedefs/typedef_vs_macro.h"},
                      commentChildren = []}}
                (Add 4)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "exampleStruct_t1")
                      (Idx 5)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "exampleStruct_t2")
                      (Idx 5)
                      (Idx 1),
                    PokeCField
                      (HsStrLit "exampleStruct_m1")
                      (Idx 5)
                      (Idx 2),
                    PokeCField
                      (HsStrLit "exampleStruct_m2")
                      (Idx 5)
                      (Idx 3)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ExampleStruct",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "ExampleStruct",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "ExampleStruct"),
          hasCFieldInstanceFieldName =
          Name
            "@NsVar"
            "exampleStruct_t1",
          hasCFieldInstanceCFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "T1"),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "ExampleStruct"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "exampleStruct_t1",
          hasFieldInstanceFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "T1"),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "ExampleStruct"),
          hasCFieldInstanceFieldName =
          Name
            "@NsVar"
            "exampleStruct_t2",
          hasCFieldInstanceCFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "T2"),
          hasCFieldInstanceFieldOffset =
          4},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "ExampleStruct"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "exampleStruct_t2",
          hasFieldInstanceFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "T2"),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "ExampleStruct"),
          hasCFieldInstanceFieldName =
          Name
            "@NsVar"
            "exampleStruct_m1",
          hasCFieldInstanceCFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "M1"),
          hasCFieldInstanceFieldOffset =
          8},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "ExampleStruct"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "exampleStruct_m1",
          hasFieldInstanceFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "M1"),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "ExampleStruct"),
          hasCFieldInstanceFieldName =
          Name
            "@NsVar"
            "exampleStruct_m2",
          hasCFieldInstanceCFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "M2"),
          hasCFieldInstanceFieldOffset =
          12},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "ExampleStruct"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "exampleStruct_m2",
          hasFieldInstanceFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "M2"),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Uint64_t",
      newtypeConstr = Name
        "@NsConstr"
        "Uint64_t",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Uint64_t",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "typedef_vs_macro.h:15:9",
          declId = NamePair {
            nameC = Name "uint64_t",
            nameHsIdent = Identifier
              "Uint64_t"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "types/typedefs/typedef_vs_macro.h"],
              headerInclude =
              "types/typedefs/typedef_vs_macro.h"},
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Uint64_t",
              newtypeField = Name
                "@NsVar"
                "un_Uint64_t"},
            macroType = TypePrim
              (PrimIntegral PrimInt Signed)},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      newtypeInstances = Set.fromList
        [
          Bits,
          Bounded,
          Enum,
          Eq,
          FiniteBits,
          Integral,
          Ix,
          Num,
          Ord,
          Read,
          Real,
          Show,
          Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "uint64_t",
          commentLocation = Just
            "typedef_vs_macro.h:15:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "types/typedefs/typedef_vs_macro.h"],
              headerInclude =
              "types/typedefs/typedef_vs_macro.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass =
      FiniteBits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Foo",
      structConstr = Name
        "@NsConstr"
        "Foo",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "foo_a",
          fieldType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Uint64_t")),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "typedef_vs_macro.h:18:13",
                fieldName = NamePair {
                  nameC = Name "a",
                  nameHsIdent = Identifier
                    "foo_a"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "uint64_t",
                    nameHsIdent = Identifier
                      "Uint64_t"}
                  NameOriginInSource),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "a",
              commentLocation = Just
                "typedef_vs_macro.h:18:13",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/typedefs/typedef_vs_macro.h"],
                  headerInclude =
                  "types/typedefs/typedef_vs_macro.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "typedef_vs_macro.h:17:8",
            declId = NamePair {
              nameC = Name "foo",
              nameHsIdent = Identifier "Foo"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  [
                    "types/typedefs/typedef_vs_macro.h"],
                headerInclude =
                "types/typedefs/typedef_vs_macro.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Foo"),
              structSizeof = 8,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "typedef_vs_macro.h:18:13",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "foo_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeMacroTypedef
                      NamePair {
                        nameC = Name "uint64_t",
                        nameHsIdent = Identifier
                          "Uint64_t"}
                      NameOriginInSource),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec {
            declSpecC = Nothing,
            declSpecHs = Nothing}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "foo",
          commentLocation = Just
            "typedef_vs_macro.h:17:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "types/typedefs/typedef_vs_macro.h"],
              headerInclude =
              "types/typedefs/typedef_vs_macro.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Foo",
          structConstr = Name
            "@NsConstr"
            "Foo",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "foo_a",
              fieldType = HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Uint64_t")),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "typedef_vs_macro.h:18:13",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "foo_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeMacroTypedef
                      NamePair {
                        nameC = Name "uint64_t",
                        nameHsIdent = Identifier
                          "Uint64_t"}
                      NameOriginInSource),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "a",
                  commentLocation = Just
                    "typedef_vs_macro.h:18:13",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/typedefs/typedef_vs_macro.h"],
                      headerInclude =
                      "types/typedefs/typedef_vs_macro.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "typedef_vs_macro.h:17:8",
                declId = NamePair {
                  nameC = Name "foo",
                  nameHsIdent = Identifier "Foo"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      [
                        "types/typedefs/typedef_vs_macro.h"],
                    headerInclude =
                    "types/typedefs/typedef_vs_macro.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Foo"),
                  structSizeof = 8,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "typedef_vs_macro.h:18:13",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = Identifier
                            "foo_a"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeMacroTypedef
                          NamePair {
                            nameC = Name "uint64_t",
                            nameHsIdent = Identifier
                              "Uint64_t"}
                          NameOriginInSource),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec {
                declSpecC = Nothing,
                declSpecHs = Nothing}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "foo",
              commentLocation = Just
                "typedef_vs_macro.h:17:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/typedefs/typedef_vs_macro.h"],
                  headerInclude =
                  "types/typedefs/typedef_vs_macro.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 8,
          storableAlignment = 8,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Foo",
                  structConstr = Name
                    "@NsConstr"
                    "Foo",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "foo_a",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name
                            "@NsTypeConstr"
                            "Uint64_t")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "typedef_vs_macro.h:18:13",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "foo_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeMacroTypedef
                              NamePair {
                                nameC = Name "uint64_t",
                                nameHsIdent = Identifier
                                  "Uint64_t"}
                              NameOriginInSource),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "typedef_vs_macro.h:18:13",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/typedefs/typedef_vs_macro.h"],
                              headerInclude =
                              "types/typedefs/typedef_vs_macro.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "typedef_vs_macro.h:17:8",
                        declId = NamePair {
                          nameC = Name "foo",
                          nameHsIdent = Identifier "Foo"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              [
                                "types/typedefs/typedef_vs_macro.h"],
                            headerInclude =
                            "types/typedefs/typedef_vs_macro.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Foo"),
                          structSizeof = 8,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "typedef_vs_macro.h:18:13",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "foo_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeMacroTypedef
                                  NamePair {
                                    nameC = Name "uint64_t",
                                    nameHsIdent = Identifier
                                      "Uint64_t"}
                                  NameOriginInSource),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "foo",
                      commentLocation = Just
                        "typedef_vs_macro.h:17:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            [
                              "types/typedefs/typedef_vs_macro.h"],
                          headerInclude =
                          "types/typedefs/typedef_vs_macro.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "foo_a")
                  (Idx 0)]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Foo",
                  structConstr = Name
                    "@NsConstr"
                    "Foo",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "foo_a",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name
                            "@NsTypeConstr"
                            "Uint64_t")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "typedef_vs_macro.h:18:13",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "foo_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeMacroTypedef
                              NamePair {
                                nameC = Name "uint64_t",
                                nameHsIdent = Identifier
                                  "Uint64_t"}
                              NameOriginInSource),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "typedef_vs_macro.h:18:13",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/typedefs/typedef_vs_macro.h"],
                              headerInclude =
                              "types/typedefs/typedef_vs_macro.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "typedef_vs_macro.h:17:8",
                        declId = NamePair {
                          nameC = Name "foo",
                          nameHsIdent = Identifier "Foo"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              [
                                "types/typedefs/typedef_vs_macro.h"],
                            headerInclude =
                            "types/typedefs/typedef_vs_macro.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Foo"),
                          structSizeof = 8,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "typedef_vs_macro.h:18:13",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "foo_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeMacroTypedef
                                  NamePair {
                                    nameC = Name "uint64_t",
                                    nameHsIdent = Identifier
                                      "Uint64_t"}
                                  NameOriginInSource),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "foo",
                      commentLocation = Just
                        "typedef_vs_macro.h:17:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            [
                              "types/typedefs/typedef_vs_macro.h"],
                          headerInclude =
                          "types/typedefs/typedef_vs_macro.h"},
                      commentChildren = []}}
                (Add 1)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "foo_a")
                      (Idx 2)
                      (Idx 0)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Foo",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Foo"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "foo_a",
          hasCFieldInstanceCFieldType =
          HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Uint64_t")),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Foo"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "foo_a",
          hasFieldInstanceFieldType =
          HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Uint64_t")),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing}]
