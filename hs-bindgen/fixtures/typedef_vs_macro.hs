[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "T1",
      newtypeConstr = HsName
        "@NsConstr"
        "T1",
      newtypeField = Field {
        fieldName = HsName
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
            nameHsIdent = HsIdentifier
              "T1"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "typedef_vs_macro.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "T1",
              newtypeField = HsName
                "@NsVar"
                "un_T1"},
            typedefType = TypePrim
              (PrimIntegral PrimInt Signed)},
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
          Ix,
          Bounded,
          Read,
          Show,
          Bits,
          FiniteBits,
          Integral,
          Num,
          Real,
          Storable],
      newtypeComment = Just
        (Comment
          Nothing
          (Just "typedef_vs_macro.h:1:13")
          (Just "typedef_vs_macro.h")
          [])},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "T2",
      newtypeConstr = HsName
        "@NsConstr"
        "T2",
      newtypeField = Field {
        fieldName = HsName
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
            nameHsIdent = HsIdentifier
              "T2"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "typedef_vs_macro.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "T2",
              newtypeField = HsName
                "@NsVar"
                "un_T2"},
            typedefType = TypePrim
              (PrimChar
                (PrimSignImplicit
                  (Just Signed)))},
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
          Ix,
          Bounded,
          Read,
          Show,
          Bits,
          FiniteBits,
          Integral,
          Num,
          Real,
          Storable],
      newtypeComment = Just
        (Comment
          Nothing
          (Just "typedef_vs_macro.h:2:14")
          (Just "typedef_vs_macro.h")
          [])},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T2",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "M1",
      newtypeConstr = HsName
        "@NsConstr"
        "M1",
      newtypeField = Field {
        fieldName = HsName
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
            nameHsIdent = HsIdentifier
              "M1"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "typedef_vs_macro.h",
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "M1",
              newtypeField = HsName
                "@NsVar"
                "un_M1"},
            macroType = TypePrim
              (PrimIntegral PrimInt Signed)},
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
          Ix,
          Bounded,
          Read,
          Show,
          Bits,
          FiniteBits,
          Integral,
          Num,
          Real,
          Storable],
      newtypeComment = Just
        (Comment
          Nothing
          (Just "typedef_vs_macro.h:4:9")
          (Just "typedef_vs_macro.h")
          [])},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M1",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "M2",
      newtypeConstr = HsName
        "@NsConstr"
        "M2",
      newtypeField = Field {
        fieldName = HsName
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
            nameHsIdent = HsIdentifier
              "M2"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "typedef_vs_macro.h",
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "M2",
              newtypeField = HsName
                "@NsVar"
                "un_M2"},
            macroType = TypePrim
              (PrimChar
                (PrimSignImplicit Nothing))},
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
          Ix,
          Bounded,
          Read,
          Show,
          Bits,
          FiniteBits,
          Integral,
          Num,
          Real,
          Storable],
      newtypeComment = Just
        (Comment
          Nothing
          (Just "typedef_vs_macro.h:5:9")
          (Just "typedef_vs_macro.h")
          [])},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M2",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "M3",
      newtypeConstr = HsName
        "@NsConstr"
        "M3",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_M3",
        fieldType = HsConstArray
          3
          (HsPrimType HsPrimCInt),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "typedef_vs_macro.h:6:9",
          declId = NamePair {
            nameC = Name "M3",
            nameHsIdent = HsIdentifier
              "M3"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "typedef_vs_macro.h",
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "M3",
              newtypeField = HsName
                "@NsVar"
                "un_M3"},
            macroType = TypeConstArray
              3
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable],
      newtypeComment = Just
        (Comment
          Nothing
          (Just "typedef_vs_macro.h:6:9")
          (Just "typedef_vs_macro.h")
          [])},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M3",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "M4",
      newtypeConstr = HsName
        "@NsConstr"
        "M4",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_M4",
        fieldType = HsPtr
          (HsPrimType HsPrimCInt),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "typedef_vs_macro.h:7:9",
          declId = NamePair {
            nameC = Name "M4",
            nameHsIdent = HsIdentifier
              "M4"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "typedef_vs_macro.h",
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "M4",
              newtypeField = HsName
                "@NsVar"
                "un_M4"},
            macroType = TypePointer
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        (Comment
          Nothing
          (Just "typedef_vs_macro.h:7:9")
          (Just "typedef_vs_macro.h")
          [])},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M4",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M4",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M4",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "M4",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "ExampleStruct",
      structConstr = HsName
        "@NsConstr"
        "ExampleStruct",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "exampleStruct_t1",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "T1"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "typedef_vs_macro.h:10:6",
                fieldName = NamePair {
                  nameC = Name "t1",
                  nameHsIdent = HsIdentifier
                    "exampleStruct_t1"},
                fieldComment = Nothing},
              structFieldType = TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "T1",
                    nameHsIdent = HsIdentifier
                      "T1"}),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "typedef_vs_macro.h:10:6")
              (Just "typedef_vs_macro.h")
              [])},
        Field {
          fieldName = HsName
            "@NsVar"
            "exampleStruct_t2",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "T2"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "typedef_vs_macro.h:11:6",
                fieldName = NamePair {
                  nameC = Name "t2",
                  nameHsIdent = HsIdentifier
                    "exampleStruct_t2"},
                fieldComment = Nothing},
              structFieldType = TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "T2",
                    nameHsIdent = HsIdentifier
                      "T2"}),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "typedef_vs_macro.h:11:6")
              (Just "typedef_vs_macro.h")
              [])},
        Field {
          fieldName = HsName
            "@NsVar"
            "exampleStruct_m1",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "M1"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "typedef_vs_macro.h:12:6",
                fieldName = NamePair {
                  nameC = Name "m1",
                  nameHsIdent = HsIdentifier
                    "exampleStruct_m1"},
                fieldComment = Nothing},
              structFieldType =
              TypeMacroTypedef
                NamePair {
                  nameC = Name "M1",
                  nameHsIdent = HsIdentifier "M1"}
                NameOriginInSource,
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "typedef_vs_macro.h:12:6")
              (Just "typedef_vs_macro.h")
              [])},
        Field {
          fieldName = HsName
            "@NsVar"
            "exampleStruct_m2",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "M2"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "typedef_vs_macro.h:13:6",
                fieldName = NamePair {
                  nameC = Name "m2",
                  nameHsIdent = HsIdentifier
                    "exampleStruct_m2"},
                fieldComment = Nothing},
              structFieldType =
              TypeMacroTypedef
                NamePair {
                  nameC = Name "M2",
                  nameHsIdent = HsIdentifier "M2"}
                NameOriginInSource,
              structFieldOffset = 96,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "typedef_vs_macro.h:13:6")
              (Just "typedef_vs_macro.h")
              [])}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "typedef_vs_macro.h:9:8",
            declId = NamePair {
              nameC = Name "ExampleStruct",
              nameHsIdent = HsIdentifier
                "ExampleStruct"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "typedef_vs_macro.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "ExampleStruct"),
              structSizeof = 16,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "typedef_vs_macro.h:10:6",
                    fieldName = NamePair {
                      nameC = Name "t1",
                      nameHsIdent = HsIdentifier
                        "exampleStruct_t1"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "T1",
                        nameHsIdent = HsIdentifier
                          "T1"}),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "typedef_vs_macro.h:11:6",
                    fieldName = NamePair {
                      nameC = Name "t2",
                      nameHsIdent = HsIdentifier
                        "exampleStruct_t2"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "T2",
                        nameHsIdent = HsIdentifier
                          "T2"}),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "typedef_vs_macro.h:12:6",
                    fieldName = NamePair {
                      nameC = Name "m1",
                      nameHsIdent = HsIdentifier
                        "exampleStruct_m1"},
                    fieldComment = Nothing},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = Name "M1",
                      nameHsIdent = HsIdentifier "M1"}
                    NameOriginInSource,
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "typedef_vs_macro.h:13:6",
                    fieldName = NamePair {
                      nameC = Name "m2",
                      nameHsIdent = HsIdentifier
                        "exampleStruct_m2"},
                    fieldComment = Nothing},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = Name "M2",
                      nameHsIdent = HsIdentifier "M2"}
                    NameOriginInSource,
                  structFieldOffset = 96,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            TypeSpec {
              typeSpecModule = Nothing,
              typeSpecIdentifier = Nothing,
              typeSpecInstances = Map.fromList
                []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        (Comment
          Nothing
          (Just "typedef_vs_macro.h:9:8")
          (Just "typedef_vs_macro.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "ExampleStruct",
          structConstr = HsName
            "@NsConstr"
            "ExampleStruct",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "exampleStruct_t1",
              fieldType = HsTypRef
                (HsName "@NsTypeConstr" "T1"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "typedef_vs_macro.h:10:6",
                    fieldName = NamePair {
                      nameC = Name "t1",
                      nameHsIdent = HsIdentifier
                        "exampleStruct_t1"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "T1",
                        nameHsIdent = HsIdentifier
                          "T1"}),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "typedef_vs_macro.h:10:6")
                  (Just "typedef_vs_macro.h")
                  [])},
            Field {
              fieldName = HsName
                "@NsVar"
                "exampleStruct_t2",
              fieldType = HsTypRef
                (HsName "@NsTypeConstr" "T2"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "typedef_vs_macro.h:11:6",
                    fieldName = NamePair {
                      nameC = Name "t2",
                      nameHsIdent = HsIdentifier
                        "exampleStruct_t2"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "T2",
                        nameHsIdent = HsIdentifier
                          "T2"}),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "typedef_vs_macro.h:11:6")
                  (Just "typedef_vs_macro.h")
                  [])},
            Field {
              fieldName = HsName
                "@NsVar"
                "exampleStruct_m1",
              fieldType = HsTypRef
                (HsName "@NsTypeConstr" "M1"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "typedef_vs_macro.h:12:6",
                    fieldName = NamePair {
                      nameC = Name "m1",
                      nameHsIdent = HsIdentifier
                        "exampleStruct_m1"},
                    fieldComment = Nothing},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = Name "M1",
                      nameHsIdent = HsIdentifier "M1"}
                    NameOriginInSource,
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "typedef_vs_macro.h:12:6")
                  (Just "typedef_vs_macro.h")
                  [])},
            Field {
              fieldName = HsName
                "@NsVar"
                "exampleStruct_m2",
              fieldType = HsTypRef
                (HsName "@NsTypeConstr" "M2"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "typedef_vs_macro.h:13:6",
                    fieldName = NamePair {
                      nameC = Name "m2",
                      nameHsIdent = HsIdentifier
                        "exampleStruct_m2"},
                    fieldComment = Nothing},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = Name "M2",
                      nameHsIdent = HsIdentifier "M2"}
                    NameOriginInSource,
                  structFieldOffset = 96,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "typedef_vs_macro.h:13:6")
                  (Just "typedef_vs_macro.h")
                  [])}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "typedef_vs_macro.h:9:8",
                declId = NamePair {
                  nameC = Name "ExampleStruct",
                  nameHsIdent = HsIdentifier
                    "ExampleStruct"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader =
                "typedef_vs_macro.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName
                      "@NsConstr"
                      "ExampleStruct"),
                  structSizeof = 16,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "typedef_vs_macro.h:10:6",
                        fieldName = NamePair {
                          nameC = Name "t1",
                          nameHsIdent = HsIdentifier
                            "exampleStruct_t1"},
                        fieldComment = Nothing},
                      structFieldType = TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "T1",
                            nameHsIdent = HsIdentifier
                              "T1"}),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "typedef_vs_macro.h:11:6",
                        fieldName = NamePair {
                          nameC = Name "t2",
                          nameHsIdent = HsIdentifier
                            "exampleStruct_t2"},
                        fieldComment = Nothing},
                      structFieldType = TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "T2",
                            nameHsIdent = HsIdentifier
                              "T2"}),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "typedef_vs_macro.h:12:6",
                        fieldName = NamePair {
                          nameC = Name "m1",
                          nameHsIdent = HsIdentifier
                            "exampleStruct_m1"},
                        fieldComment = Nothing},
                      structFieldType =
                      TypeMacroTypedef
                        NamePair {
                          nameC = Name "M1",
                          nameHsIdent = HsIdentifier "M1"}
                        NameOriginInSource,
                      structFieldOffset = 64,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "typedef_vs_macro.h:13:6",
                        fieldName = NamePair {
                          nameC = Name "m2",
                          nameHsIdent = HsIdentifier
                            "exampleStruct_m2"},
                        fieldComment = Nothing},
                      structFieldType =
                      TypeMacroTypedef
                        NamePair {
                          nameC = Name "M2",
                          nameHsIdent = HsIdentifier "M2"}
                        NameOriginInSource,
                      structFieldOffset = 96,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                TypeSpec {
                  typeSpecModule = Nothing,
                  typeSpecIdentifier = Nothing,
                  typeSpecInstances = Map.fromList
                    []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            (Comment
              Nothing
              (Just "typedef_vs_macro.h:9:8")
              (Just "typedef_vs_macro.h")
              [])}
        StorableInstance {
          storableSizeOf = 16,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "ExampleStruct",
                  structConstr = HsName
                    "@NsConstr"
                    "ExampleStruct",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "exampleStruct_t1",
                      fieldType = HsTypRef
                        (HsName "@NsTypeConstr" "T1"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "typedef_vs_macro.h:10:6",
                            fieldName = NamePair {
                              nameC = Name "t1",
                              nameHsIdent = HsIdentifier
                                "exampleStruct_t1"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefRegular
                              NamePair {
                                nameC = Name "T1",
                                nameHsIdent = HsIdentifier
                                  "T1"}),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "typedef_vs_macro.h:10:6")
                          (Just "typedef_vs_macro.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "exampleStruct_t2",
                      fieldType = HsTypRef
                        (HsName "@NsTypeConstr" "T2"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "typedef_vs_macro.h:11:6",
                            fieldName = NamePair {
                              nameC = Name "t2",
                              nameHsIdent = HsIdentifier
                                "exampleStruct_t2"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefRegular
                              NamePair {
                                nameC = Name "T2",
                                nameHsIdent = HsIdentifier
                                  "T2"}),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "typedef_vs_macro.h:11:6")
                          (Just "typedef_vs_macro.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "exampleStruct_m1",
                      fieldType = HsTypRef
                        (HsName "@NsTypeConstr" "M1"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "typedef_vs_macro.h:12:6",
                            fieldName = NamePair {
                              nameC = Name "m1",
                              nameHsIdent = HsIdentifier
                                "exampleStruct_m1"},
                            fieldComment = Nothing},
                          structFieldType =
                          TypeMacroTypedef
                            NamePair {
                              nameC = Name "M1",
                              nameHsIdent = HsIdentifier "M1"}
                            NameOriginInSource,
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "typedef_vs_macro.h:12:6")
                          (Just "typedef_vs_macro.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "exampleStruct_m2",
                      fieldType = HsTypRef
                        (HsName "@NsTypeConstr" "M2"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "typedef_vs_macro.h:13:6",
                            fieldName = NamePair {
                              nameC = Name "m2",
                              nameHsIdent = HsIdentifier
                                "exampleStruct_m2"},
                            fieldComment = Nothing},
                          structFieldType =
                          TypeMacroTypedef
                            NamePair {
                              nameC = Name "M2",
                              nameHsIdent = HsIdentifier "M2"}
                            NameOriginInSource,
                          structFieldOffset = 96,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "typedef_vs_macro.h:13:6")
                          (Just "typedef_vs_macro.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "typedef_vs_macro.h:9:8",
                        declId = NamePair {
                          nameC = Name "ExampleStruct",
                          nameHsIdent = HsIdentifier
                            "ExampleStruct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "typedef_vs_macro.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "ExampleStruct"),
                          structSizeof = 16,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "typedef_vs_macro.h:10:6",
                                fieldName = NamePair {
                                  nameC = Name "t1",
                                  nameHsIdent = HsIdentifier
                                    "exampleStruct_t1"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefRegular
                                  NamePair {
                                    nameC = Name "T1",
                                    nameHsIdent = HsIdentifier
                                      "T1"}),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "typedef_vs_macro.h:11:6",
                                fieldName = NamePair {
                                  nameC = Name "t2",
                                  nameHsIdent = HsIdentifier
                                    "exampleStruct_t2"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefRegular
                                  NamePair {
                                    nameC = Name "T2",
                                    nameHsIdent = HsIdentifier
                                      "T2"}),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "typedef_vs_macro.h:12:6",
                                fieldName = NamePair {
                                  nameC = Name "m1",
                                  nameHsIdent = HsIdentifier
                                    "exampleStruct_m1"},
                                fieldComment = Nothing},
                              structFieldType =
                              TypeMacroTypedef
                                NamePair {
                                  nameC = Name "M1",
                                  nameHsIdent = HsIdentifier "M1"}
                                NameOriginInSource,
                              structFieldOffset = 64,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "typedef_vs_macro.h:13:6",
                                fieldName = NamePair {
                                  nameC = Name "m2",
                                  nameHsIdent = HsIdentifier
                                    "exampleStruct_m2"},
                                fieldComment = Nothing},
                              structFieldType =
                              TypeMacroTypedef
                                NamePair {
                                  nameC = Name "M2",
                                  nameHsIdent = HsIdentifier "M2"}
                                NameOriginInSource,
                              structFieldOffset = 96,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "typedef_vs_macro.h:9:8")
                      (Just "typedef_vs_macro.h")
                      [])})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 4,
                PeekByteOff (Idx 0) 8,
                PeekByteOff (Idx 0) 12]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "ExampleStruct",
                  structConstr = HsName
                    "@NsConstr"
                    "ExampleStruct",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "exampleStruct_t1",
                      fieldType = HsTypRef
                        (HsName "@NsTypeConstr" "T1"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "typedef_vs_macro.h:10:6",
                            fieldName = NamePair {
                              nameC = Name "t1",
                              nameHsIdent = HsIdentifier
                                "exampleStruct_t1"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefRegular
                              NamePair {
                                nameC = Name "T1",
                                nameHsIdent = HsIdentifier
                                  "T1"}),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "typedef_vs_macro.h:10:6")
                          (Just "typedef_vs_macro.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "exampleStruct_t2",
                      fieldType = HsTypRef
                        (HsName "@NsTypeConstr" "T2"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "typedef_vs_macro.h:11:6",
                            fieldName = NamePair {
                              nameC = Name "t2",
                              nameHsIdent = HsIdentifier
                                "exampleStruct_t2"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefRegular
                              NamePair {
                                nameC = Name "T2",
                                nameHsIdent = HsIdentifier
                                  "T2"}),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "typedef_vs_macro.h:11:6")
                          (Just "typedef_vs_macro.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "exampleStruct_m1",
                      fieldType = HsTypRef
                        (HsName "@NsTypeConstr" "M1"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "typedef_vs_macro.h:12:6",
                            fieldName = NamePair {
                              nameC = Name "m1",
                              nameHsIdent = HsIdentifier
                                "exampleStruct_m1"},
                            fieldComment = Nothing},
                          structFieldType =
                          TypeMacroTypedef
                            NamePair {
                              nameC = Name "M1",
                              nameHsIdent = HsIdentifier "M1"}
                            NameOriginInSource,
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "typedef_vs_macro.h:12:6")
                          (Just "typedef_vs_macro.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "exampleStruct_m2",
                      fieldType = HsTypRef
                        (HsName "@NsTypeConstr" "M2"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "typedef_vs_macro.h:13:6",
                            fieldName = NamePair {
                              nameC = Name "m2",
                              nameHsIdent = HsIdentifier
                                "exampleStruct_m2"},
                            fieldComment = Nothing},
                          structFieldType =
                          TypeMacroTypedef
                            NamePair {
                              nameC = Name "M2",
                              nameHsIdent = HsIdentifier "M2"}
                            NameOriginInSource,
                          structFieldOffset = 96,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "typedef_vs_macro.h:13:6")
                          (Just "typedef_vs_macro.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "typedef_vs_macro.h:9:8",
                        declId = NamePair {
                          nameC = Name "ExampleStruct",
                          nameHsIdent = HsIdentifier
                            "ExampleStruct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "typedef_vs_macro.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "ExampleStruct"),
                          structSizeof = 16,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "typedef_vs_macro.h:10:6",
                                fieldName = NamePair {
                                  nameC = Name "t1",
                                  nameHsIdent = HsIdentifier
                                    "exampleStruct_t1"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefRegular
                                  NamePair {
                                    nameC = Name "T1",
                                    nameHsIdent = HsIdentifier
                                      "T1"}),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "typedef_vs_macro.h:11:6",
                                fieldName = NamePair {
                                  nameC = Name "t2",
                                  nameHsIdent = HsIdentifier
                                    "exampleStruct_t2"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefRegular
                                  NamePair {
                                    nameC = Name "T2",
                                    nameHsIdent = HsIdentifier
                                      "T2"}),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "typedef_vs_macro.h:12:6",
                                fieldName = NamePair {
                                  nameC = Name "m1",
                                  nameHsIdent = HsIdentifier
                                    "exampleStruct_m1"},
                                fieldComment = Nothing},
                              structFieldType =
                              TypeMacroTypedef
                                NamePair {
                                  nameC = Name "M1",
                                  nameHsIdent = HsIdentifier "M1"}
                                NameOriginInSource,
                              structFieldOffset = 64,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "typedef_vs_macro.h:13:6",
                                fieldName = NamePair {
                                  nameC = Name "m2",
                                  nameHsIdent = HsIdentifier
                                    "exampleStruct_m2"},
                                fieldComment = Nothing},
                              structFieldType =
                              TypeMacroTypedef
                                NamePair {
                                  nameC = Name "M2",
                                  nameHsIdent = HsIdentifier "M2"}
                                NameOriginInSource,
                              structFieldOffset = 96,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "typedef_vs_macro.h:9:8")
                      (Just "typedef_vs_macro.h")
                      [])}
                (Add 4)
                (Seq
                  [
                    PokeByteOff (Idx 5) 0 (Idx 0),
                    PokeByteOff (Idx 5) 4 (Idx 1),
                    PokeByteOff (Idx 5) 8 (Idx 2),
                    PokeByteOff
                      (Idx 5)
                      12
                      (Idx 3)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "ExampleStruct",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "ExampleStruct",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Uint64_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Uint64_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Uint64_t",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "typedef_vs_macro.h:16:9",
          declId = NamePair {
            nameC = Name "uint64_t",
            nameHsIdent = HsIdentifier
              "Uint64_t"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "typedef_vs_macro.h",
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Uint64_t",
              newtypeField = HsName
                "@NsVar"
                "un_Uint64_t"},
            macroType = TypePrim
              (PrimIntegral PrimInt Signed)},
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
          Ix,
          Bounded,
          Read,
          Show,
          Bits,
          FiniteBits,
          Integral,
          Num,
          Real,
          Storable],
      newtypeComment = Just
        (Comment
          Nothing
          (Just "typedef_vs_macro.h:16:9")
          (Just "typedef_vs_macro.h")
          [])},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Uint64_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Uint64_t",
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
            "foo_a",
          fieldType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Uint64_t")),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "typedef_vs_macro.h:19:13",
                fieldName = NamePair {
                  nameC = Name "a",
                  nameHsIdent = HsIdentifier
                    "foo_a"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "uint64_t",
                    nameHsIdent = HsIdentifier
                      "Uint64_t"}
                  NameOriginInSource),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just
                "typedef_vs_macro.h:19:13")
              (Just "typedef_vs_macro.h")
              [])}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "typedef_vs_macro.h:18:8",
            declId = NamePair {
              nameC = Name "foo",
              nameHsIdent = HsIdentifier
                "Foo"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "typedef_vs_macro.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Foo"),
              structSizeof = 8,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "typedef_vs_macro.h:19:13",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = HsIdentifier
                        "foo_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeMacroTypedef
                      NamePair {
                        nameC = Name "uint64_t",
                        nameHsIdent = HsIdentifier
                          "Uint64_t"}
                      NameOriginInSource),
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
        [Eq, Show, Storable],
      structComment = Just
        (Comment
          Nothing
          (Just "typedef_vs_macro.h:18:8")
          (Just "typedef_vs_macro.h")
          [])},
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
                "foo_a",
              fieldType = HsPtr
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Uint64_t")),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "typedef_vs_macro.h:19:13",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = HsIdentifier
                        "foo_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeMacroTypedef
                      NamePair {
                        nameC = Name "uint64_t",
                        nameHsIdent = HsIdentifier
                          "Uint64_t"}
                      NameOriginInSource),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just
                    "typedef_vs_macro.h:19:13")
                  (Just "typedef_vs_macro.h")
                  [])}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "typedef_vs_macro.h:18:8",
                declId = NamePair {
                  nameC = Name "foo",
                  nameHsIdent = HsIdentifier
                    "Foo"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader =
                "typedef_vs_macro.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Foo"),
                  structSizeof = 8,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "typedef_vs_macro.h:19:13",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = HsIdentifier
                            "foo_a"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeMacroTypedef
                          NamePair {
                            nameC = Name "uint64_t",
                            nameHsIdent = HsIdentifier
                              "Uint64_t"}
                          NameOriginInSource),
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
            [Eq, Show, Storable],
          structComment = Just
            (Comment
              Nothing
              (Just "typedef_vs_macro.h:18:8")
              (Just "typedef_vs_macro.h")
              [])}
        StorableInstance {
          storableSizeOf = 8,
          storableAlignment = 8,
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
                        "foo_a",
                      fieldType = HsPtr
                        (HsTypRef
                          (HsName
                            "@NsTypeConstr"
                            "Uint64_t")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "typedef_vs_macro.h:19:13",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = HsIdentifier
                                "foo_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeMacroTypedef
                              NamePair {
                                nameC = Name "uint64_t",
                                nameHsIdent = HsIdentifier
                                  "Uint64_t"}
                              NameOriginInSource),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just
                            "typedef_vs_macro.h:19:13")
                          (Just "typedef_vs_macro.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "typedef_vs_macro.h:18:8",
                        declId = NamePair {
                          nameC = Name "foo",
                          nameHsIdent = HsIdentifier
                            "Foo"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "typedef_vs_macro.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Foo"),
                          structSizeof = 8,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "typedef_vs_macro.h:19:13",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
                                    "foo_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeMacroTypedef
                                  NamePair {
                                    nameC = Name "uint64_t",
                                    nameHsIdent = HsIdentifier
                                      "Uint64_t"}
                                  NameOriginInSource),
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "typedef_vs_macro.h:18:8")
                      (Just "typedef_vs_macro.h")
                      [])})
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
                        "foo_a",
                      fieldType = HsPtr
                        (HsTypRef
                          (HsName
                            "@NsTypeConstr"
                            "Uint64_t")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "typedef_vs_macro.h:19:13",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = HsIdentifier
                                "foo_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeMacroTypedef
                              NamePair {
                                nameC = Name "uint64_t",
                                nameHsIdent = HsIdentifier
                                  "Uint64_t"}
                              NameOriginInSource),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just
                            "typedef_vs_macro.h:19:13")
                          (Just "typedef_vs_macro.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "typedef_vs_macro.h:18:8",
                        declId = NamePair {
                          nameC = Name "foo",
                          nameHsIdent = HsIdentifier
                            "Foo"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "typedef_vs_macro.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Foo"),
                          structSizeof = 8,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "typedef_vs_macro.h:19:13",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
                                    "foo_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeMacroTypedef
                                  NamePair {
                                    nameC = Name "uint64_t",
                                    nameHsIdent = HsIdentifier
                                      "Uint64_t"}
                                  NameOriginInSource),
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "typedef_vs_macro.h:18:8")
                      (Just "typedef_vs_macro.h")
                      [])}
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
      Nothing}]
