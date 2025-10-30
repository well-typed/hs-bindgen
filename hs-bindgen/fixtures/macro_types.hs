[
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "PtrInt",
      newtypeConstr = Name
        "@NsConstr"
        "PtrInt",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_PtrInt",
        fieldType = HsPtr
          (HsPrimType HsPrimCInt),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "macro_types.h:2:9",
          declId = NamePair {
            nameC = Name "PtrInt",
            nameHsIdent = Identifier
              "PtrInt"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_types.h"],
              headerInclude =
              "macro_types.h"},
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "PtrInt",
              newtypeField = Name
                "@NsVar"
                "un_PtrInt"},
            macroType = TypePointer
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = ModuleName
              "Example",
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "PtrInt",
          commentLocation = Just
            "macro_types.h:2:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_types.h"],
              headerInclude =
              "macro_types.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "PtrInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "PtrInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "PtrInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "PtrInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "PtrPtrChar",
      newtypeConstr = Name
        "@NsConstr"
        "PtrPtrChar",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_PtrPtrChar",
        fieldType = HsPtr
          (HsPtr
            (HsPrimType HsPrimCChar)),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "macro_types.h:5:9",
          declId = NamePair {
            nameC = Name "PtrPtrChar",
            nameHsIdent = Identifier
              "PtrPtrChar"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_types.h"],
              headerInclude =
              "macro_types.h"},
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "PtrPtrChar",
              newtypeField = Name
                "@NsVar"
                "un_PtrPtrChar"},
            macroType = TypePointer
              (TypePointer
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit Nothing))))},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = ModuleName
              "Example",
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "PtrPtrChar",
          commentLocation = Just
            "macro_types.h:5:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_types.h"],
              headerInclude =
              "macro_types.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "PtrPtrChar",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "PtrPtrChar",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "PtrPtrChar",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "PtrPtrChar",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "MTy",
      newtypeConstr = Name
        "@NsConstr"
        "MTy",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_MTy",
        fieldType = HsPrimType
          HsPrimCFloat,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "macro_types.h:8:9",
          declId = NamePair {
            nameC = Name "MTy",
            nameHsIdent = Identifier "MTy"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_types.h"],
              headerInclude =
              "macro_types.h"},
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "MTy",
              newtypeField = Name
                "@NsVar"
                "un_MTy"},
            macroType = TypePrim
              (PrimFloating PrimFloat)},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = ModuleName
              "Example",
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [
          Enum,
          Eq,
          Floating,
          Fractional,
          Num,
          Ord,
          Read,
          Real,
          RealFloat,
          RealFrac,
          Show,
          Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "MTy",
          commentLocation = Just
            "macro_types.h:8:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_types.h"],
              headerInclude =
              "macro_types.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MTy",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MTy",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MTy",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MTy",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MTy",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MTy",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Floating,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MTy",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass =
      Fractional,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MTy",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MTy",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MTy",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = RealFloat,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MTy",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = RealFrac,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MTy",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Tty",
      newtypeConstr = Name
        "@NsConstr"
        "Tty",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Tty",
        fieldType = HsTypRef
          (Name "@NsTypeConstr" "MTy"),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "macro_types.h:9:13",
          declId = NamePair {
            nameC = Name "tty",
            nameHsIdent = Identifier "Tty"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_types.h"],
              headerInclude =
              "macro_types.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Tty",
              newtypeField = Name
                "@NsVar"
                "un_Tty"},
            typedefType = TypeMacroTypedef
              NamePair {
                nameC = Name "MTy",
                nameHsIdent = Identifier "MTy"}
              NameOriginInSource},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = ModuleName
              "Example",
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [
          Enum,
          Eq,
          Floating,
          Fractional,
          Num,
          Ord,
          Read,
          Real,
          RealFloat,
          RealFrac,
          Show,
          Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "tty",
          commentLocation = Just
            "macro_types.h:9:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_types.h"],
              headerInclude =
              "macro_types.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Tty",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Tty",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Tty",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Tty",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Tty",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Tty",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Floating,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Tty",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass =
      Fractional,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Tty",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Tty",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Tty",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = RealFloat,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Tty",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = RealFrac,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Tty",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "UINT8_T",
      newtypeConstr = Name
        "@NsConstr"
        "UINT8_T",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_UINT8_T",
        fieldType = HsPrimType
          HsPrimCUChar,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "macro_types.h:11:9",
          declId = NamePair {
            nameC = Name "UINT8_T",
            nameHsIdent = Identifier
              "UINT8_T"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_types.h"],
              headerInclude =
              "macro_types.h"},
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "UINT8_T",
              newtypeField = Name
                "@NsVar"
                "un_UINT8_T"},
            macroType = TypePrim
              (PrimChar
                (PrimSignExplicit Unsigned))},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = ModuleName
              "Example",
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
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
          commentOrigin = Just "UINT8_T",
          commentLocation = Just
            "macro_types.h:11:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_types.h"],
              headerInclude =
              "macro_types.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "UINT8_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "UINT8_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "UINT8_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "UINT8_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "UINT8_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "UINT8_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "UINT8_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "UINT8_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "UINT8_T",
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
        "UINT8_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "UINT8_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "UINT8_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "UINT8_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "BOOLEAN_T",
      newtypeConstr = Name
        "@NsConstr"
        "BOOLEAN_T",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_BOOLEAN_T",
        fieldType = HsTypRef
          (Name
            "@NsTypeConstr"
            "UINT8_T"),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "macro_types.h:12:9",
          declId = NamePair {
            nameC = Name "BOOLEAN_T",
            nameHsIdent = Identifier
              "BOOLEAN_T"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_types.h"],
              headerInclude =
              "macro_types.h"},
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "BOOLEAN_T",
              newtypeField = Name
                "@NsVar"
                "un_BOOLEAN_T"},
            macroType = TypeMacroTypedef
              NamePair {
                nameC = Name "UINT8_T",
                nameHsIdent = Identifier
                  "UINT8_T"}
              NameOriginInSource},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = ModuleName
              "Example",
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
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
          commentOrigin = Just
            "BOOLEAN_T",
          commentLocation = Just
            "macro_types.h:12:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_types.h"],
              headerInclude =
              "macro_types.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOLEAN_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOLEAN_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOLEAN_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOLEAN_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOLEAN_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOLEAN_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOLEAN_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOLEAN_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOLEAN_T",
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
        "BOOLEAN_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOLEAN_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOLEAN_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "BOOLEAN_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Boolean_T",
      newtypeConstr = Name
        "@NsConstr"
        "Boolean_T",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Boolean_T",
        fieldType = HsTypRef
          (Name
            "@NsTypeConstr"
            "BOOLEAN_T"),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "macro_types.h:13:19",
          declId = NamePair {
            nameC = Name "boolean_T",
            nameHsIdent = Identifier
              "Boolean_T"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_types.h"],
              headerInclude =
              "macro_types.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Boolean_T",
              newtypeField = Name
                "@NsVar"
                "un_Boolean_T"},
            typedefType = TypeMacroTypedef
              NamePair {
                nameC = Name "BOOLEAN_T",
                nameHsIdent = Identifier
                  "BOOLEAN_T"}
              NameOriginInSource},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = ModuleName
              "Example",
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
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
          commentOrigin = Just
            "boolean_T",
          commentLocation = Just
            "macro_types.h:13:19",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_types.h"],
              headerInclude =
              "macro_types.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Boolean_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Boolean_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Boolean_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Boolean_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Boolean_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Boolean_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Boolean_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Boolean_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Boolean_T",
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
        "Boolean_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Boolean_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Boolean_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Boolean_T",
      deriveInstanceComment =
      Nothing}]
