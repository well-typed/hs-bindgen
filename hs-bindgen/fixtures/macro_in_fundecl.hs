[
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "I",
      newtypeConstr = Name
        "@NsConstr"
        "I",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_I",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "macro_in_fundecl.h:5:9",
          declId = NamePair {
            nameC = Name "I",
            nameHsIdent = Identifier "I"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "I",
              newtypeField = Name
                "@NsVar"
                "un_I"},
            macroType = TypePrim
              (PrimIntegral PrimInt Signed)},
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
          commentOrigin = Just "I",
          commentLocation = Just
            "macro_in_fundecl.h:5:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "I",
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
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "C",
      newtypeConstr = Name
        "@NsConstr"
        "C",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_C",
        fieldType = HsPrimType
          HsPrimCChar,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "macro_in_fundecl.h:6:9",
          declId = NamePair {
            nameC = Name "C",
            nameHsIdent = Identifier "C"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "C",
              newtypeField = Name
                "@NsVar"
                "un_C"},
            macroType = TypePrim
              (PrimChar
                (PrimSignImplicit Nothing))},
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
          commentOrigin = Just "C",
          commentLocation = Just
            "macro_in_fundecl.h:6:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "C",
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
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "F",
      newtypeConstr = Name
        "@NsConstr"
        "F",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_F",
        fieldType = HsPrimType
          HsPrimCFloat,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "macro_in_fundecl.h:7:9",
          declId = NamePair {
            nameC = Name "F",
            nameHsIdent = Identifier "F"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "F",
              newtypeField = Name
                "@NsVar"
                "un_F"},
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
          commentOrigin = Just "F",
          commentLocation = Just
            "macro_in_fundecl.h:7:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "F",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "F",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "F",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "F",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "F",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "F",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Floating,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "F",
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
        "F",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "F",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "F",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = RealFloat,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "F",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = RealFrac,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "F",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "L",
      newtypeConstr = Name
        "@NsConstr"
        "L",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_L",
        fieldType = HsPrimType
          HsPrimCLong,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "macro_in_fundecl.h:8:9",
          declId = NamePair {
            nameC = Name "L",
            nameHsIdent = Identifier "L"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "L",
              newtypeField = Name
                "@NsVar"
                "un_L"},
            macroType = TypePrim
              (PrimIntegral PrimLong Signed)},
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
          commentOrigin = Just "L",
          commentLocation = Just
            "macro_in_fundecl.h:8:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "L",
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
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "S",
      newtypeConstr = Name
        "@NsConstr"
        "S",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_S",
        fieldType = HsPrimType
          HsPrimCShort,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "macro_in_fundecl.h:9:9",
          declId = NamePair {
            nameC = Name "S",
            nameHsIdent = Identifier "S"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "S",
              newtypeField = Name
                "@NsVar"
                "un_S"},
            macroType = TypePrim
              (PrimIntegral
                PrimShort
                Signed)},
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
          commentOrigin = Just "S",
          commentLocation = Just
            "macro_in_fundecl.h:9:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S",
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
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "quux",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "F"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "y"),
          functionParameterType =
          HsPrimType HsPrimCChar,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCChar)),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_9c091e7a5fbe00eb",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "char hs_bindgen_test_macro_in_fundecl_9c091e7a5fbe00eb (\n",
              "  F arg1,\n",
              "  char arg2\n",
              ")\n",
              "{\n",
              "  return quux(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "F",
                  nameHsIdent = Identifier "F"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier "y"})
              (TypePrim
                (PrimChar
                  (PrimSignImplicit Nothing)))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimChar
              (PrimSignImplicit Nothing))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "quux",
          commentLocation = Just
            "macro_in_fundecl.h:12:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "wam",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCFloat,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "y"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "C")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "C")))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_ca29786771bf115c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "C *hs_bindgen_test_macro_in_fundecl_ca29786771bf115c (\n",
              "  float arg1,\n",
              "  C *arg2\n",
              ")\n",
              "{\n",
              "  return wam(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimFloating PrimFloat)),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier "y"})
              (TypePointer
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "C",
                    nameHsIdent = Identifier "C"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeMacroTypedef
              NamePair {
                nameC = Name "C",
                nameHsIdent = Identifier "C"}
              NameOriginInSource)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "wam",
          commentLocation = Just
            "macro_in_fundecl.h:13:4",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "foo1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCFloat,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "g"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimCInt))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "g",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCChar))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_a1ddb0ab90dd90ae",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "char *hs_bindgen_test_macro_in_fundecl_a1ddb0ab90dd90ae (\n",
              "  float arg1,\n",
              "  signed int (*arg2) (\n",
              "  signed int arg1\n",
              ")\n",
              ")\n",
              "{\n",
              "  return foo1(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimFloating PrimFloat)),
            _×_
              (Just
                NamePair {
                  nameC = Name "g",
                  nameHsIdent = Identifier "g"})
              (TypePointer
                (TypeFun
                  [
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypePrim
              (PrimChar
                (PrimSignImplicit
                  (Just Signed))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "foo1",
          commentLocation = Just
            "macro_in_fundecl.h:16:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "foo2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "F"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "g"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimCInt))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "g",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCChar))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_39158ea36a52c749",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "char *hs_bindgen_test_macro_in_fundecl_39158ea36a52c749 (\n",
              "  F arg1,\n",
              "  signed int (*arg2) (\n",
              "  signed int arg1\n",
              ")\n",
              ")\n",
              "{\n",
              "  return foo2(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "F",
                  nameHsIdent = Identifier "F"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "g",
                  nameHsIdent = Identifier "g"})
              (TypePointer
                (TypeFun
                  [
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypePrim
              (PrimChar
                (PrimSignImplicit Nothing)))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "foo2",
          commentLocation = Just
            "macro_in_fundecl.h:17:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "foo3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCFloat,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "g"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimCInt))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "g",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "C")))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_30c473506139927c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "C *hs_bindgen_test_macro_in_fundecl_30c473506139927c (\n",
              "  float arg1,\n",
              "  signed int (*arg2) (\n",
              "  signed int arg1\n",
              ")\n",
              ")\n",
              "{\n",
              "  return foo3(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimFloating PrimFloat)),
            _×_
              (Just
                NamePair {
                  nameC = Name "g",
                  nameHsIdent = Identifier "g"})
              (TypePointer
                (TypeFun
                  [
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeMacroTypedef
              NamePair {
                nameC = Name "C",
                nameHsIdent = Identifier "C"}
              NameOriginInSource)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "foo3",
          commentLocation = Just
            "macro_in_fundecl.h:18:4",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "bar1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCLong,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCShort)
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_ef6d9a2254300a4a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int (*hs_bindgen_test_macro_in_fundecl_ef6d9a2254300a4a (\n",
              "  signed long arg1\n",
              ")) (\n",
              "  signed short arg1\n",
              ")\n",
              "{\n",
              "  return bar1(arg1);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimIntegral
                  PrimLong
                  Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimShort Signed)]
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed)))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "bar1",
          commentLocation = Just
            "macro_in_fundecl.h:21:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "bar2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "L"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCShort)
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_6570ce6435b60197",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int (*hs_bindgen_test_macro_in_fundecl_6570ce6435b60197 (\n",
              "  L arg1\n",
              ")) (\n",
              "  signed short arg1\n",
              ")\n",
              "{\n",
              "  return bar2(arg1);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "L",
                  nameHsIdent = Identifier "L"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimShort Signed)]
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed)))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "bar2",
          commentLocation = Just
            "macro_in_fundecl.h:22:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "bar3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCLong,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "S"))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_ac35ce1ad86420e1",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int (*hs_bindgen_test_macro_in_fundecl_ac35ce1ad86420e1 (\n",
              "  signed long arg1\n",
              ")) (\n",
              "  S arg1\n",
              ")\n",
              "{\n",
              "  return bar3(arg1);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimIntegral
                  PrimLong
                  Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeFun
              [
                TypeMacroTypedef
                  NamePair {
                    nameC = Name "S",
                    nameHsIdent = Identifier "S"}
                  NameOriginInSource]
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed)))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "bar3",
          commentLocation = Just
            "macro_in_fundecl.h:23:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "bar4",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCLong,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCShort)
              (HsIO
                (HsTypRef
                  (Name "@NsTypeConstr" "I")))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_c5e59f203c41c1c2",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "I (*hs_bindgen_test_macro_in_fundecl_c5e59f203c41c1c2 (\n",
              "  signed long arg1\n",
              ")) (\n",
              "  signed short arg1\n",
              ")\n",
              "{\n",
              "  return bar4(arg1);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimIntegral
                  PrimLong
                  Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimShort Signed)]
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "I",
                  nameHsIdent = Identifier "I"}
                NameOriginInSource))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "bar4",
          commentLocation = Just
            "macro_in_fundecl.h:24:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "baz1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "i"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "i",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              2
              (HsConstArray
                3
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_f2b917ad9122f0e2",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int (*hs_bindgen_test_macro_in_fundecl_f2b917ad9122f0e2 (\n",
              "  signed int const arg1\n",
              "))[2][3]\n",
              "{\n",
              "  return baz1(arg1);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "i",
                  nameHsIdent = Identifier "i"})
              (TypeQualified
                TypeQualifierConst
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeConstArray
              2
              (TypeConstArray
                3
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "baz1",
          commentLocation = Just
            "macro_in_fundecl.h:27:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "baz2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "i"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "I"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "i",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              2
              (HsConstArray
                3
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_d27cd45b344a00d3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int (*hs_bindgen_test_macro_in_fundecl_d27cd45b344a00d3 (\n",
              "  I const arg1\n",
              "))[2][3]\n",
              "{\n",
              "  return baz2(arg1);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "i",
                  nameHsIdent = Identifier "i"})
              (TypeQualified
                TypeQualifierConst
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "I",
                    nameHsIdent = Identifier "I"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeConstArray
              2
              (TypeConstArray
                3
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "baz2",
          commentLocation = Just
            "macro_in_fundecl.h:35:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "baz3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "i"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "i",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              2
              (HsConstArray
                3
                (HsTypRef
                  (Name "@NsTypeConstr" "I")))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_c4ed14d761bc89ba",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "I (*hs_bindgen_test_macro_in_fundecl_c4ed14d761bc89ba (\n",
              "  signed int const arg1\n",
              "))[2][3]\n",
              "{\n",
              "  return baz3(arg1);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "i",
                  nameHsIdent = Identifier "i"})
              (TypeQualified
                TypeQualifierConst
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeConstArray
              2
              (TypeConstArray
                3
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "I",
                    nameHsIdent = Identifier "I"}
                  NameOriginInSource)))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "baz3",
          commentLocation = Just
            "macro_in_fundecl.h:43:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "no_args_no_void",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (Name "@NsTypeConstr" "I"))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_8d4283a1963012db",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "I hs_bindgen_test_macro_in_fundecl_8d4283a1963012db (void)\n",
              "{\n",
              "  return no_args_no_void();\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeMacroTypedef
            NamePair {
              nameC = Name "I",
              nameHsIdent = Identifier "I"}
            NameOriginInSource},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "no_args_no_void",
          commentLocation = Just
            "macro_in_fundecl.h:53:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "quux",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "F"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "y"),
          functionParameterType =
          HsPrimType HsPrimCChar,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCChar)),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_7542f939354dfc0b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "char hs_bindgen_test_macro_in_fundecl_7542f939354dfc0b (\n",
              "  F arg1,\n",
              "  char arg2\n",
              ")\n",
              "{\n",
              "  return quux(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "F",
                  nameHsIdent = Identifier "F"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier "y"})
              (TypePrim
                (PrimChar
                  (PrimSignImplicit Nothing)))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimChar
              (PrimSignImplicit Nothing))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "quux",
          commentLocation = Just
            "macro_in_fundecl.h:12:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "wam",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCFloat,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "y"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "C")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "C")))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_e19961744945a727",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "C *hs_bindgen_test_macro_in_fundecl_e19961744945a727 (\n",
              "  float arg1,\n",
              "  C *arg2\n",
              ")\n",
              "{\n",
              "  return wam(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimFloating PrimFloat)),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier "y"})
              (TypePointer
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "C",
                    nameHsIdent = Identifier "C"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeMacroTypedef
              NamePair {
                nameC = Name "C",
                nameHsIdent = Identifier "C"}
              NameOriginInSource)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "wam",
          commentLocation = Just
            "macro_in_fundecl.h:13:4",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "foo1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCFloat,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "g"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimCInt))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "g",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCChar))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_3ce18d84b02c9784",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "char *hs_bindgen_test_macro_in_fundecl_3ce18d84b02c9784 (\n",
              "  float arg1,\n",
              "  signed int (*arg2) (\n",
              "  signed int arg1\n",
              ")\n",
              ")\n",
              "{\n",
              "  return foo1(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimFloating PrimFloat)),
            _×_
              (Just
                NamePair {
                  nameC = Name "g",
                  nameHsIdent = Identifier "g"})
              (TypePointer
                (TypeFun
                  [
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypePrim
              (PrimChar
                (PrimSignImplicit
                  (Just Signed))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "foo1",
          commentLocation = Just
            "macro_in_fundecl.h:16:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "foo2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "F"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "g"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimCInt))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "g",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCChar))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_483294beebe8552d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "char *hs_bindgen_test_macro_in_fundecl_483294beebe8552d (\n",
              "  F arg1,\n",
              "  signed int (*arg2) (\n",
              "  signed int arg1\n",
              ")\n",
              ")\n",
              "{\n",
              "  return foo2(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "F",
                  nameHsIdent = Identifier "F"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "g",
                  nameHsIdent = Identifier "g"})
              (TypePointer
                (TypeFun
                  [
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypePrim
              (PrimChar
                (PrimSignImplicit Nothing)))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "foo2",
          commentLocation = Just
            "macro_in_fundecl.h:17:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "foo3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCFloat,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "g"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimCInt))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "g",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "C")))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_5fb7e024e43f13d7",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "C *hs_bindgen_test_macro_in_fundecl_5fb7e024e43f13d7 (\n",
              "  float arg1,\n",
              "  signed int (*arg2) (\n",
              "  signed int arg1\n",
              ")\n",
              ")\n",
              "{\n",
              "  return foo3(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimFloating PrimFloat)),
            _×_
              (Just
                NamePair {
                  nameC = Name "g",
                  nameHsIdent = Identifier "g"})
              (TypePointer
                (TypeFun
                  [
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeMacroTypedef
              NamePair {
                nameC = Name "C",
                nameHsIdent = Identifier "C"}
              NameOriginInSource)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "foo3",
          commentLocation = Just
            "macro_in_fundecl.h:18:4",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "bar1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCLong,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCShort)
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_5c62229deaba458d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int (*hs_bindgen_test_macro_in_fundecl_5c62229deaba458d (\n",
              "  signed long arg1\n",
              ")) (\n",
              "  signed short arg1\n",
              ")\n",
              "{\n",
              "  return bar1(arg1);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimIntegral
                  PrimLong
                  Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimShort Signed)]
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed)))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "bar1",
          commentLocation = Just
            "macro_in_fundecl.h:21:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "bar2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "L"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCShort)
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_120cb32a2369f37f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int (*hs_bindgen_test_macro_in_fundecl_120cb32a2369f37f (\n",
              "  L arg1\n",
              ")) (\n",
              "  signed short arg1\n",
              ")\n",
              "{\n",
              "  return bar2(arg1);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "L",
                  nameHsIdent = Identifier "L"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimShort Signed)]
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed)))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "bar2",
          commentLocation = Just
            "macro_in_fundecl.h:22:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "bar3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCLong,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "S"))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_8163c1d52847d0b4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int (*hs_bindgen_test_macro_in_fundecl_8163c1d52847d0b4 (\n",
              "  signed long arg1\n",
              ")) (\n",
              "  S arg1\n",
              ")\n",
              "{\n",
              "  return bar3(arg1);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimIntegral
                  PrimLong
                  Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeFun
              [
                TypeMacroTypedef
                  NamePair {
                    nameC = Name "S",
                    nameHsIdent = Identifier "S"}
                  NameOriginInSource]
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed)))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "bar3",
          commentLocation = Just
            "macro_in_fundecl.h:23:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "bar4",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCLong,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCShort)
              (HsIO
                (HsTypRef
                  (Name "@NsTypeConstr" "I")))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_d7df5128adb899bb",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "I (*hs_bindgen_test_macro_in_fundecl_d7df5128adb899bb (\n",
              "  signed long arg1\n",
              ")) (\n",
              "  signed short arg1\n",
              ")\n",
              "{\n",
              "  return bar4(arg1);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimIntegral
                  PrimLong
                  Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimShort Signed)]
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "I",
                  nameHsIdent = Identifier "I"}
                NameOriginInSource))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "bar4",
          commentLocation = Just
            "macro_in_fundecl.h:24:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "baz1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "i"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "i",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              2
              (HsConstArray
                3
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_af25911956e2e01d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int (*hs_bindgen_test_macro_in_fundecl_af25911956e2e01d (\n",
              "  signed int const arg1\n",
              "))[2][3]\n",
              "{\n",
              "  return baz1(arg1);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "i",
                  nameHsIdent = Identifier "i"})
              (TypeQualified
                TypeQualifierConst
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeConstArray
              2
              (TypeConstArray
                3
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "baz1",
          commentLocation = Just
            "macro_in_fundecl.h:27:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "baz2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "i"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "I"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "i",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              2
              (HsConstArray
                3
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_21c316299046fe75",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "signed int (*hs_bindgen_test_macro_in_fundecl_21c316299046fe75 (\n",
              "  I const arg1\n",
              "))[2][3]\n",
              "{\n",
              "  return baz2(arg1);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "i",
                  nameHsIdent = Identifier "i"})
              (TypeQualified
                TypeQualifierConst
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "I",
                    nameHsIdent = Identifier "I"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeConstArray
              2
              (TypeConstArray
                3
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "baz2",
          commentLocation = Just
            "macro_in_fundecl.h:35:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "baz3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "i"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "i",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              2
              (HsConstArray
                3
                (HsTypRef
                  (Name "@NsTypeConstr" "I")))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_8d077398ab5e3b39",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "I (*hs_bindgen_test_macro_in_fundecl_8d077398ab5e3b39 (\n",
              "  signed int const arg1\n",
              "))[2][3]\n",
              "{\n",
              "  return baz3(arg1);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "i",
                  nameHsIdent = Identifier "i"})
              (TypeQualified
                TypeQualifierConst
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeConstArray
              2
              (TypeConstArray
                3
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "I",
                    nameHsIdent = Identifier "I"}
                  NameOriginInSource)))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "baz3",
          commentLocation = Just
            "macro_in_fundecl.h:43:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "no_args_no_void",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (Name "@NsTypeConstr" "I"))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_26de47450bbc61e3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "I hs_bindgen_test_macro_in_fundecl_26de47450bbc61e3 (void)\n",
              "{\n",
              "  return no_args_no_void();\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeMacroTypedef
            NamePair {
              nameC = Name "I",
              nameHsIdent = Identifier "I"}
            NameOriginInSource},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "no_args_no_void",
          commentLocation = Just
            "macro_in_fundecl.h:53:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_in_fundecl.h"],
              headerInclude =
              "macro_in_fundecl.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_75296b863af23367",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "F"))
              (HsFun
                (HsPrimType HsPrimCChar)
                (HsIO
                  (HsPrimType HsPrimCChar)))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_75296b863af23367",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_quux_ptr */\n",
              "__attribute__ ((const))\n",
              "char (*hs_bindgen_test_macro_in_fundecl_75296b863af23367 (void)) (\n",
              "  F arg1,\n",
              "  char arg2\n",
              ")\n",
              "{\n",
              "  return &quux;\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "F",
                nameHsIdent = Identifier "F"}
              NameOriginInSource,
            TypePrim
              (PrimChar
                (PrimSignImplicit Nothing))]
          (TypePrim
            (PrimChar
              (PrimSignImplicit Nothing)))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_fd1ccca5616729da",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCFloat)
              (HsFun
                (HsPtr
                  (HsTypRef
                    (Name "@NsTypeConstr" "C")))
                (HsIO
                  (HsPtr
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "C")))))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_fd1ccca5616729da",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_wam_ptr */\n",
              "__attribute__ ((const))\n",
              "C *(*hs_bindgen_test_macro_in_fundecl_fd1ccca5616729da (void)) (\n",
              "  float arg1,\n",
              "  C *arg2\n",
              ")\n",
              "{\n",
              "  return &wam;\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimFloating PrimFloat),
            TypePointer
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "C",
                  nameHsIdent = Identifier "C"}
                NameOriginInSource)]
          (TypePointer
            (TypeMacroTypedef
              NamePair {
                nameC = Name "C",
                nameHsIdent = Identifier "C"}
              NameOriginInSource))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_786c8d7bfea481fd",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCFloat)
              (HsFun
                (HsFunPtr
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsIO (HsPrimType HsPrimCInt))))
                (HsIO
                  (HsPtr
                    (HsPrimType HsPrimCChar))))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_786c8d7bfea481fd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_foo1_ptr */\n",
              "__attribute__ ((const))\n",
              "char *(*hs_bindgen_test_macro_in_fundecl_786c8d7bfea481fd (void)) (\n",
              "  float arg1,\n",
              "  signed int (*arg2) (\n",
              "  signed int arg1\n",
              ")\n",
              ")\n",
              "{\n",
              "  return &foo1;\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimFloating PrimFloat),
            TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed)]
                (TypePrim
                  (PrimIntegral PrimInt Signed)))]
          (TypePointer
            (TypePrim
              (PrimChar
                (PrimSignImplicit
                  (Just Signed)))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_42a47aecc35f5bda",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "F"))
              (HsFun
                (HsFunPtr
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsIO (HsPrimType HsPrimCInt))))
                (HsIO
                  (HsPtr
                    (HsPrimType HsPrimCChar))))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_42a47aecc35f5bda",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_foo2_ptr */\n",
              "__attribute__ ((const))\n",
              "char *(*hs_bindgen_test_macro_in_fundecl_42a47aecc35f5bda (void)) (\n",
              "  F arg1,\n",
              "  signed int (*arg2) (\n",
              "  signed int arg1\n",
              ")\n",
              ")\n",
              "{\n",
              "  return &foo2;\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "F",
                nameHsIdent = Identifier "F"}
              NameOriginInSource,
            TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed)]
                (TypePrim
                  (PrimIntegral PrimInt Signed)))]
          (TypePointer
            (TypePrim
              (PrimChar
                (PrimSignImplicit Nothing))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_17760ec60140242e",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCFloat)
              (HsFun
                (HsFunPtr
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsIO (HsPrimType HsPrimCInt))))
                (HsIO
                  (HsPtr
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "C")))))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_17760ec60140242e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_foo3_ptr */\n",
              "__attribute__ ((const))\n",
              "C *(*hs_bindgen_test_macro_in_fundecl_17760ec60140242e (void)) (\n",
              "  float arg1,\n",
              "  signed int (*arg2) (\n",
              "  signed int arg1\n",
              ")\n",
              ")\n",
              "{\n",
              "  return &foo3;\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimFloating PrimFloat),
            TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed)]
                (TypePrim
                  (PrimIntegral PrimInt Signed)))]
          (TypePointer
            (TypeMacroTypedef
              NamePair {
                nameC = Name "C",
                nameHsIdent = Identifier "C"}
              NameOriginInSource))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_13fa512840072e8d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCLong)
              (HsIO
                (HsFunPtr
                  (HsFun
                    (HsPrimType HsPrimCShort)
                    (HsIO
                      (HsPrimType HsPrimCInt)))))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_13fa512840072e8d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_bar1_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*(*hs_bindgen_test_macro_in_fundecl_13fa512840072e8d (void)) (\n",
              "  signed long arg1\n",
              ")) (\n",
              "  signed short arg1\n",
              ")\n",
              "{\n",
              "  return &bar1;\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimLong Signed)]
          (TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimShort Signed)]
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_0d63f3c4f98f04a3",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "L"))
              (HsIO
                (HsFunPtr
                  (HsFun
                    (HsPrimType HsPrimCShort)
                    (HsIO
                      (HsPrimType HsPrimCInt)))))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_0d63f3c4f98f04a3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_bar2_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*(*hs_bindgen_test_macro_in_fundecl_0d63f3c4f98f04a3 (void)) (\n",
              "  L arg1\n",
              ")) (\n",
              "  signed short arg1\n",
              ")\n",
              "{\n",
              "  return &bar2;\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "L",
                nameHsIdent = Identifier "L"}
              NameOriginInSource]
          (TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimShort Signed)]
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_8bd44eebdbce7f71",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCLong)
              (HsIO
                (HsFunPtr
                  (HsFun
                    (HsTypRef
                      (Name "@NsTypeConstr" "S"))
                    (HsIO
                      (HsPrimType HsPrimCInt)))))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_8bd44eebdbce7f71",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_bar3_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*(*hs_bindgen_test_macro_in_fundecl_8bd44eebdbce7f71 (void)) (\n",
              "  signed long arg1\n",
              ")) (\n",
              "  S arg1\n",
              ")\n",
              "{\n",
              "  return &bar3;\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimLong Signed)]
          (TypePointer
            (TypeFun
              [
                TypeMacroTypedef
                  NamePair {
                    nameC = Name "S",
                    nameHsIdent = Identifier "S"}
                  NameOriginInSource]
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_0515cdde3c6f0f19",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCLong)
              (HsIO
                (HsFunPtr
                  (HsFun
                    (HsPrimType HsPrimCShort)
                    (HsIO
                      (HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "I"))))))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_0515cdde3c6f0f19",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_bar4_ptr */\n",
              "__attribute__ ((const))\n",
              "I (*(*hs_bindgen_test_macro_in_fundecl_0515cdde3c6f0f19 (void)) (\n",
              "  signed long arg1\n",
              ")) (\n",
              "  signed short arg1\n",
              ")\n",
              "{\n",
              "  return &bar4;\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimLong Signed)]
          (TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimShort Signed)]
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "I",
                  nameHsIdent = Identifier "I"}
                NameOriginInSource)))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_8edeef2444de2cee",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO
                (HsPtr
                  (HsConstArray
                    2
                    (HsConstArray
                      3
                      (HsPrimType HsPrimCInt)))))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_8edeef2444de2cee",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_baz1_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*(*hs_bindgen_test_macro_in_fundecl_8edeef2444de2cee (void)) (\n",
              "  signed int const arg1\n",
              "))[2][3]\n",
              "{\n",
              "  return &baz1;\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeQualified
              TypeQualifierConst
              (TypePrim
                (PrimIntegral PrimInt Signed))]
          (TypePointer
            (TypeConstArray
              2
              (TypeConstArray
                3
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_61853d26cc39ced6",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "I"))
              (HsIO
                (HsPtr
                  (HsConstArray
                    2
                    (HsConstArray
                      3
                      (HsPrimType HsPrimCInt)))))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_61853d26cc39ced6",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_baz2_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*(*hs_bindgen_test_macro_in_fundecl_61853d26cc39ced6 (void)) (\n",
              "  I const arg1\n",
              "))[2][3]\n",
              "{\n",
              "  return &baz2;\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeQualified
              TypeQualifierConst
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "I",
                  nameHsIdent = Identifier "I"}
                NameOriginInSource)]
          (TypePointer
            (TypeConstArray
              2
              (TypeConstArray
                3
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_b465262d2f67a146",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO
                (HsPtr
                  (HsConstArray
                    2
                    (HsConstArray
                      3
                      (HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "I"))))))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_b465262d2f67a146",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_baz3_ptr */\n",
              "__attribute__ ((const))\n",
              "I (*(*hs_bindgen_test_macro_in_fundecl_b465262d2f67a146 (void)) (\n",
              "  signed int const arg1\n",
              "))[2][3]\n",
              "{\n",
              "  return &baz3;\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeQualified
              TypeQualifierConst
              (TypePrim
                (PrimIntegral PrimInt Signed))]
          (TypePointer
            (TypeConstArray
              2
              (TypeConstArray
                3
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "I",
                    nameHsIdent = Identifier "I"}
                  NameOriginInSource))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_452280b5085b4ccd",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsTypRef
                (Name "@NsTypeConstr" "I"))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_452280b5085b4ccd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_no_args_no_void_ptr */\n",
              "__attribute__ ((const))\n",
              "I (*hs_bindgen_test_macro_in_fundecl_452280b5085b4ccd (void)) (void)\n",
              "{\n",
              "  return &no_args_no_void;\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
          (TypeMacroTypedef
            NamePair {
              nameC = Name "I",
              nameHsIdent = Identifier "I"}
            NameOriginInSource)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
