[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "I",
      newtypeConstr = HsName
        "@NsConstr"
        "I",
      newtypeField = Field {
        fieldName = HsName
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
            nameHsIdent = HsIdentifier "I"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "macro_in_fundecl.h",
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "I",
              newtypeField = HsName
                "@NsVar"
                "un_I"},
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
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "I",
          commentLocation = Just
            "macro_in_fundecl.h:5:9",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "I",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "C",
      newtypeConstr = HsName
        "@NsConstr"
        "C",
      newtypeField = Field {
        fieldName = HsName
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
            nameHsIdent = HsIdentifier "C"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "macro_in_fundecl.h",
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "C",
              newtypeField = HsName
                "@NsVar"
                "un_C"},
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
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "C",
          commentLocation = Just
            "macro_in_fundecl.h:6:9",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "F",
      newtypeConstr = HsName
        "@NsConstr"
        "F",
      newtypeField = Field {
        fieldName = HsName
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
            nameHsIdent = HsIdentifier "F"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "macro_in_fundecl.h",
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "F",
              newtypeField = HsName
                "@NsVar"
                "un_F"},
            macroType = TypePrim
              (PrimFloating PrimFloat)},
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
          Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "F",
          commentLocation = Just
            "macro_in_fundecl.h:7:9",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "F",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "F",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "F",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "F",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "F",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "F",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Floating,
      deriveInstanceName = HsName
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "F",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "F",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "F",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = RealFloat,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "F",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = RealFrac,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "F",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "L",
      newtypeConstr = HsName
        "@NsConstr"
        "L",
      newtypeField = Field {
        fieldName = HsName
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
            nameHsIdent = HsIdentifier "L"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "macro_in_fundecl.h",
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "L",
              newtypeField = HsName
                "@NsVar"
                "un_L"},
            macroType = TypePrim
              (PrimIntegral PrimLong Signed)},
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
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "L",
          commentLocation = Just
            "macro_in_fundecl.h:8:9",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "L",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "S",
      newtypeConstr = HsName
        "@NsConstr"
        "S",
      newtypeField = Field {
        fieldName = HsName
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
            nameHsIdent = HsIdentifier "S"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "macro_in_fundecl.h",
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "S",
              newtypeField = HsName
                "@NsVar"
                "un_S"},
            macroType = TypePrim
              (PrimIntegral
                PrimShort
                Signed)},
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
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "S",
          commentLocation = Just
            "macro_in_fundecl.h:9:9",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "quux",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "F"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "y"),
          functionParameterType =
          HsPrimType HsPrimCChar,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCChar)),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_206ab5e09886d1e7",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "char hs_bindgen_test_macro_in_fundecl_206ab5e09886d1e7 (F arg1, char arg2) { return quux(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "F",
                  nameHsIdent = HsIdentifier "F"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier "y"})
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
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_14c52c6a6a7242b5",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "F"))
              (HsFun
                (HsPrimType HsPrimCChar)
                (HsIO
                  (HsPrimType HsPrimCChar)))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_14c52c6a6a7242b5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_quux_ptr */ __attribute__ ((const)) char (*hs_bindgen_test_macro_in_fundecl_14c52c6a6a7242b5 (void)) (F arg1, char arg2) { return &quux; } ",
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "F",
                nameHsIdent = HsIdentifier "F"}
              NameOriginInSource,
            TypePrim
              (PrimChar
                (PrimSignImplicit Nothing))]
          (TypePrim
            (PrimChar
              (PrimSignImplicit Nothing)))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "quux",
          commentLocation = Just
            "macro_in_fundecl.h:12:6",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "wam",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCFloat,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "y"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "C")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "C")))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_5e951ebfcf556c2b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "C *hs_bindgen_test_macro_in_fundecl_5e951ebfcf556c2b (float arg1, C *arg2) { return wam(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypePrim
                (PrimFloating PrimFloat)),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier "y"})
              (TypePointer
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "C",
                    nameHsIdent = HsIdentifier "C"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeMacroTypedef
              NamePair {
                nameC = Name "C",
                nameHsIdent = HsIdentifier "C"}
              NameOriginInSource)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "wam",
          commentLocation = Just
            "macro_in_fundecl.h:13:4",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_663b869ffc1f7213",
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
                    (HsName "@NsTypeConstr" "C")))
                (HsIO
                  (HsPtr
                    (HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "C")))))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_663b869ffc1f7213",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_wam_ptr */ __attribute__ ((const)) C *(*hs_bindgen_test_macro_in_fundecl_663b869ffc1f7213 (void)) (float arg1, C *arg2) { return &wam; } ",
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
                  nameHsIdent = HsIdentifier "C"}
                NameOriginInSource)]
          (TypePointer
            (TypeMacroTypedef
              NamePair {
                nameC = Name "C",
                nameHsIdent = HsIdentifier "C"}
              NameOriginInSource))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "wam",
          commentLocation = Just
            "macro_in_fundecl.h:13:4",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "foo1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCFloat,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "g"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimCInt))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "g",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCChar))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_beb332fafcb4f833",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "char *hs_bindgen_test_macro_in_fundecl_beb332fafcb4f833 (float arg1, signed int (*arg2) (signed int arg1)) { return foo1(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypePrim
                (PrimFloating PrimFloat)),
            _×_
              (Just
                NamePair {
                  nameC = Name "g",
                  nameHsIdent = HsIdentifier "g"})
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
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_4c474102032285f3",
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
      "hs_bindgen_test_macro_in_fundecl_4c474102032285f3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_foo1_ptr */ __attribute__ ((const)) char *(*hs_bindgen_test_macro_in_fundecl_4c474102032285f3 (void)) (float arg1, signed int (*arg2) (signed int arg1)) { return &foo1; } ",
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
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "foo1",
          commentLocation = Just
            "macro_in_fundecl.h:16:7",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "foo2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "F"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "g"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimCInt))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "g",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCChar))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_e1f27efd2405af7b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "char *hs_bindgen_test_macro_in_fundecl_e1f27efd2405af7b (F arg1, signed int (*arg2) (signed int arg1)) { return foo2(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "F",
                  nameHsIdent = HsIdentifier "F"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "g",
                  nameHsIdent = HsIdentifier "g"})
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
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_1d056e5b12d9c34a",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "F"))
              (HsFun
                (HsFunPtr
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsIO (HsPrimType HsPrimCInt))))
                (HsIO
                  (HsPtr
                    (HsPrimType HsPrimCChar))))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_1d056e5b12d9c34a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_foo2_ptr */ __attribute__ ((const)) char *(*hs_bindgen_test_macro_in_fundecl_1d056e5b12d9c34a (void)) (F arg1, signed int (*arg2) (signed int arg1)) { return &foo2; } ",
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "F",
                nameHsIdent = HsIdentifier "F"}
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
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "foo2",
          commentLocation = Just
            "macro_in_fundecl.h:17:7",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "foo3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCFloat,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "g"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimCInt))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "g",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "C")))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_4c3cb01285513c07",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "C *hs_bindgen_test_macro_in_fundecl_4c3cb01285513c07 (float arg1, signed int (*arg2) (signed int arg1)) { return foo3(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypePrim
                (PrimFloating PrimFloat)),
            _×_
              (Just
                NamePair {
                  nameC = Name "g",
                  nameHsIdent = HsIdentifier "g"})
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
                nameHsIdent = HsIdentifier "C"}
              NameOriginInSource)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "foo3",
          commentLocation = Just
            "macro_in_fundecl.h:18:4",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_7be3908af5c5e7b4",
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
                      (HsName
                        "@NsTypeConstr"
                        "C")))))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_7be3908af5c5e7b4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_foo3_ptr */ __attribute__ ((const)) C *(*hs_bindgen_test_macro_in_fundecl_7be3908af5c5e7b4 (void)) (float arg1, signed int (*arg2) (signed int arg1)) { return &foo3; } ",
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
                nameHsIdent = HsIdentifier "C"}
              NameOriginInSource))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "foo3",
          commentLocation = Just
            "macro_in_fundecl.h:18:4",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "bar1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCLong,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeader = Nothing,
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
      "hs_bindgen_test_macro_in_fundecl_105b4afb95e972a0",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int (*hs_bindgen_test_macro_in_fundecl_105b4afb95e972a0 (signed long arg1)) (signed short arg1) { return bar1(arg1); }",
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
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
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_a130943e7c1dc6d1",
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
      "hs_bindgen_test_macro_in_fundecl_a130943e7c1dc6d1",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_bar1_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_macro_in_fundecl_a130943e7c1dc6d1 (void)) (signed long arg1)) (signed short arg1) { return &bar1; } ",
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
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "bar1",
          commentLocation = Just
            "macro_in_fundecl.h:21:7",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "bar2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "L"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCShort)
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_111e58aa3ace1ef5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int (*hs_bindgen_test_macro_in_fundecl_111e58aa3ace1ef5 (L arg1)) (signed short arg1) { return bar2(arg1); }",
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "L",
                  nameHsIdent = HsIdentifier "L"}
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
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_06fc2db95e4cd938",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "L"))
              (HsIO
                (HsFunPtr
                  (HsFun
                    (HsPrimType HsPrimCShort)
                    (HsIO
                      (HsPrimType HsPrimCInt)))))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_06fc2db95e4cd938",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_bar2_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_macro_in_fundecl_06fc2db95e4cd938 (void)) (L arg1)) (signed short arg1) { return &bar2; } ",
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "L",
                nameHsIdent = HsIdentifier "L"}
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
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "bar2",
          commentLocation = Just
            "macro_in_fundecl.h:22:7",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "bar3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType =
          HsPrimType HsPrimCLong,
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "S"))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_ab5779d2bff0d08e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int (*hs_bindgen_test_macro_in_fundecl_ab5779d2bff0d08e (signed long arg1)) (S arg1) { return bar3(arg1); }",
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
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
                    nameHsIdent = HsIdentifier "S"}
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
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_d2fff4d202997609",
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
                      (HsName "@NsTypeConstr" "S"))
                    (HsIO
                      (HsPrimType HsPrimCInt)))))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_d2fff4d202997609",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_bar3_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_macro_in_fundecl_d2fff4d202997609 (void)) (signed long arg1)) (S arg1) { return &bar3; } ",
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
                    nameHsIdent = HsIdentifier "S"}
                  NameOriginInSource]
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed))))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "bar3",
          commentLocation = Just
            "macro_in_fundecl.h:23:7",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "bar4",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType =
          HsPrimType HsPrimCLong,
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCShort)
              (HsIO
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "I")))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_819bcb2cfe998801",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "I (*hs_bindgen_test_macro_in_fundecl_819bcb2cfe998801 (signed long arg1)) (signed short arg1) { return bar4(arg1); }",
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
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
                  nameHsIdent = HsIdentifier "I"}
                NameOriginInSource))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "bar4",
          commentLocation = Just
            "macro_in_fundecl.h:24:5",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_e53d6f29f5ea7fcd",
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
                        (HsName
                          "@NsTypeConstr"
                          "I"))))))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_e53d6f29f5ea7fcd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_bar4_ptr */ __attribute__ ((const)) I (*(*hs_bindgen_test_macro_in_fundecl_e53d6f29f5ea7fcd (void)) (signed long arg1)) (signed short arg1) { return &bar4; } ",
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
                  nameHsIdent = HsIdentifier "I"}
                NameOriginInSource)))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "bar4",
          commentLocation = Just
            "macro_in_fundecl.h:24:5",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "baz1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "i"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "i",
              commentLocation = Nothing,
              commentHeader = Nothing,
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
      "hs_bindgen_test_macro_in_fundecl_5ae188a985c17f69",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int (*hs_bindgen_test_macro_in_fundecl_5ae188a985c17f69 (signed int const arg1))[2][3] { return baz1(arg1); }",
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "i",
                  nameHsIdent = HsIdentifier "i"})
              (TypeConst
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
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_fe6e07215634608f",
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
      "hs_bindgen_test_macro_in_fundecl_fe6e07215634608f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_baz1_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_macro_in_fundecl_fe6e07215634608f (void)) (signed int const arg1))[2][3] { return &baz1; } ",
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeConst
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
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "baz1",
          commentLocation = Just
            "macro_in_fundecl.h:27:7",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "baz2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "i"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "i",
              commentLocation = Nothing,
              commentHeader = Nothing,
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
      "hs_bindgen_test_macro_in_fundecl_5b4ef76ef034352c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int (*hs_bindgen_test_macro_in_fundecl_5b4ef76ef034352c (signed int const arg1))[2][3] { return baz2(arg1); }",
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "i",
                  nameHsIdent = HsIdentifier "i"})
              (TypeConst
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
          commentOrigin = Just "baz2",
          commentLocation = Just
            "macro_in_fundecl.h:35:7",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_38938fbf1c30da29",
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
      "hs_bindgen_test_macro_in_fundecl_38938fbf1c30da29",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_baz2_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_macro_in_fundecl_38938fbf1c30da29 (void)) (signed int const arg1))[2][3] { return &baz2; } ",
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeConst
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
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "baz2",
          commentLocation = Just
            "macro_in_fundecl.h:35:7",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "baz3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "i"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "i",
              commentLocation = Nothing,
              commentHeader = Nothing,
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
      "hs_bindgen_test_macro_in_fundecl_f47e020b23c5aa4d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int (*hs_bindgen_test_macro_in_fundecl_f47e020b23c5aa4d (signed int const arg1))[2][3] { return baz3(arg1); }",
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "i",
                  nameHsIdent = HsIdentifier "i"})
              (TypeConst
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
          commentOrigin = Just "baz3",
          commentLocation = Just
            "macro_in_fundecl.h:43:5",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_dc9d10f056d20148",
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
      "hs_bindgen_test_macro_in_fundecl_dc9d10f056d20148",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_baz3_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_macro_in_fundecl_dc9d10f056d20148 (void)) (signed int const arg1))[2][3] { return &baz3; } ",
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeConst
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
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "baz3",
          commentLocation = Just
            "macro_in_fundecl.h:43:5",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "no_args_no_void",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName "@NsTypeConstr" "I"))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_9d7e58d4e189732b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "I hs_bindgen_test_macro_in_fundecl_9d7e58d4e189732b (void) { return no_args_no_void(); }",
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
              nameHsIdent = HsIdentifier "I"}
            NameOriginInSource},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "no_args_no_void",
          commentLocation = Just
            "macro_in_fundecl.h:53:3",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_1f43e6c47e963043",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "I"))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_1f43e6c47e963043",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_no_args_no_void_ptr */ __attribute__ ((const)) I (*hs_bindgen_test_macro_in_fundecl_1f43e6c47e963043 (void)) (void) { return &no_args_no_void; } ",
          capiWrapperImport =
          "macro_in_fundecl.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
          (TypeMacroTypedef
            NamePair {
              nameC = Name "I",
              nameHsIdent = HsIdentifier "I"}
            NameOriginInSource)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "no_args_no_void",
          commentLocation = Just
            "macro_in_fundecl.h:53:3",
          commentHeader = Just
            "macro_in_fundecl.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
