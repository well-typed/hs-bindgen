[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "PtrInt",
      newtypeConstr = HsName
        "@NsConstr"
        "PtrInt",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_PtrInt",
        fieldType = HsPtr
          (HsPrimType HsPrimCInt),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "macro_types.h:2:9",
          declId = NamePair {
            nameC = Name "PtrInt",
            nameHsIdent = HsIdentifier
              "PtrInt"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "macro_types.h"},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "PtrInt",
              newtypeField = HsName
                "@NsVar"
                "un_PtrInt"},
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
        [Eq, Ord, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "PtrInt"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "PtrInt"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "PtrInt"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "PtrInt"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "PtrPtrChar",
      newtypeConstr = HsName
        "@NsConstr"
        "PtrPtrChar",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_PtrPtrChar",
        fieldType = HsPtr
          (HsPtr
            (HsPrimType HsPrimCChar)),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "macro_types.h:5:9",
          declId = NamePair {
            nameC = Name "PtrPtrChar",
            nameHsIdent = HsIdentifier
              "PtrPtrChar"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "macro_types.h"},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "PtrPtrChar",
              newtypeField = HsName
                "@NsVar"
                "un_PtrPtrChar"},
            macroType = TypePointer
              (TypePointer
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit Nothing))))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "PtrPtrChar"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "PtrPtrChar"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "PtrPtrChar"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "PtrPtrChar"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Arr1",
      newtypeConstr = HsName
        "@NsConstr"
        "Arr1",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Arr1",
        fieldType = HsConstArray
          2
          (HsPrimType HsPrimCInt),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "macro_types.h:8:9",
          declId = NamePair {
            nameC = Name "Arr1",
            nameHsIdent = HsIdentifier
              "Arr1"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "macro_types.h"},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Arr1",
              newtypeField = HsName
                "@NsVar"
                "un_Arr1"},
            macroType = TypeConstArray
              2
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "Arr1"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Arr1"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Arr1"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Arr2",
      newtypeConstr = HsName
        "@NsConstr"
        "Arr2",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Arr2",
        fieldType = HsConstArray
          3
          (HsPtr
            (HsPrimType HsPrimCFloat)),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "macro_types.h:11:9",
          declId = NamePair {
            nameC = Name "Arr2",
            nameHsIdent = HsIdentifier
              "Arr2"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "macro_types.h"},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Arr2",
              newtypeField = HsName
                "@NsVar"
                "un_Arr2"},
            macroType = TypeConstArray
              3
              (TypePointer
                (TypePrim
                  (PrimFloating PrimFloat)))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "Arr2"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Arr2"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Arr2"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Arr3",
      newtypeConstr = HsName
        "@NsConstr"
        "Arr3",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Arr3",
        fieldType = HsConstArray
          4
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCDouble)
              (HsIO
                (HsPrimType HsPrimCFloat)))),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "macro_types.h:14:9",
          declId = NamePair {
            nameC = Name "Arr3",
            nameHsIdent = HsIdentifier
              "Arr3"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "macro_types.h"},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Arr3",
              newtypeField = HsName
                "@NsVar"
                "un_Arr3"},
            macroType = TypeConstArray
              4
              (TypePointer
                (TypeFun
                  [
                    TypePrim
                      (PrimFloating PrimDouble)]
                  (TypePrim
                    (PrimFloating PrimFloat))))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "Arr3"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Arr3"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Arr3"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Fun1",
      newtypeConstr = HsName
        "@NsConstr"
        "Fun1",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Fun1",
        fieldType = HsFun
          (HsPrimType HsPrimCInt)
          (HsIO
            (HsPtr
              (HsPrimType HsPrimCFloat))),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "macro_types.h:17:9",
          declId = NamePair {
            nameC = Name "Fun1",
            nameHsIdent = HsIdentifier
              "Fun1"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "macro_types.h"},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Fun1",
              newtypeField = HsName
                "@NsVar"
                "un_Fun1"},
            macroType = TypeFun
              [
                TypePrim
                  (PrimIntegral PrimInt Signed)]
              (TypePointer
                (TypePrim
                  (PrimFloating PrimFloat)))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        []},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Fun2",
      newtypeConstr = HsName
        "@NsConstr"
        "Fun2",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Fun2",
        fieldType = HsFunPtr
          (HsFun
            (HsPrimType HsPrimCFloat)
            (HsFun
              (HsPtr
                (HsPrimType HsPrimCDouble))
              (HsIO
                (HsPrimType HsPrimCInt)))),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "macro_types.h:20:9",
          declId = NamePair {
            nameC = Name "Fun2",
            nameHsIdent = HsIdentifier
              "Fun2"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "macro_types.h"},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Fun2",
              newtypeField = HsName
                "@NsVar"
                "un_Fun2"},
            macroType = TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimFloating PrimFloat),
                  TypePointer
                    (TypePrim
                      (PrimFloating PrimDouble))]
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "Fun2"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Fun2"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "Fun2"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Fun2"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Fun3",
      newtypeConstr = HsName
        "@NsConstr"
        "Fun3",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Fun3",
        fieldType = HsFunPtr
          (HsFun
            (HsPtr
              (HsPrimType HsPrimCFloat))
            (HsIO
              (HsPtr
                (HsPrimType HsPrimCInt)))),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "macro_types.h:23:9",
          declId = NamePair {
            nameC = Name "Fun3",
            nameHsIdent = HsIdentifier
              "Fun3"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "macro_types.h"},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Fun3",
              newtypeField = HsName
                "@NsVar"
                "un_Fun3"},
            macroType = TypePointer
              (TypeFun
                [
                  TypePointer
                    (TypePrim
                      (PrimFloating PrimFloat))]
                (TypePointer
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "Fun3"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Fun3"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "Fun3"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Fun3"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Fun4",
      newtypeConstr = HsName
        "@NsConstr"
        "Fun4",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Fun4",
        fieldType = HsFun
          (HsPrimType HsPrimCInt)
          (HsFun
            (HsPtr (HsPrimType HsPrimCLong))
            (HsIO
              (HsFunPtr
                (HsFun
                  (HsPrimType HsPrimCFloat)
                  (HsFun
                    (HsPtr
                      (HsPrimType HsPrimCDouble))
                    (HsIO
                      (HsPtr
                        (HsPrimType HsPrimCLong)))))))),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "macro_types.h:26:9",
          declId = NamePair {
            nameC = Name "Fun4",
            nameHsIdent = HsIdentifier
              "Fun4"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "macro_types.h"},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Fun4",
              newtypeField = HsName
                "@NsVar"
                "un_Fun4"},
            macroType = TypeFun
              [
                TypePrim
                  (PrimIntegral PrimInt Signed),
                TypePointer
                  (TypePrim
                    (PrimIntegral PrimLong Signed))]
              (TypePointer
                (TypeFun
                  [
                    TypePrim
                      (PrimFloating PrimFloat),
                    TypePointer
                      (TypePrim
                        (PrimFloating PrimDouble))]
                  (TypePointer
                    (TypePrim
                      (PrimIntegral
                        PrimLong
                        Signed)))))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        []},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Fun5",
      newtypeConstr = HsName
        "@NsConstr"
        "Fun5",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Fun5",
        fieldType = HsFun
          (HsConstArray
            8
            (HsPrimType HsPrimCChar))
          (HsIO
            (HsPtr
              (HsConstArray
                2
                (HsPtr
                  (HsPrimType HsPrimCShort))))),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "macro_types.h:29:9",
          declId = NamePair {
            nameC = Name "Fun5",
            nameHsIdent = HsIdentifier
              "Fun5"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "macro_types.h"},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Fun5",
              newtypeField = HsName
                "@NsVar"
                "un_Fun5"},
            macroType = TypeFun
              [
                TypeConstArray
                  8
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit Nothing)))]
              (TypePointer
                (TypeConstArray
                  2
                  (TypePointer
                    (TypePrim
                      (PrimIntegral
                        PrimShort
                        Signed)))))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        []},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "MTy",
      newtypeConstr = HsName
        "@NsConstr"
        "MTy",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_MTy",
        fieldType = HsPrimType
          HsPrimCFloat,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "macro_types.h:33:9",
          declId = NamePair {
            nameC = Name "MTy",
            nameHsIdent = HsIdentifier
              "MTy"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "macro_types.h"},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "MTy",
              newtypeField = HsName
                "@NsVar"
                "un_MTy"},
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
          Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtypeInstance
    DeriveNewtype
    Floating
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtypeInstance
    DeriveNewtype
    Fractional
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtypeInstance
    DeriveNewtype
    RealFloat
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtypeInstance
    DeriveNewtype
    RealFrac
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Tty",
      newtypeConstr = HsName
        "@NsConstr"
        "Tty",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Tty",
        fieldType = HsTypRef
          (HsName "@NsTypeConstr" "MTy"),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "macro_types.h:34:13",
          declId = NamePair {
            nameC = Name "tty",
            nameHsIdent = HsIdentifier
              "Tty"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "macro_types.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Tty",
              newtypeField = HsName
                "@NsVar"
                "un_Tty"},
            typedefType = TypeMacroTypedef
              NamePair {
                nameC = Name "MTy",
                nameHsIdent = HsIdentifier
                  "MTy"}
              NameOriginInSource},
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
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtypeInstance
    DeriveNewtype
    Floating
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtypeInstance
    DeriveNewtype
    Fractional
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtypeInstance
    DeriveNewtype
    RealFloat
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtypeInstance
    DeriveNewtype
    RealFrac
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "UINT8_T",
      newtypeConstr = HsName
        "@NsConstr"
        "UINT8_T",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_UINT8_T",
        fieldType = HsPrimType
          HsPrimCUChar,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "macro_types.h:36:9",
          declId = NamePair {
            nameC = Name "UINT8_T",
            nameHsIdent = HsIdentifier
              "UINT8_T"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "macro_types.h"},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "UINT8_T",
              newtypeField = HsName
                "@NsVar"
                "un_UINT8_T"},
            macroType = TypePrim
              (PrimChar
                (PrimSignExplicit Unsigned))},
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
          Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "BOOLEAN_T",
      newtypeConstr = HsName
        "@NsConstr"
        "BOOLEAN_T",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_BOOLEAN_T",
        fieldType = HsTypRef
          (HsName
            "@NsTypeConstr"
            "UINT8_T"),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "macro_types.h:37:9",
          declId = NamePair {
            nameC = Name "BOOLEAN_T",
            nameHsIdent = HsIdentifier
              "BOOLEAN_T"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "macro_types.h"},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "BOOLEAN_T",
              newtypeField = HsName
                "@NsVar"
                "un_BOOLEAN_T"},
            macroType = TypeMacroTypedef
              NamePair {
                nameC = Name "UINT8_T",
                nameHsIdent = HsIdentifier
                  "UINT8_T"}
              NameOriginInSource},
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
          Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Boolean_T",
      newtypeConstr = HsName
        "@NsConstr"
        "Boolean_T",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Boolean_T",
        fieldType = HsTypRef
          (HsName
            "@NsTypeConstr"
            "BOOLEAN_T"),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "macro_types.h:38:19",
          declId = NamePair {
            nameC = Name "boolean_T",
            nameHsIdent = HsIdentifier
              "Boolean_T"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "macro_types.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Boolean_T",
              newtypeField = HsName
                "@NsVar"
                "un_Boolean_T"},
            typedefType = TypeMacroTypedef
              NamePair {
                nameC = Name "BOOLEAN_T",
                nameHsIdent = HsIdentifier
                  "BOOLEAN_T"}
              NameOriginInSource},
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
          Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "Boolean_T")]
