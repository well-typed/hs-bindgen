[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "MC",
      newtypeConstr = HsName
        "@NsConstr"
        "MC",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_MC",
        fieldType = HsPrimType
          HsPrimCChar,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "macro_in_fundecl_vs_typedef.h:4:9",
          declId = NamePair {
            nameC = CName "MC",
            nameHsIdent = HsIdentifier
              "MC"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "macro_in_fundecl_vs_typedef.h"},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "MC",
              newtypeField = HsName
                "@NsVar"
                "un_MC"},
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
          Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "TC",
      newtypeConstr = HsName
        "@NsConstr"
        "TC",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_TC",
        fieldType = HsPrimType
          HsPrimCChar,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "macro_in_fundecl_vs_typedef.h:5:14",
          declId = NamePair {
            nameC = CName "TC",
            nameHsIdent = HsIdentifier
              "TC"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "macro_in_fundecl_vs_typedef.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "TC",
              newtypeField = HsName
                "@NsVar"
                "un_TC"},
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
          Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "TC"),
  DeclInlineCInclude
    "macro_in_fundecl_vs_typedef.h",
  DeclInlineC
    "char testmodule_quux1 (MC arg1, TC arg2) { return quux1(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "quux1",
      foreignImportType = HsFun
        (HsTypRef
          (HsName "@NsTypeConstr" "MC"))
        (HsFun
          (HsTypRef
            (HsName "@NsTypeConstr" "TC"))
          (HsIO
            (HsPrimType HsPrimCChar))),
      foreignImportOrigName =
      "testmodule_quux1",
      foreignImportHeader =
      "macro_in_fundecl_vs_typedef.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypeMacroTypedef
              NamePair {
                nameC = CName "MC",
                nameHsIdent = HsIdentifier "MC"}
              NameOriginInSource,
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = CName "TC",
                  nameHsIdent = HsIdentifier
                    "TC"})],
          functionRes = TypePrim
            (PrimChar
              (PrimSignImplicit Nothing))}},
  DeclInlineCInclude
    "macro_in_fundecl_vs_typedef.h",
  DeclInlineC
    "TC testmodule_quux2 (MC arg1, char arg2) { return quux2(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "quux2",
      foreignImportType = HsFun
        (HsTypRef
          (HsName "@NsTypeConstr" "MC"))
        (HsFun
          (HsPrimType HsPrimCChar)
          (HsIO
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "TC")))),
      foreignImportOrigName =
      "testmodule_quux2",
      foreignImportHeader =
      "macro_in_fundecl_vs_typedef.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypeMacroTypedef
              NamePair {
                nameC = CName "MC",
                nameHsIdent = HsIdentifier "MC"}
              NameOriginInSource,
            TypePrim
              (PrimChar
                (PrimSignImplicit Nothing))],
          functionRes = TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = CName "TC",
                nameHsIdent = HsIdentifier
                  "TC"})}},
  DeclInlineCInclude
    "macro_in_fundecl_vs_typedef.h",
  DeclInlineC
    "MC *testmodule_wam1 (float arg1, TC *arg2) { return wam1(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "wam1",
      foreignImportType = HsFun
        (HsPrimType HsPrimCFloat)
        (HsFun
          (HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "TC")))
          (HsIO
            (HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "MC"))))),
      foreignImportOrigName =
      "testmodule_wam1",
      foreignImportHeader =
      "macro_in_fundecl_vs_typedef.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePrim
              (PrimFloating PrimFloat),
            TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = CName "TC",
                    nameHsIdent = HsIdentifier
                      "TC"}))],
          functionRes = TypePointer
            (TypeMacroTypedef
              NamePair {
                nameC = CName "MC",
                nameHsIdent = HsIdentifier "MC"}
              NameOriginInSource)}},
  DeclInlineCInclude
    "macro_in_fundecl_vs_typedef.h",
  DeclInlineC
    "TC *testmodule_wam2 (float arg1, MC *arg2) { return wam2(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "wam2",
      foreignImportType = HsFun
        (HsPrimType HsPrimCFloat)
        (HsFun
          (HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "MC")))
          (HsIO
            (HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "TC"))))),
      foreignImportOrigName =
      "testmodule_wam2",
      foreignImportHeader =
      "macro_in_fundecl_vs_typedef.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePrim
              (PrimFloating PrimFloat),
            TypePointer
              (TypeMacroTypedef
                NamePair {
                  nameC = CName "MC",
                  nameHsIdent = HsIdentifier "MC"}
                NameOriginInSource)],
          functionRes = TypePointer
            (TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = CName "TC",
                  nameHsIdent = HsIdentifier
                    "TC"}))}},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Struct1",
      structConstr = HsName
        "@NsConstr"
        "Struct1",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "struct1_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "macro_in_fundecl_vs_typedef.h:18:30",
              structFieldName = NamePair {
                nameC = CName "a",
                nameHsIdent = HsIdentifier
                  "struct1_a"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "macro_in_fundecl_vs_typedef.h:18:16",
            declId = NamePair {
              nameC = CName "struct1",
              nameHsIdent = HsIdentifier
                "Struct1"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "macro_in_fundecl_vs_typedef.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Struct1"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "macro_in_fundecl_vs_typedef.h:18:30",
                  structFieldName = NamePair {
                    nameC = CName "a",
                    nameHsIdent = HsIdentifier
                      "struct1_a"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
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
        [Eq, Show, Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Struct1",
        structConstr = HsName
          "@NsConstr"
          "Struct1",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "struct1_a",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "macro_in_fundecl_vs_typedef.h:18:30",
                structFieldName = NamePair {
                  nameC = CName "a",
                  nameHsIdent = HsIdentifier
                    "struct1_a"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "macro_in_fundecl_vs_typedef.h:18:16",
              declId = NamePair {
                nameC = CName "struct1",
                nameHsIdent = HsIdentifier
                  "Struct1"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader =
              "macro_in_fundecl_vs_typedef.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Struct1"),
                structSizeof = 4,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "macro_in_fundecl_vs_typedef.h:18:30",
                    structFieldName = NamePair {
                      nameC = CName "a",
                      nameHsIdent = HsIdentifier
                        "struct1_a"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
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
          [Eq, Show, Storable]}
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
                  "Struct1",
                structConstr = HsName
                  "@NsConstr"
                  "Struct1",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "struct1_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "macro_in_fundecl_vs_typedef.h:18:30",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "struct1_a"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "macro_in_fundecl_vs_typedef.h:18:16",
                      declId = NamePair {
                        nameC = CName "struct1",
                        nameHsIdent = HsIdentifier
                          "Struct1"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "macro_in_fundecl_vs_typedef.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Struct1"),
                        structSizeof = 4,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "macro_in_fundecl_vs_typedef.h:18:30",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "struct1_a"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
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
                  [Eq, Show, Storable]})
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
                  "Struct1",
                structConstr = HsName
                  "@NsConstr"
                  "Struct1",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "struct1_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "macro_in_fundecl_vs_typedef.h:18:30",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "struct1_a"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "macro_in_fundecl_vs_typedef.h:18:16",
                      declId = NamePair {
                        nameC = CName "struct1",
                        nameHsIdent = HsIdentifier
                          "Struct1"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "macro_in_fundecl_vs_typedef.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Struct1"),
                        structSizeof = 4,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "macro_in_fundecl_vs_typedef.h:18:30",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "struct1_a"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
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
                  [Eq, Show, Storable]}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct1"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct1"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Struct2",
      structConstr = HsName
        "@NsConstr"
        "Struct2",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "struct2_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "macro_in_fundecl_vs_typedef.h:19:30",
              structFieldName = NamePair {
                nameC = CName "a",
                nameHsIdent = HsIdentifier
                  "struct2_a"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "macro_in_fundecl_vs_typedef.h:19:9",
            declId = NamePair {
              nameC = CName "struct2",
              nameHsIdent = HsIdentifier
                "Struct2"},
            declOrigin = NameOriginGenerated
              (AnonId
                "macro_in_fundecl_vs_typedef.h:19:9"),
            declAliases = [CName "struct2"],
            declHeader =
            "macro_in_fundecl_vs_typedef.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Struct2"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "macro_in_fundecl_vs_typedef.h:19:30",
                  structFieldName = NamePair {
                    nameC = CName "a",
                    nameHsIdent = HsIdentifier
                      "struct2_a"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
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
        [Eq, Show, Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Struct2",
        structConstr = HsName
          "@NsConstr"
          "Struct2",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "struct2_a",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "macro_in_fundecl_vs_typedef.h:19:30",
                structFieldName = NamePair {
                  nameC = CName "a",
                  nameHsIdent = HsIdentifier
                    "struct2_a"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "macro_in_fundecl_vs_typedef.h:19:9",
              declId = NamePair {
                nameC = CName "struct2",
                nameHsIdent = HsIdentifier
                  "Struct2"},
              declOrigin = NameOriginGenerated
                (AnonId
                  "macro_in_fundecl_vs_typedef.h:19:9"),
              declAliases = [CName "struct2"],
              declHeader =
              "macro_in_fundecl_vs_typedef.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Struct2"),
                structSizeof = 4,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "macro_in_fundecl_vs_typedef.h:19:30",
                    structFieldName = NamePair {
                      nameC = CName "a",
                      nameHsIdent = HsIdentifier
                        "struct2_a"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
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
          [Eq, Show, Storable]}
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
                  "Struct2",
                structConstr = HsName
                  "@NsConstr"
                  "Struct2",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "struct2_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "macro_in_fundecl_vs_typedef.h:19:30",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "struct2_a"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "macro_in_fundecl_vs_typedef.h:19:9",
                      declId = NamePair {
                        nameC = CName "struct2",
                        nameHsIdent = HsIdentifier
                          "Struct2"},
                      declOrigin = NameOriginGenerated
                        (AnonId
                          "macro_in_fundecl_vs_typedef.h:19:9"),
                      declAliases = [CName "struct2"],
                      declHeader =
                      "macro_in_fundecl_vs_typedef.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Struct2"),
                        structSizeof = 4,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "macro_in_fundecl_vs_typedef.h:19:30",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "struct2_a"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
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
                  [Eq, Show, Storable]})
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
                  "Struct2",
                structConstr = HsName
                  "@NsConstr"
                  "Struct2",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "struct2_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "macro_in_fundecl_vs_typedef.h:19:30",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "struct2_a"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "macro_in_fundecl_vs_typedef.h:19:9",
                      declId = NamePair {
                        nameC = CName "struct2",
                        nameHsIdent = HsIdentifier
                          "Struct2"},
                      declOrigin = NameOriginGenerated
                        (AnonId
                          "macro_in_fundecl_vs_typedef.h:19:9"),
                      declAliases = [CName "struct2"],
                      declHeader =
                      "macro_in_fundecl_vs_typedef.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Struct2"),
                        structSizeof = 4,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "macro_in_fundecl_vs_typedef.h:19:30",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "struct2_a"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
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
                  [Eq, Show, Storable]}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct2"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct2"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Struct3",
      structConstr = HsName
        "@NsConstr"
        "Struct3",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "struct3_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "macro_in_fundecl_vs_typedef.h:20:30",
              structFieldName = NamePair {
                nameC = CName "a",
                nameHsIdent = HsIdentifier
                  "struct3_a"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "macro_in_fundecl_vs_typedef.h:20:16",
            declId = NamePair {
              nameC = CName "struct3",
              nameHsIdent = HsIdentifier
                "Struct3"},
            declOrigin = NameOriginInSource,
            declAliases = [
              CName "struct3_t"],
            declHeader =
            "macro_in_fundecl_vs_typedef.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Struct3"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "macro_in_fundecl_vs_typedef.h:20:30",
                  structFieldName = NamePair {
                    nameC = CName "a",
                    nameHsIdent = HsIdentifier
                      "struct3_a"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
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
        [Eq, Show, Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Struct3",
        structConstr = HsName
          "@NsConstr"
          "Struct3",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "struct3_a",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "macro_in_fundecl_vs_typedef.h:20:30",
                structFieldName = NamePair {
                  nameC = CName "a",
                  nameHsIdent = HsIdentifier
                    "struct3_a"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "macro_in_fundecl_vs_typedef.h:20:16",
              declId = NamePair {
                nameC = CName "struct3",
                nameHsIdent = HsIdentifier
                  "Struct3"},
              declOrigin = NameOriginInSource,
              declAliases = [
                CName "struct3_t"],
              declHeader =
              "macro_in_fundecl_vs_typedef.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Struct3"),
                structSizeof = 4,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "macro_in_fundecl_vs_typedef.h:20:30",
                    structFieldName = NamePair {
                      nameC = CName "a",
                      nameHsIdent = HsIdentifier
                        "struct3_a"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
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
          [Eq, Show, Storable]}
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
                  "Struct3",
                structConstr = HsName
                  "@NsConstr"
                  "Struct3",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "struct3_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "macro_in_fundecl_vs_typedef.h:20:30",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "struct3_a"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "macro_in_fundecl_vs_typedef.h:20:16",
                      declId = NamePair {
                        nameC = CName "struct3",
                        nameHsIdent = HsIdentifier
                          "Struct3"},
                      declOrigin = NameOriginInSource,
                      declAliases = [
                        CName "struct3_t"],
                      declHeader =
                      "macro_in_fundecl_vs_typedef.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Struct3"),
                        structSizeof = 4,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "macro_in_fundecl_vs_typedef.h:20:30",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "struct3_a"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
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
                  [Eq, Show, Storable]})
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
                  "Struct3",
                structConstr = HsName
                  "@NsConstr"
                  "Struct3",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "struct3_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "macro_in_fundecl_vs_typedef.h:20:30",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "struct3_a"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "macro_in_fundecl_vs_typedef.h:20:16",
                      declId = NamePair {
                        nameC = CName "struct3",
                        nameHsIdent = HsIdentifier
                          "Struct3"},
                      declOrigin = NameOriginInSource,
                      declAliases = [
                        CName "struct3_t"],
                      declHeader =
                      "macro_in_fundecl_vs_typedef.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Struct3"),
                        structSizeof = 4,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "macro_in_fundecl_vs_typedef.h:20:30",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "struct3_a"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
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
                  [Eq, Show, Storable]}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct3"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct3"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Struct3_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Struct3_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Struct3_t",
        fieldType = HsTypRef
          (HsName
            "@NsTypeConstr"
            "Struct3"),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "macro_in_fundecl_vs_typedef.h:20:35",
          declId = NamePair {
            nameC = CName "struct3_t",
            nameHsIdent = HsIdentifier
              "Struct3_t"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "macro_in_fundecl_vs_typedef.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Struct3_t",
              newtypeField = HsName
                "@NsVar"
                "un_Struct3_t"},
            typedefType = TypeStruct
              NamePair {
                nameC = CName "struct3",
                nameHsIdent = HsIdentifier
                  "Struct3"}
              NameOriginInSource},
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
    (HsName
      "@NsTypeConstr"
      "Struct3_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct3_t"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct3_t"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Struct4",
      structConstr = HsName
        "@NsConstr"
        "Struct4",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "struct4_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "macro_in_fundecl_vs_typedef.h:21:30",
              structFieldName = NamePair {
                nameC = CName "a",
                nameHsIdent = HsIdentifier
                  "struct4_a"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "macro_in_fundecl_vs_typedef.h:21:16",
            declId = NamePair {
              nameC = CName "struct4",
              nameHsIdent = HsIdentifier
                "Struct4"},
            declOrigin = NameOriginInSource,
            declAliases = [CName "struct4"],
            declHeader =
            "macro_in_fundecl_vs_typedef.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Struct4"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "macro_in_fundecl_vs_typedef.h:21:30",
                  structFieldName = NamePair {
                    nameC = CName "a",
                    nameHsIdent = HsIdentifier
                      "struct4_a"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
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
        [Eq, Show, Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Struct4",
        structConstr = HsName
          "@NsConstr"
          "Struct4",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "struct4_a",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "macro_in_fundecl_vs_typedef.h:21:30",
                structFieldName = NamePair {
                  nameC = CName "a",
                  nameHsIdent = HsIdentifier
                    "struct4_a"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "macro_in_fundecl_vs_typedef.h:21:16",
              declId = NamePair {
                nameC = CName "struct4",
                nameHsIdent = HsIdentifier
                  "Struct4"},
              declOrigin = NameOriginInSource,
              declAliases = [CName "struct4"],
              declHeader =
              "macro_in_fundecl_vs_typedef.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Struct4"),
                structSizeof = 4,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "macro_in_fundecl_vs_typedef.h:21:30",
                    structFieldName = NamePair {
                      nameC = CName "a",
                      nameHsIdent = HsIdentifier
                        "struct4_a"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
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
          [Eq, Show, Storable]}
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
                  "Struct4",
                structConstr = HsName
                  "@NsConstr"
                  "Struct4",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "struct4_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "macro_in_fundecl_vs_typedef.h:21:30",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "struct4_a"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "macro_in_fundecl_vs_typedef.h:21:16",
                      declId = NamePair {
                        nameC = CName "struct4",
                        nameHsIdent = HsIdentifier
                          "Struct4"},
                      declOrigin = NameOriginInSource,
                      declAliases = [CName "struct4"],
                      declHeader =
                      "macro_in_fundecl_vs_typedef.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Struct4"),
                        structSizeof = 4,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "macro_in_fundecl_vs_typedef.h:21:30",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "struct4_a"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
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
                  [Eq, Show, Storable]})
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
                  "Struct4",
                structConstr = HsName
                  "@NsConstr"
                  "Struct4",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "struct4_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "macro_in_fundecl_vs_typedef.h:21:30",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "struct4_a"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "macro_in_fundecl_vs_typedef.h:21:16",
                      declId = NamePair {
                        nameC = CName "struct4",
                        nameHsIdent = HsIdentifier
                          "Struct4"},
                      declOrigin = NameOriginInSource,
                      declAliases = [CName "struct4"],
                      declHeader =
                      "macro_in_fundecl_vs_typedef.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Struct4"),
                        structSizeof = 4,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "macro_in_fundecl_vs_typedef.h:21:30",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "struct4_a"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
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
                  [Eq, Show, Storable]}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct4"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct4"),
  DeclInlineCInclude
    "macro_in_fundecl_vs_typedef.h",
  DeclInlineC
    "void testmodule_struct_typedef1 (struct2 *arg1, MC arg2) { struct_typedef1(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "struct_typedef1",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct2")))
        (HsFun
          (HsTypRef
            (HsName "@NsTypeConstr" "MC"))
          (HsIO (HsPrimType HsPrimUnit))),
      foreignImportOrigName =
      "testmodule_struct_typedef1",
      foreignImportHeader =
      "macro_in_fundecl_vs_typedef.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePointer
              (TypeTypedef
                (TypedefSquashed
                  (CName "struct2")
                  (TypeStruct
                    NamePair {
                      nameC = CName "struct2",
                      nameHsIdent = HsIdentifier
                        "Struct2"}
                    (NameOriginGenerated
                      (AnonId
                        "macro_in_fundecl_vs_typedef.h:19:9"))))),
            TypeMacroTypedef
              NamePair {
                nameC = CName "MC",
                nameHsIdent = HsIdentifier "MC"}
              NameOriginInSource],
          functionRes = TypeVoid}},
  DeclInlineCInclude
    "macro_in_fundecl_vs_typedef.h",
  DeclInlineC
    "void testmodule_struct_typedef2 (struct3_t *arg1, MC arg2) { struct_typedef2(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "struct_typedef2",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct3_t")))
        (HsFun
          (HsTypRef
            (HsName "@NsTypeConstr" "MC"))
          (HsIO (HsPrimType HsPrimUnit))),
      foreignImportOrigName =
      "testmodule_struct_typedef2",
      foreignImportHeader =
      "macro_in_fundecl_vs_typedef.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = CName "struct3_t",
                    nameHsIdent = HsIdentifier
                      "Struct3_t"})),
            TypeMacroTypedef
              NamePair {
                nameC = CName "MC",
                nameHsIdent = HsIdentifier "MC"}
              NameOriginInSource],
          functionRes = TypeVoid}},
  DeclInlineCInclude
    "macro_in_fundecl_vs_typedef.h",
  DeclInlineC
    "void testmodule_struct_typedef3 (struct4 *arg1, MC arg2) { struct_typedef3(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "struct_typedef3",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct4")))
        (HsFun
          (HsTypRef
            (HsName "@NsTypeConstr" "MC"))
          (HsIO (HsPrimType HsPrimUnit))),
      foreignImportOrigName =
      "testmodule_struct_typedef3",
      foreignImportHeader =
      "macro_in_fundecl_vs_typedef.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePointer
              (TypeTypedef
                (TypedefSquashed
                  (CName "struct4")
                  (TypeStruct
                    NamePair {
                      nameC = CName "struct4",
                      nameHsIdent = HsIdentifier
                        "Struct4"}
                    NameOriginInSource))),
            TypeMacroTypedef
              NamePair {
                nameC = CName "MC",
                nameHsIdent = HsIdentifier "MC"}
              NameOriginInSource],
          functionRes = TypeVoid}},
  DeclInlineCInclude
    "macro_in_fundecl_vs_typedef.h",
  DeclInlineC
    "void testmodule_struct_name1 (struct struct1 *arg1, MC arg2) { struct_name1(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "struct_name1",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct1")))
        (HsFun
          (HsTypRef
            (HsName "@NsTypeConstr" "MC"))
          (HsIO (HsPrimType HsPrimUnit))),
      foreignImportOrigName =
      "testmodule_struct_name1",
      foreignImportHeader =
      "macro_in_fundecl_vs_typedef.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = CName "struct1",
                  nameHsIdent = HsIdentifier
                    "Struct1"}
                NameOriginInSource),
            TypeMacroTypedef
              NamePair {
                nameC = CName "MC",
                nameHsIdent = HsIdentifier "MC"}
              NameOriginInSource],
          functionRes = TypeVoid}},
  DeclInlineCInclude
    "macro_in_fundecl_vs_typedef.h",
  DeclInlineC
    "void testmodule_struct_name2 (struct struct3 *arg1, MC arg2) { struct_name2(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "struct_name2",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct3")))
        (HsFun
          (HsTypRef
            (HsName "@NsTypeConstr" "MC"))
          (HsIO (HsPrimType HsPrimUnit))),
      foreignImportOrigName =
      "testmodule_struct_name2",
      foreignImportHeader =
      "macro_in_fundecl_vs_typedef.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = CName "struct3",
                  nameHsIdent = HsIdentifier
                    "Struct3"}
                NameOriginInSource),
            TypeMacroTypedef
              NamePair {
                nameC = CName "MC",
                nameHsIdent = HsIdentifier "MC"}
              NameOriginInSource],
          functionRes = TypeVoid}},
  DeclInlineCInclude
    "macro_in_fundecl_vs_typedef.h",
  DeclInlineC
    "void testmodule_struct_name3 (struct struct4 *arg1, MC arg2) { struct_name3(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "struct_name3",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct4")))
        (HsFun
          (HsTypRef
            (HsName "@NsTypeConstr" "MC"))
          (HsIO (HsPrimType HsPrimUnit))),
      foreignImportOrigName =
      "testmodule_struct_name3",
      foreignImportHeader =
      "macro_in_fundecl_vs_typedef.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = CName "struct4",
                  nameHsIdent = HsIdentifier
                    "Struct4"}
                NameOriginInSource),
            TypeMacroTypedef
              NamePair {
                nameC = CName "MC",
                nameHsIdent = HsIdentifier "MC"}
              NameOriginInSource],
          functionRes = TypeVoid}}]
