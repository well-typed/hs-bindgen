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
              "MC"}},
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
              "TC"}},
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
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = CName "MC",
                  nameHsIdent = HsIdentifier
                    "MC"}),
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = CName "TC",
                  nameHsIdent = HsIdentifier
                    "TC"})],
          functionRes = TypePrim
            (PrimChar
              (PrimSignImplicit Nothing)),
          functionHeader =
          "macro_in_fundecl_vs_typedef.h"}},
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
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = CName "MC",
                  nameHsIdent = HsIdentifier
                    "MC"}),
            TypePrim
              (PrimChar
                (PrimSignImplicit Nothing))],
          functionRes = TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = CName "TC",
                nameHsIdent = HsIdentifier
                  "TC"}),
          functionHeader =
          "macro_in_fundecl_vs_typedef.h"}},
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
            (TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = CName "MC",
                  nameHsIdent = HsIdentifier
                    "MC"})),
          functionHeader =
          "macro_in_fundecl_vs_typedef.h"}},
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
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = CName "MC",
                    nameHsIdent = HsIdentifier
                      "MC"}))],
          functionRes = TypePointer
            (TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = CName "TC",
                  nameHsIdent = HsIdentifier
                    "TC"})),
          functionHeader =
          "macro_in_fundecl_vs_typedef.h"}},
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
                "Struct1"}},
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
                  "Struct1"}},
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
                          "Struct1"}},
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
                          "Struct1"}},
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
                "Struct2"}},
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
                  "Struct2"}},
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
                          "Struct2"}},
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
                          "Struct2"}},
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
                "Struct3"}},
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
                  "Struct3"}},
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
                          "Struct3"}},
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
                          "Struct3"}},
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
              "Struct3_t"}},
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
                  "Struct3"}},
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
                "Struct4"}},
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
                  "Struct4"}},
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
                          "Struct4"}},
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
                          "Struct4"}},
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
                        "Struct2"}))),
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = CName "MC",
                  nameHsIdent = HsIdentifier
                    "MC"})],
          functionRes = TypeVoid,
          functionHeader =
          "macro_in_fundecl_vs_typedef.h"}},
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
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = CName "MC",
                  nameHsIdent = HsIdentifier
                    "MC"})],
          functionRes = TypeVoid,
          functionHeader =
          "macro_in_fundecl_vs_typedef.h"}},
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
                        "Struct4"}))),
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = CName "MC",
                  nameHsIdent = HsIdentifier
                    "MC"})],
          functionRes = TypeVoid,
          functionHeader =
          "macro_in_fundecl_vs_typedef.h"}},
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
                    "Struct1"}),
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = CName "MC",
                  nameHsIdent = HsIdentifier
                    "MC"})],
          functionRes = TypeVoid,
          functionHeader =
          "macro_in_fundecl_vs_typedef.h"}},
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
                    "Struct3"}),
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = CName "MC",
                  nameHsIdent = HsIdentifier
                    "MC"})],
          functionRes = TypeVoid,
          functionHeader =
          "macro_in_fundecl_vs_typedef.h"}},
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
                    "Struct4"}),
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = CName "MC",
                  nameHsIdent = HsIdentifier
                    "MC"})],
          functionRes = TypeVoid,
          functionHeader =
          "macro_in_fundecl_vs_typedef.h"}}]
