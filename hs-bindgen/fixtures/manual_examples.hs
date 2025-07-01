[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Triple",
      structConstr = HsName
        "@NsConstr"
        "Triple",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "triple_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "manual_examples.h:15:9",
              structFieldName = NamePair {
                nameC = CName "a",
                nameHsIdent = HsIdentifier
                  "triple_a"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "triple_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "manual_examples.h:16:9",
              structFieldName = NamePair {
                nameC = CName "b",
                nameHsIdent = HsIdentifier
                  "triple_b"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "triple_c",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "manual_examples.h:17:9",
              structFieldName = NamePair {
                nameC = CName "c",
                nameHsIdent = HsIdentifier
                  "triple_c"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "manual_examples.h:14:16",
            declId = NamePair {
              nameC = CName "triple",
              nameHsIdent = HsIdentifier
                "Triple"},
            declOrigin = NameOriginInSource,
            declAliases = [CName "triple"],
            declHeader =
            "manual_examples.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Triple"),
              structSizeof = 12,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "manual_examples.h:15:9",
                  structFieldName = NamePair {
                    nameC = CName "a",
                    nameHsIdent = HsIdentifier
                      "triple_a"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "manual_examples.h:16:9",
                  structFieldName = NamePair {
                    nameC = CName "b",
                    nameHsIdent = HsIdentifier
                      "triple_b"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "manual_examples.h:17:9",
                  structFieldName = NamePair {
                    nameC = CName "c",
                    nameHsIdent = HsIdentifier
                      "triple_c"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 64,
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
          "Triple",
        structConstr = HsName
          "@NsConstr"
          "Triple",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "triple_a",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "manual_examples.h:15:9",
                structFieldName = NamePair {
                  nameC = CName "a",
                  nameHsIdent = HsIdentifier
                    "triple_a"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "triple_b",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "manual_examples.h:16:9",
                structFieldName = NamePair {
                  nameC = CName "b",
                  nameHsIdent = HsIdentifier
                    "triple_b"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 32,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "triple_c",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "manual_examples.h:17:9",
                structFieldName = NamePair {
                  nameC = CName "c",
                  nameHsIdent = HsIdentifier
                    "triple_c"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "manual_examples.h:14:16",
              declId = NamePair {
                nameC = CName "triple",
                nameHsIdent = HsIdentifier
                  "Triple"},
              declOrigin = NameOriginInSource,
              declAliases = [CName "triple"],
              declHeader =
              "manual_examples.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Triple"),
                structSizeof = 12,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "manual_examples.h:15:9",
                    structFieldName = NamePair {
                      nameC = CName "a",
                      nameHsIdent = HsIdentifier
                        "triple_a"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "manual_examples.h:16:9",
                    structFieldName = NamePair {
                      nameC = CName "b",
                      nameHsIdent = HsIdentifier
                        "triple_b"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 32,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "manual_examples.h:17:9",
                    structFieldName = NamePair {
                      nameC = CName "c",
                      nameHsIdent = HsIdentifier
                        "triple_c"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 64,
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
        storableSizeOf = 12,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Triple",
                structConstr = HsName
                  "@NsConstr"
                  "Triple",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "triple_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:15:9",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "triple_a"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "triple_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:16:9",
                        structFieldName = NamePair {
                          nameC = CName "b",
                          nameHsIdent = HsIdentifier
                            "triple_b"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "triple_c",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:17:9",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "triple_c"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "manual_examples.h:14:16",
                      declId = NamePair {
                        nameC = CName "triple",
                        nameHsIdent = HsIdentifier
                          "Triple"},
                      declOrigin = NameOriginInSource,
                      declAliases = [CName "triple"],
                      declHeader =
                      "manual_examples.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Triple"),
                        structSizeof = 12,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:15:9",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "triple_a"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:16:9",
                            structFieldName = NamePair {
                              nameC = CName "b",
                              nameHsIdent = HsIdentifier
                                "triple_b"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:17:9",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "triple_c"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 64,
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
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 4,
              PeekByteOff (Idx 0) 8]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Triple",
                structConstr = HsName
                  "@NsConstr"
                  "Triple",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "triple_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:15:9",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "triple_a"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "triple_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:16:9",
                        structFieldName = NamePair {
                          nameC = CName "b",
                          nameHsIdent = HsIdentifier
                            "triple_b"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "triple_c",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:17:9",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "triple_c"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "manual_examples.h:14:16",
                      declId = NamePair {
                        nameC = CName "triple",
                        nameHsIdent = HsIdentifier
                          "Triple"},
                      declOrigin = NameOriginInSource,
                      declAliases = [CName "triple"],
                      declHeader =
                      "manual_examples.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Triple"),
                        structSizeof = 12,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:15:9",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "triple_a"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:16:9",
                            structFieldName = NamePair {
                              nameC = CName "b",
                              nameHsIdent = HsIdentifier
                                "triple_b"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:17:9",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "triple_c"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 64,
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
              (Add 3)
              (Seq
                [
                  PokeByteOff (Idx 4) 0 (Idx 0),
                  PokeByteOff (Idx 4) 4 (Idx 1),
                  PokeByteOff
                    (Idx 4)
                    8
                    (Idx 2)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Triple"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Triple"),
  DeclInlineCInclude
    "manual_examples.h",
  DeclInlineC
    "void testmodule_mk_triple (signed int arg1, signed int arg2, signed int arg3, triple *arg4) { mk_triple(arg1, arg2, arg3, arg4); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "mk_triple",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsFun
          (HsPrimType HsPrimCInt)
          (HsFun
            (HsPrimType HsPrimCInt)
            (HsFun
              (HsPtr
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Triple")))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "testmodule_mk_triple",
      foreignImportHeader =
      "manual_examples.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypePointer
              (TypeTypedef
                (TypedefSquashed
                  (CName "triple")
                  (TypeStruct
                    NamePair {
                      nameC = CName "triple",
                      nameHsIdent = HsIdentifier
                        "Triple"}
                    NameOriginInSource)))],
          functionRes = TypeVoid}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Index",
      newtypeConstr = HsName
        "@NsConstr"
        "Index",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Index",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "manual_examples.h:26:14",
          declId = NamePair {
            nameC = CName "index",
            nameHsIdent = HsIdentifier
              "Index"},
          declOrigin = NameOriginInSource,
          declAliases = [CName "index"],
          declHeader =
          "manual_examples.h"},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Index",
              newtypeField = HsName
                "@NsVar"
                "un_Index"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:27:5",
                enumConstantName = NamePair {
                  nameC = CName "A",
                  nameHsIdent = HsIdentifier "A"},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:28:5",
                enumConstantName = NamePair {
                  nameC = CName "B",
                  nameHsIdent = HsIdentifier "B"},
                enumConstantValue = 1},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:29:5",
                enumConstantName = NamePair {
                  nameC = CName "C",
                  nameHsIdent = HsIdentifier "C"},
                enumConstantValue = 2}]},
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
          Read,
          Show,
          Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Index",
        structConstr = HsName
          "@NsConstr"
          "Index",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Index",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
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
                  "Index",
                structConstr = HsName
                  "@NsConstr"
                  "Index",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Index",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
                structInstances = Set.fromList
                  [
                    Eq,
                    Ord,
                    Read,
                    Show,
                    Storable]})
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
                  "Index",
                structConstr = HsName
                  "@NsConstr"
                  "Index",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Index",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
                structInstances = Set.fromList
                  [Eq, Ord, Read, Show, Storable]}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Index"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Index"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Index",
        structConstr = HsName
          "@NsConstr"
          "Index",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Index",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_ 0 (NE.fromList ["A"]),
          _×_ 1 (NE.fromList ["B"]),
          _×_ 2 (NE.fromList ["C"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Index",
        structConstr = HsName
          "@NsConstr"
          "Index",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Index",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsName "@NsConstr" "A")
      (HsName "@NsConstr" "C")),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Index",
        structConstr = HsName
          "@NsConstr"
          "Index",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Index",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclInstance
    (InstanceCEnumRead
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Index",
        structConstr = HsName
          "@NsConstr"
          "Index",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Index",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "A",
      patSynType = HsName
        "@NsTypeConstr"
        "Index",
      patSynConstr = HsName
        "@NsConstr"
        "Index",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:27:5",
          enumConstantName = NamePair {
            nameC = CName "A",
            nameHsIdent = HsIdentifier "A"},
          enumConstantValue = 0}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "B",
      patSynType = HsName
        "@NsTypeConstr"
        "Index",
      patSynConstr = HsName
        "@NsConstr"
        "Index",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:28:5",
          enumConstantName = NamePair {
            nameC = CName "B",
            nameHsIdent = HsIdentifier "B"},
          enumConstantValue = 1}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "C",
      patSynType = HsName
        "@NsTypeConstr"
        "Index",
      patSynConstr = HsName
        "@NsConstr"
        "Index",
      patSynValue = 2,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:29:5",
          enumConstantName = NamePair {
            nameC = CName "C",
            nameHsIdent = HsIdentifier "C"},
          enumConstantValue = 2}},
  DeclInlineCInclude
    "manual_examples.h",
  DeclInlineC
    "signed int testmodule_index_triple (triple *arg1, index arg2) { return index_triple(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "index_triple",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Triple")))
        (HsFun
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Index"))
          (HsIO (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "testmodule_index_triple",
      foreignImportHeader =
      "manual_examples.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePointer
              (TypeTypedef
                (TypedefSquashed
                  (CName "triple")
                  (TypeStruct
                    NamePair {
                      nameC = CName "triple",
                      nameHsIdent = HsIdentifier
                        "Triple"}
                    NameOriginInSource))),
            TypeTypedef
              (TypedefSquashed
                (CName "index")
                (TypeEnum
                  NamePair {
                    nameC = CName "index",
                    nameHsIdent = HsIdentifier
                      "Index"}
                  NameOriginInSource))],
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Sum",
      newtypeConstr = HsName
        "@NsConstr"
        "Sum",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Sum",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "manual_examples.h:38:13",
          declId = NamePair {
            nameC = CName "sum",
            nameHsIdent = HsIdentifier
              "Sum"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "manual_examples.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Sum",
              newtypeField = HsName
                "@NsVar"
                "un_Sum"},
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
          Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Average",
      newtypeConstr = HsName
        "@NsConstr"
        "Average",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Average",
        fieldType = HsPrimType
          HsPrimCDouble,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "manual_examples.h:39:16",
          declId = NamePair {
            nameC = CName "average",
            nameHsIdent = HsIdentifier
              "Average"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "manual_examples.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Average",
              newtypeField = HsName
                "@NsVar"
                "un_Average"},
            typedefType = TypePrim
              (PrimFloating PrimDouble)},
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
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclNewtypeInstance
    DeriveNewtype
    Floating
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclNewtypeInstance
    DeriveNewtype
    Fractional
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclNewtypeInstance
    DeriveNewtype
    RealFloat
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclNewtypeInstance
    DeriveNewtype
    RealFrac
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclInlineCInclude
    "manual_examples.h",
  DeclInlineC
    "sum testmodule_sum_triple (triple *arg1) { return sum_triple(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "sum_triple",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Triple")))
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Sum"))),
      foreignImportOrigName =
      "testmodule_sum_triple",
      foreignImportHeader =
      "manual_examples.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePointer
              (TypeTypedef
                (TypedefSquashed
                  (CName "triple")
                  (TypeStruct
                    NamePair {
                      nameC = CName "triple",
                      nameHsIdent = HsIdentifier
                        "Triple"}
                    NameOriginInSource)))],
          functionRes = TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = CName "sum",
                nameHsIdent = HsIdentifier
                  "Sum"})}},
  DeclInlineCInclude
    "manual_examples.h",
  DeclInlineC
    "average testmodule_average_triple (triple *arg1) { return average_triple(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "average_triple",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Triple")))
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Average"))),
      foreignImportOrigName =
      "testmodule_average_triple",
      foreignImportHeader =
      "manual_examples.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePointer
              (TypeTypedef
                (TypedefSquashed
                  (CName "triple")
                  (TypeStruct
                    NamePair {
                      nameC = CName "triple",
                      nameHsIdent = HsIdentifier
                        "Triple"}
                    NameOriginInSource)))],
          functionRes = TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = CName "average",
                nameHsIdent = HsIdentifier
                  "Average"})}},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "fIELD_OFFSET",
      varDeclType = ForallTy {
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = TyConAppTy
            (ATyCon
              (GenerativeTyCon
                (DataTyCon IntLikeTyCon)))
            [
              TyConAppTy
                (ATyCon
                  (GenerativeTyCon
                    (DataTyCon
                      (IntLikeTyCon
                        (CIntegralType
                          (IntLike (Int Signed)))))))
                []]}},
      varDeclBody = VarDeclIntegral
        4
        HsPrimCInt},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "ePSILON",
      varDeclType = ForallTy {
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = TyConAppTy
            (ATyCon
              (GenerativeTyCon
                (DataTyCon FloatLikeTyCon)))
            [
              TyConAppTy
                (ATyCon
                  (GenerativeTyCon
                    (DataTyCon
                      (FloatLikeTyCon DoubleType))))
                []]}},
      varDeclBody = VarDeclDouble
        0.1},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "pTR_TO_FIELD",
      varDeclType = ForallTy {
        forallTyBinders = [
          NameHint "a"],
        forallTy = QuantTy {
          quantTyCts = [
            ClassTy
              (AClass
                (GenerativeTyCon
                  (ClassTyCon AddTyCon)))
              [
                TyVarTy (Idx 0),
                TyConAppTy
                  (ATyCon
                    (GenerativeTyCon
                      (DataTyCon IntLikeTyCon)))
                  [
                    TyConAppTy
                      (ATyCon
                        (GenerativeTyCon
                          (DataTyCon
                            (IntLikeTyCon
                              (CIntegralType
                                (IntLike (Int Signed)))))))
                      []]]],
          quantTyBody = FunTy
            (TyVarTy (Idx 0))
            (TyConAppTy
              (ATyCon
                (FamilyTyCon AddResTyCon))
              [
                TyVarTy (Idx 0),
                TyConAppTy
                  (ATyCon
                    (GenerativeTyCon
                      (DataTyCon IntLikeTyCon)))
                  [
                    TyConAppTy
                      (ATyCon
                        (GenerativeTyCon
                          (DataTyCon
                            (IntLikeTyCon
                              (CIntegralType
                                (IntLike (Int Signed)))))))
                      []]])}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "ptr")
          (VarDeclApp
            (InfixAppHead MAdd)
            [
              VarDeclVar (Idx 0),
              VarDeclIntegral
                4
                HsPrimCInt]))},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "YEAR",
      newtypeConstr = HsName
        "@NsConstr"
        "YEAR",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_YEAR",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "manual_examples.h:53:9",
          declId = NamePair {
            nameC = CName "YEAR",
            nameHsIdent = HsIdentifier
              "YEAR"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "manual_examples.h"},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "YEAR",
              newtypeField = HsName
                "@NsVar"
                "un_YEAR"},
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
          Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "MONTH",
      newtypeConstr = HsName
        "@NsConstr"
        "MONTH",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_MONTH",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "manual_examples.h:54:9",
          declId = NamePair {
            nameC = CName "MONTH",
            nameHsIdent = HsIdentifier
              "MONTH"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "manual_examples.h"},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "MONTH",
              newtypeField = HsName
                "@NsVar"
                "un_MONTH"},
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
          Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "DAY",
      newtypeConstr = HsName
        "@NsConstr"
        "DAY",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_DAY",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "manual_examples.h:55:9",
          declId = NamePair {
            nameC = CName "DAY",
            nameHsIdent = HsIdentifier
              "DAY"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "manual_examples.h"},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "DAY",
              newtypeField = HsName
                "@NsVar"
                "un_DAY"},
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
          Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "DAY"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Date",
      structConstr = HsName
        "@NsConstr"
        "Date",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "date_year",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "YEAR"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "manual_examples.h:58:11",
              structFieldName = NamePair {
                nameC = CName "year",
                nameHsIdent = HsIdentifier
                  "date_year"},
              structFieldType =
              TypeMacroTypedef
                NamePair {
                  nameC = CName "YEAR",
                  nameHsIdent = HsIdentifier
                    "YEAR"}
                NameOriginInSource,
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "date_month",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "MONTH"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "manual_examples.h:59:11",
              structFieldName = NamePair {
                nameC = CName "month",
                nameHsIdent = HsIdentifier
                  "date_month"},
              structFieldType =
              TypeMacroTypedef
                NamePair {
                  nameC = CName "MONTH",
                  nameHsIdent = HsIdentifier
                    "MONTH"}
                NameOriginInSource,
              structFieldOffset = 32,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "date_day",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "DAY"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "manual_examples.h:60:11",
              structFieldName = NamePair {
                nameC = CName "day",
                nameHsIdent = HsIdentifier
                  "date_day"},
              structFieldType =
              TypeMacroTypedef
                NamePair {
                  nameC = CName "DAY",
                  nameHsIdent = HsIdentifier
                    "DAY"}
                NameOriginInSource,
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "manual_examples.h:57:16",
            declId = NamePair {
              nameC = CName "date",
              nameHsIdent = HsIdentifier
                "Date"},
            declOrigin = NameOriginInSource,
            declAliases = [CName "date"],
            declHeader =
            "manual_examples.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Date"),
              structSizeof = 12,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "manual_examples.h:58:11",
                  structFieldName = NamePair {
                    nameC = CName "year",
                    nameHsIdent = HsIdentifier
                      "date_year"},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = CName "YEAR",
                      nameHsIdent = HsIdentifier
                        "YEAR"}
                    NameOriginInSource,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "manual_examples.h:59:11",
                  structFieldName = NamePair {
                    nameC = CName "month",
                    nameHsIdent = HsIdentifier
                      "date_month"},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = CName "MONTH",
                      nameHsIdent = HsIdentifier
                        "MONTH"}
                    NameOriginInSource,
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "manual_examples.h:60:11",
                  structFieldName = NamePair {
                    nameC = CName "day",
                    nameHsIdent = HsIdentifier
                      "date_day"},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = CName "DAY",
                      nameHsIdent = HsIdentifier
                        "DAY"}
                    NameOriginInSource,
                  structFieldOffset = 64,
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
          "Date",
        structConstr = HsName
          "@NsConstr"
          "Date",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "date_year",
            fieldType = HsTypRef
              (HsName "@NsTypeConstr" "YEAR"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "manual_examples.h:58:11",
                structFieldName = NamePair {
                  nameC = CName "year",
                  nameHsIdent = HsIdentifier
                    "date_year"},
                structFieldType =
                TypeMacroTypedef
                  NamePair {
                    nameC = CName "YEAR",
                    nameHsIdent = HsIdentifier
                      "YEAR"}
                  NameOriginInSource,
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "date_month",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "MONTH"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "manual_examples.h:59:11",
                structFieldName = NamePair {
                  nameC = CName "month",
                  nameHsIdent = HsIdentifier
                    "date_month"},
                structFieldType =
                TypeMacroTypedef
                  NamePair {
                    nameC = CName "MONTH",
                    nameHsIdent = HsIdentifier
                      "MONTH"}
                  NameOriginInSource,
                structFieldOffset = 32,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "date_day",
            fieldType = HsTypRef
              (HsName "@NsTypeConstr" "DAY"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "manual_examples.h:60:11",
                structFieldName = NamePair {
                  nameC = CName "day",
                  nameHsIdent = HsIdentifier
                    "date_day"},
                structFieldType =
                TypeMacroTypedef
                  NamePair {
                    nameC = CName "DAY",
                    nameHsIdent = HsIdentifier
                      "DAY"}
                  NameOriginInSource,
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "manual_examples.h:57:16",
              declId = NamePair {
                nameC = CName "date",
                nameHsIdent = HsIdentifier
                  "Date"},
              declOrigin = NameOriginInSource,
              declAliases = [CName "date"],
              declHeader =
              "manual_examples.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Date"),
                structSizeof = 12,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "manual_examples.h:58:11",
                    structFieldName = NamePair {
                      nameC = CName "year",
                      nameHsIdent = HsIdentifier
                        "date_year"},
                    structFieldType =
                    TypeMacroTypedef
                      NamePair {
                        nameC = CName "YEAR",
                        nameHsIdent = HsIdentifier
                          "YEAR"}
                      NameOriginInSource,
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "manual_examples.h:59:11",
                    structFieldName = NamePair {
                      nameC = CName "month",
                      nameHsIdent = HsIdentifier
                        "date_month"},
                    structFieldType =
                    TypeMacroTypedef
                      NamePair {
                        nameC = CName "MONTH",
                        nameHsIdent = HsIdentifier
                          "MONTH"}
                      NameOriginInSource,
                    structFieldOffset = 32,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "manual_examples.h:60:11",
                    structFieldName = NamePair {
                      nameC = CName "day",
                      nameHsIdent = HsIdentifier
                        "date_day"},
                    structFieldType =
                    TypeMacroTypedef
                      NamePair {
                        nameC = CName "DAY",
                        nameHsIdent = HsIdentifier
                          "DAY"}
                      NameOriginInSource,
                    structFieldOffset = 64,
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
        storableSizeOf = 12,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Date",
                structConstr = HsName
                  "@NsConstr"
                  "Date",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "date_year",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "YEAR"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:58:11",
                        structFieldName = NamePair {
                          nameC = CName "year",
                          nameHsIdent = HsIdentifier
                            "date_year"},
                        structFieldType =
                        TypeMacroTypedef
                          NamePair {
                            nameC = CName "YEAR",
                            nameHsIdent = HsIdentifier
                              "YEAR"}
                          NameOriginInSource,
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "date_month",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "MONTH"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:59:11",
                        structFieldName = NamePair {
                          nameC = CName "month",
                          nameHsIdent = HsIdentifier
                            "date_month"},
                        structFieldType =
                        TypeMacroTypedef
                          NamePair {
                            nameC = CName "MONTH",
                            nameHsIdent = HsIdentifier
                              "MONTH"}
                          NameOriginInSource,
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "date_day",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "DAY"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:60:11",
                        structFieldName = NamePair {
                          nameC = CName "day",
                          nameHsIdent = HsIdentifier
                            "date_day"},
                        structFieldType =
                        TypeMacroTypedef
                          NamePair {
                            nameC = CName "DAY",
                            nameHsIdent = HsIdentifier
                              "DAY"}
                          NameOriginInSource,
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "manual_examples.h:57:16",
                      declId = NamePair {
                        nameC = CName "date",
                        nameHsIdent = HsIdentifier
                          "Date"},
                      declOrigin = NameOriginInSource,
                      declAliases = [CName "date"],
                      declHeader =
                      "manual_examples.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Date"),
                        structSizeof = 12,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:58:11",
                            structFieldName = NamePair {
                              nameC = CName "year",
                              nameHsIdent = HsIdentifier
                                "date_year"},
                            structFieldType =
                            TypeMacroTypedef
                              NamePair {
                                nameC = CName "YEAR",
                                nameHsIdent = HsIdentifier
                                  "YEAR"}
                              NameOriginInSource,
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:59:11",
                            structFieldName = NamePair {
                              nameC = CName "month",
                              nameHsIdent = HsIdentifier
                                "date_month"},
                            structFieldType =
                            TypeMacroTypedef
                              NamePair {
                                nameC = CName "MONTH",
                                nameHsIdent = HsIdentifier
                                  "MONTH"}
                              NameOriginInSource,
                            structFieldOffset = 32,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:60:11",
                            structFieldName = NamePair {
                              nameC = CName "day",
                              nameHsIdent = HsIdentifier
                                "date_day"},
                            structFieldType =
                            TypeMacroTypedef
                              NamePair {
                                nameC = CName "DAY",
                                nameHsIdent = HsIdentifier
                                  "DAY"}
                              NameOriginInSource,
                            structFieldOffset = 64,
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
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 4,
              PeekByteOff (Idx 0) 8]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Date",
                structConstr = HsName
                  "@NsConstr"
                  "Date",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "date_year",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "YEAR"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:58:11",
                        structFieldName = NamePair {
                          nameC = CName "year",
                          nameHsIdent = HsIdentifier
                            "date_year"},
                        structFieldType =
                        TypeMacroTypedef
                          NamePair {
                            nameC = CName "YEAR",
                            nameHsIdent = HsIdentifier
                              "YEAR"}
                          NameOriginInSource,
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "date_month",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "MONTH"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:59:11",
                        structFieldName = NamePair {
                          nameC = CName "month",
                          nameHsIdent = HsIdentifier
                            "date_month"},
                        structFieldType =
                        TypeMacroTypedef
                          NamePair {
                            nameC = CName "MONTH",
                            nameHsIdent = HsIdentifier
                              "MONTH"}
                          NameOriginInSource,
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "date_day",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "DAY"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:60:11",
                        structFieldName = NamePair {
                          nameC = CName "day",
                          nameHsIdent = HsIdentifier
                            "date_day"},
                        structFieldType =
                        TypeMacroTypedef
                          NamePair {
                            nameC = CName "DAY",
                            nameHsIdent = HsIdentifier
                              "DAY"}
                          NameOriginInSource,
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "manual_examples.h:57:16",
                      declId = NamePair {
                        nameC = CName "date",
                        nameHsIdent = HsIdentifier
                          "Date"},
                      declOrigin = NameOriginInSource,
                      declAliases = [CName "date"],
                      declHeader =
                      "manual_examples.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Date"),
                        structSizeof = 12,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:58:11",
                            structFieldName = NamePair {
                              nameC = CName "year",
                              nameHsIdent = HsIdentifier
                                "date_year"},
                            structFieldType =
                            TypeMacroTypedef
                              NamePair {
                                nameC = CName "YEAR",
                                nameHsIdent = HsIdentifier
                                  "YEAR"}
                              NameOriginInSource,
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:59:11",
                            structFieldName = NamePair {
                              nameC = CName "month",
                              nameHsIdent = HsIdentifier
                                "date_month"},
                            structFieldType =
                            TypeMacroTypedef
                              NamePair {
                                nameC = CName "MONTH",
                                nameHsIdent = HsIdentifier
                                  "MONTH"}
                              NameOriginInSource,
                            structFieldOffset = 32,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:60:11",
                            structFieldName = NamePair {
                              nameC = CName "day",
                              nameHsIdent = HsIdentifier
                                "date_day"},
                            structFieldType =
                            TypeMacroTypedef
                              NamePair {
                                nameC = CName "DAY",
                                nameHsIdent = HsIdentifier
                                  "DAY"}
                              NameOriginInSource,
                            structFieldOffset = 64,
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
              (Add 3)
              (Seq
                [
                  PokeByteOff (Idx 4) 0 (Idx 0),
                  PokeByteOff (Idx 4) 4 (Idx 1),
                  PokeByteOff
                    (Idx 4)
                    8
                    (Idx 2)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Date"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Date"),
  DeclInlineCInclude
    "manual_examples.h",
  DeclInlineC
    "YEAR testmodule_getYear (date *arg1) { return getYear(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "getYear",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Date")))
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "YEAR"))),
      foreignImportOrigName =
      "testmodule_getYear",
      foreignImportHeader =
      "manual_examples.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePointer
              (TypeTypedef
                (TypedefSquashed
                  (CName "date")
                  (TypeStruct
                    NamePair {
                      nameC = CName "date",
                      nameHsIdent = HsIdentifier
                        "Date"}
                    NameOriginInSource)))],
          functionRes = TypeMacroTypedef
            NamePair {
              nameC = CName "YEAR",
              nameHsIdent = HsIdentifier
                "YEAR"}
            NameOriginInSource}},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Student",
      structConstr = HsName
        "@NsConstr"
        "Student",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "student_university",
          fieldType = HsPtr
            (HsPrimType HsPrimCChar),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "manual_examples.h:71:11",
              structFieldName = NamePair {
                nameC = CName "university",
                nameHsIdent = HsIdentifier
                  "student_university"},
              structFieldType = TypePointer
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "student_year",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "manual_examples.h:72:9",
              structFieldName = NamePair {
                nameC = CName "year",
                nameHsIdent = HsIdentifier
                  "student_year"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "manual_examples.h:70:10",
            declId = NamePair {
              nameC = CName "student",
              nameHsIdent = HsIdentifier
                "Student"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "manual_examples.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Student"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "manual_examples.h:71:11",
                  structFieldName = NamePair {
                    nameC = CName "university",
                    nameHsIdent = HsIdentifier
                      "student_university"},
                  structFieldType = TypePointer
                    (TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed)))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "manual_examples.h:72:9",
                  structFieldName = NamePair {
                    nameC = CName "year",
                    nameHsIdent = HsIdentifier
                      "student_year"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 64,
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
          "Student",
        structConstr = HsName
          "@NsConstr"
          "Student",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "student_university",
            fieldType = HsPtr
              (HsPrimType HsPrimCChar),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "manual_examples.h:71:11",
                structFieldName = NamePair {
                  nameC = CName "university",
                  nameHsIdent = HsIdentifier
                    "student_university"},
                structFieldType = TypePointer
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed)))),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "student_year",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "manual_examples.h:72:9",
                structFieldName = NamePair {
                  nameC = CName "year",
                  nameHsIdent = HsIdentifier
                    "student_year"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "manual_examples.h:70:10",
              declId = NamePair {
                nameC = CName "student",
                nameHsIdent = HsIdentifier
                  "Student"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader =
              "manual_examples.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Student"),
                structSizeof = 16,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "manual_examples.h:71:11",
                    structFieldName = NamePair {
                      nameC = CName "university",
                      nameHsIdent = HsIdentifier
                        "student_university"},
                    structFieldType = TypePointer
                      (TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed)))),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "manual_examples.h:72:9",
                    structFieldName = NamePair {
                      nameC = CName "year",
                      nameHsIdent = HsIdentifier
                        "student_year"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 64,
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
        storableSizeOf = 16,
        storableAlignment = 8,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Student",
                structConstr = HsName
                  "@NsConstr"
                  "Student",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "student_university",
                    fieldType = HsPtr
                      (HsPrimType HsPrimCChar),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:71:11",
                        structFieldName = NamePair {
                          nameC = CName "university",
                          nameHsIdent = HsIdentifier
                            "student_university"},
                        structFieldType = TypePointer
                          (TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed)))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "student_year",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:72:9",
                        structFieldName = NamePair {
                          nameC = CName "year",
                          nameHsIdent = HsIdentifier
                            "student_year"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "manual_examples.h:70:10",
                      declId = NamePair {
                        nameC = CName "student",
                        nameHsIdent = HsIdentifier
                          "Student"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "manual_examples.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Student"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:71:11",
                            structFieldName = NamePair {
                              nameC = CName "university",
                              nameHsIdent = HsIdentifier
                                "student_university"},
                            structFieldType = TypePointer
                              (TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed)))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:72:9",
                            structFieldName = NamePair {
                              nameC = CName "year",
                              nameHsIdent = HsIdentifier
                                "student_year"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 64,
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
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 8]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Student",
                structConstr = HsName
                  "@NsConstr"
                  "Student",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "student_university",
                    fieldType = HsPtr
                      (HsPrimType HsPrimCChar),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:71:11",
                        structFieldName = NamePair {
                          nameC = CName "university",
                          nameHsIdent = HsIdentifier
                            "student_university"},
                        structFieldType = TypePointer
                          (TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed)))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "student_year",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:72:9",
                        structFieldName = NamePair {
                          nameC = CName "year",
                          nameHsIdent = HsIdentifier
                            "student_year"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "manual_examples.h:70:10",
                      declId = NamePair {
                        nameC = CName "student",
                        nameHsIdent = HsIdentifier
                          "Student"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "manual_examples.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Student"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:71:11",
                            structFieldName = NamePair {
                              nameC = CName "university",
                              nameHsIdent = HsIdentifier
                                "student_university"},
                            structFieldType = TypePointer
                              (TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed)))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:72:9",
                            structFieldName = NamePair {
                              nameC = CName "year",
                              nameHsIdent = HsIdentifier
                                "student_year"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 64,
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
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    8
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Student"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Student"),
  DeclEmpty
    EmptyData {
      emptyDataName = HsName
        "@NsTypeConstr"
        "Person",
      emptyDataOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "manual_examples.h:77:12",
          declId = NamePair {
            nameC = CName "person",
            nameHsIdent = HsIdentifier
              "Person"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "manual_examples.h"},
        declKind = OpaqueStruct,
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}}},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Employee",
      structConstr = HsName
        "@NsConstr"
        "Employee",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "employee_company",
          fieldType = HsPtr
            (HsPrimType HsPrimCChar),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "manual_examples.h:76:11",
              structFieldName = NamePair {
                nameC = CName "company",
                nameHsIdent = HsIdentifier
                  "employee_company"},
              structFieldType = TypePointer
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "employee_supervisor",
          fieldType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Person")),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "manual_examples.h:77:20",
              structFieldName = NamePair {
                nameC = CName "supervisor",
                nameHsIdent = HsIdentifier
                  "employee_supervisor"},
              structFieldType = TypePointer
                (TypeStruct
                  NamePair {
                    nameC = CName "person",
                    nameHsIdent = HsIdentifier
                      "Person"}
                  NameOriginInSource),
              structFieldOffset = 64,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "employee_salary",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "manual_examples.h:78:9",
              structFieldName = NamePair {
                nameC = CName "salary",
                nameHsIdent = HsIdentifier
                  "employee_salary"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 128,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "manual_examples.h:75:10",
            declId = NamePair {
              nameC = CName "employee",
              nameHsIdent = HsIdentifier
                "Employee"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "manual_examples.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Employee"),
              structSizeof = 24,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "manual_examples.h:76:11",
                  structFieldName = NamePair {
                    nameC = CName "company",
                    nameHsIdent = HsIdentifier
                      "employee_company"},
                  structFieldType = TypePointer
                    (TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed)))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "manual_examples.h:77:20",
                  structFieldName = NamePair {
                    nameC = CName "supervisor",
                    nameHsIdent = HsIdentifier
                      "employee_supervisor"},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = CName "person",
                        nameHsIdent = HsIdentifier
                          "Person"}
                      NameOriginInSource),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "manual_examples.h:78:9",
                  structFieldName = NamePair {
                    nameC = CName "salary",
                    nameHsIdent = HsIdentifier
                      "employee_salary"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 128,
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
          "Employee",
        structConstr = HsName
          "@NsConstr"
          "Employee",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "employee_company",
            fieldType = HsPtr
              (HsPrimType HsPrimCChar),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "manual_examples.h:76:11",
                structFieldName = NamePair {
                  nameC = CName "company",
                  nameHsIdent = HsIdentifier
                    "employee_company"},
                structFieldType = TypePointer
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed)))),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "employee_supervisor",
            fieldType = HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Person")),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "manual_examples.h:77:20",
                structFieldName = NamePair {
                  nameC = CName "supervisor",
                  nameHsIdent = HsIdentifier
                    "employee_supervisor"},
                structFieldType = TypePointer
                  (TypeStruct
                    NamePair {
                      nameC = CName "person",
                      nameHsIdent = HsIdentifier
                        "Person"}
                    NameOriginInSource),
                structFieldOffset = 64,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "employee_salary",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "manual_examples.h:78:9",
                structFieldName = NamePair {
                  nameC = CName "salary",
                  nameHsIdent = HsIdentifier
                    "employee_salary"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 128,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "manual_examples.h:75:10",
              declId = NamePair {
                nameC = CName "employee",
                nameHsIdent = HsIdentifier
                  "Employee"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader =
              "manual_examples.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Employee"),
                structSizeof = 24,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "manual_examples.h:76:11",
                    structFieldName = NamePair {
                      nameC = CName "company",
                      nameHsIdent = HsIdentifier
                        "employee_company"},
                    structFieldType = TypePointer
                      (TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed)))),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "manual_examples.h:77:20",
                    structFieldName = NamePair {
                      nameC = CName "supervisor",
                      nameHsIdent = HsIdentifier
                        "employee_supervisor"},
                    structFieldType = TypePointer
                      (TypeStruct
                        NamePair {
                          nameC = CName "person",
                          nameHsIdent = HsIdentifier
                            "Person"}
                        NameOriginInSource),
                    structFieldOffset = 64,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "manual_examples.h:78:9",
                    structFieldName = NamePair {
                      nameC = CName "salary",
                      nameHsIdent = HsIdentifier
                        "employee_salary"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 128,
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
        storableSizeOf = 24,
        storableAlignment = 8,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Employee",
                structConstr = HsName
                  "@NsConstr"
                  "Employee",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "employee_company",
                    fieldType = HsPtr
                      (HsPrimType HsPrimCChar),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:76:11",
                        structFieldName = NamePair {
                          nameC = CName "company",
                          nameHsIdent = HsIdentifier
                            "employee_company"},
                        structFieldType = TypePointer
                          (TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed)))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "employee_supervisor",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Person")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:77:20",
                        structFieldName = NamePair {
                          nameC = CName "supervisor",
                          nameHsIdent = HsIdentifier
                            "employee_supervisor"},
                        structFieldType = TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = CName "person",
                              nameHsIdent = HsIdentifier
                                "Person"}
                            NameOriginInSource),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "employee_salary",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:78:9",
                        structFieldName = NamePair {
                          nameC = CName "salary",
                          nameHsIdent = HsIdentifier
                            "employee_salary"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 128,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "manual_examples.h:75:10",
                      declId = NamePair {
                        nameC = CName "employee",
                        nameHsIdent = HsIdentifier
                          "Employee"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "manual_examples.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Employee"),
                        structSizeof = 24,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:76:11",
                            structFieldName = NamePair {
                              nameC = CName "company",
                              nameHsIdent = HsIdentifier
                                "employee_company"},
                            structFieldType = TypePointer
                              (TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed)))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:77:20",
                            structFieldName = NamePair {
                              nameC = CName "supervisor",
                              nameHsIdent = HsIdentifier
                                "employee_supervisor"},
                            structFieldType = TypePointer
                              (TypeStruct
                                NamePair {
                                  nameC = CName "person",
                                  nameHsIdent = HsIdentifier
                                    "Person"}
                                NameOriginInSource),
                            structFieldOffset = 64,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:78:9",
                            structFieldName = NamePair {
                              nameC = CName "salary",
                              nameHsIdent = HsIdentifier
                                "employee_salary"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 128,
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
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 8,
              PeekByteOff (Idx 0) 16]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Employee",
                structConstr = HsName
                  "@NsConstr"
                  "Employee",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "employee_company",
                    fieldType = HsPtr
                      (HsPrimType HsPrimCChar),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:76:11",
                        structFieldName = NamePair {
                          nameC = CName "company",
                          nameHsIdent = HsIdentifier
                            "employee_company"},
                        structFieldType = TypePointer
                          (TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed)))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "employee_supervisor",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Person")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:77:20",
                        structFieldName = NamePair {
                          nameC = CName "supervisor",
                          nameHsIdent = HsIdentifier
                            "employee_supervisor"},
                        structFieldType = TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = CName "person",
                              nameHsIdent = HsIdentifier
                                "Person"}
                            NameOriginInSource),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "employee_salary",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:78:9",
                        structFieldName = NamePair {
                          nameC = CName "salary",
                          nameHsIdent = HsIdentifier
                            "employee_salary"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 128,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "manual_examples.h:75:10",
                      declId = NamePair {
                        nameC = CName "employee",
                        nameHsIdent = HsIdentifier
                          "Employee"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "manual_examples.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Employee"),
                        structSizeof = 24,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:76:11",
                            structFieldName = NamePair {
                              nameC = CName "company",
                              nameHsIdent = HsIdentifier
                                "employee_company"},
                            structFieldType = TypePointer
                              (TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed)))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:77:20",
                            structFieldName = NamePair {
                              nameC = CName "supervisor",
                              nameHsIdent = HsIdentifier
                                "employee_supervisor"},
                            structFieldType = TypePointer
                              (TypeStruct
                                NamePair {
                                  nameC = CName "person",
                                  nameHsIdent = HsIdentifier
                                    "Person"}
                                NameOriginInSource),
                            structFieldOffset = 64,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:78:9",
                            structFieldName = NamePair {
                              nameC = CName "salary",
                              nameHsIdent = HsIdentifier
                                "employee_salary"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 128,
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
              (Add 3)
              (Seq
                [
                  PokeByteOff (Idx 4) 0 (Idx 0),
                  PokeByteOff (Idx 4) 8 (Idx 1),
                  PokeByteOff
                    (Idx 4)
                    16
                    (Idx 2)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Employee"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Employee"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Occupation",
      newtypeConstr = HsName
        "@NsConstr"
        "Occupation",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Occupation",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "manual_examples.h:69:15",
          declId = NamePair {
            nameC = CName "occupation",
            nameHsIdent = HsIdentifier
              "Occupation"},
          declOrigin = NameOriginInSource,
          declAliases = [
            CName "occupation"],
          declHeader =
          "manual_examples.h"},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Occupation",
              newtypeField = HsName
                "@NsVar"
                "un_Occupation"},
            unionSizeof = 24,
            unionAlignment = 8,
            unionFields = [
              UnionField {
                unionFieldLoc =
                "manual_examples.h:73:5",
                unionFieldName = NamePair {
                  nameC = CName "student",
                  nameHsIdent = HsIdentifier
                    "occupation_student"},
                unionFieldType = TypeStruct
                  NamePair {
                    nameC = CName "student",
                    nameHsIdent = HsIdentifier
                      "Student"}
                  NameOriginInSource},
              UnionField {
                unionFieldLoc =
                "manual_examples.h:79:5",
                unionFieldName = NamePair {
                  nameC = CName "employee",
                  nameHsIdent = HsIdentifier
                    "occupation_employee"},
                unionFieldType = TypeStruct
                  NamePair {
                    nameC = CName "employee",
                    nameHsIdent = HsIdentifier
                      "Employee"}
                  NameOriginInSource}]},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Storable]},
  DeclNewtypeInstance
    (DeriveVia
      (HsSizedByteArray 24 8))
    Storable
    (HsName
      "@NsTypeConstr"
      "Occupation"),
  DeclUnionGetter
    (HsName
      "@NsTypeConstr"
      "Occupation")
    (HsTypRef
      (HsName
        "@NsTypeConstr"
        "Student"))
    (HsName
      "@NsVar"
      "get_occupation_student"),
  DeclUnionSetter
    (HsName
      "@NsTypeConstr"
      "Occupation")
    (HsTypRef
      (HsName
        "@NsTypeConstr"
        "Student"))
    (HsName
      "@NsVar"
      "set_occupation_student"),
  DeclUnionGetter
    (HsName
      "@NsTypeConstr"
      "Occupation")
    (HsTypRef
      (HsName
        "@NsTypeConstr"
        "Employee"))
    (HsName
      "@NsVar"
      "get_occupation_employee"),
  DeclUnionSetter
    (HsName
      "@NsTypeConstr"
      "Occupation")
    (HsTypRef
      (HsName
        "@NsTypeConstr"
        "Employee"))
    (HsName
      "@NsVar"
      "set_occupation_employee"),
  DeclInlineCInclude
    "manual_examples.h",
  DeclInlineC
    "void testmodule_print_occupation (signed int arg1, occupation *arg2) { print_occupation(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "print_occupation",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsFun
          (HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Occupation")))
          (HsIO (HsPrimType HsPrimUnit))),
      foreignImportOrigName =
      "testmodule_print_occupation",
      foreignImportHeader =
      "manual_examples.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypePointer
              (TypeTypedef
                (TypedefSquashed
                  (CName "occupation")
                  (TypeUnion
                    NamePair {
                      nameC = CName "occupation",
                      nameHsIdent = HsIdentifier
                        "Occupation"}
                    NameOriginInSource)))],
          functionRes = TypeVoid}},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Rect_lower_left",
      structConstr = HsName
        "@NsConstr"
        "Rect_lower_left",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "rect_lower_left_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "manual_examples.h:90:9",
              structFieldName = NamePair {
                nameC = CName "x",
                nameHsIdent = HsIdentifier
                  "rect_lower_left_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "rect_lower_left_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "manual_examples.h:91:9",
              structFieldName = NamePair {
                nameC = CName "y",
                nameHsIdent = HsIdentifier
                  "rect_lower_left_y"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "manual_examples.h:89:3",
            declId = NamePair {
              nameC = CName "rect_lower_left",
              nameHsIdent = HsIdentifier
                "Rect_lower_left"},
            declOrigin = NameOriginGenerated
              (AnonId
                "manual_examples.h:89:3"),
            declAliases = [],
            declHeader =
            "manual_examples.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Rect_lower_left"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "manual_examples.h:90:9",
                  structFieldName = NamePair {
                    nameC = CName "x",
                    nameHsIdent = HsIdentifier
                      "rect_lower_left_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "manual_examples.h:91:9",
                  structFieldName = NamePair {
                    nameC = CName "y",
                    nameHsIdent = HsIdentifier
                      "rect_lower_left_y"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
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
          "Rect_lower_left",
        structConstr = HsName
          "@NsConstr"
          "Rect_lower_left",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "rect_lower_left_x",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "manual_examples.h:90:9",
                structFieldName = NamePair {
                  nameC = CName "x",
                  nameHsIdent = HsIdentifier
                    "rect_lower_left_x"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "rect_lower_left_y",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "manual_examples.h:91:9",
                structFieldName = NamePair {
                  nameC = CName "y",
                  nameHsIdent = HsIdentifier
                    "rect_lower_left_y"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 32,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "manual_examples.h:89:3",
              declId = NamePair {
                nameC = CName "rect_lower_left",
                nameHsIdent = HsIdentifier
                  "Rect_lower_left"},
              declOrigin = NameOriginGenerated
                (AnonId
                  "manual_examples.h:89:3"),
              declAliases = [],
              declHeader =
              "manual_examples.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "Rect_lower_left"),
                structSizeof = 8,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "manual_examples.h:90:9",
                    structFieldName = NamePair {
                      nameC = CName "x",
                      nameHsIdent = HsIdentifier
                        "rect_lower_left_x"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "manual_examples.h:91:9",
                    structFieldName = NamePair {
                      nameC = CName "y",
                      nameHsIdent = HsIdentifier
                        "rect_lower_left_y"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 32,
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
        storableSizeOf = 8,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Rect_lower_left",
                structConstr = HsName
                  "@NsConstr"
                  "Rect_lower_left",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_lower_left_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:90:9",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "rect_lower_left_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_lower_left_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:91:9",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "rect_lower_left_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "manual_examples.h:89:3",
                      declId = NamePair {
                        nameC = CName "rect_lower_left",
                        nameHsIdent = HsIdentifier
                          "Rect_lower_left"},
                      declOrigin = NameOriginGenerated
                        (AnonId
                          "manual_examples.h:89:3"),
                      declAliases = [],
                      declHeader =
                      "manual_examples.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Rect_lower_left"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:90:9",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "rect_lower_left_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:91:9",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "rect_lower_left_y"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
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
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 4]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Rect_lower_left",
                structConstr = HsName
                  "@NsConstr"
                  "Rect_lower_left",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_lower_left_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:90:9",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "rect_lower_left_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_lower_left_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:91:9",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "rect_lower_left_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "manual_examples.h:89:3",
                      declId = NamePair {
                        nameC = CName "rect_lower_left",
                        nameHsIdent = HsIdentifier
                          "Rect_lower_left"},
                      declOrigin = NameOriginGenerated
                        (AnonId
                          "manual_examples.h:89:3"),
                      declAliases = [],
                      declHeader =
                      "manual_examples.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Rect_lower_left"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:90:9",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "rect_lower_left_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:91:9",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "rect_lower_left_y"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
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
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    4
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Rect_lower_left"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Rect_lower_left"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Rect_upper_right",
      structConstr = HsName
        "@NsConstr"
        "Rect_upper_right",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "rect_upper_right_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "manual_examples.h:95:9",
              structFieldName = NamePair {
                nameC = CName "x",
                nameHsIdent = HsIdentifier
                  "rect_upper_right_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "rect_upper_right_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "manual_examples.h:96:9",
              structFieldName = NamePair {
                nameC = CName "y",
                nameHsIdent = HsIdentifier
                  "rect_upper_right_y"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "manual_examples.h:94:3",
            declId = NamePair {
              nameC = CName
                "rect_upper_right",
              nameHsIdent = HsIdentifier
                "Rect_upper_right"},
            declOrigin = NameOriginGenerated
              (AnonId
                "manual_examples.h:94:3"),
            declAliases = [],
            declHeader =
            "manual_examples.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Rect_upper_right"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "manual_examples.h:95:9",
                  structFieldName = NamePair {
                    nameC = CName "x",
                    nameHsIdent = HsIdentifier
                      "rect_upper_right_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "manual_examples.h:96:9",
                  structFieldName = NamePair {
                    nameC = CName "y",
                    nameHsIdent = HsIdentifier
                      "rect_upper_right_y"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
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
          "Rect_upper_right",
        structConstr = HsName
          "@NsConstr"
          "Rect_upper_right",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "rect_upper_right_x",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "manual_examples.h:95:9",
                structFieldName = NamePair {
                  nameC = CName "x",
                  nameHsIdent = HsIdentifier
                    "rect_upper_right_x"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "rect_upper_right_y",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "manual_examples.h:96:9",
                structFieldName = NamePair {
                  nameC = CName "y",
                  nameHsIdent = HsIdentifier
                    "rect_upper_right_y"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 32,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "manual_examples.h:94:3",
              declId = NamePair {
                nameC = CName
                  "rect_upper_right",
                nameHsIdent = HsIdentifier
                  "Rect_upper_right"},
              declOrigin = NameOriginGenerated
                (AnonId
                  "manual_examples.h:94:3"),
              declAliases = [],
              declHeader =
              "manual_examples.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "Rect_upper_right"),
                structSizeof = 8,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "manual_examples.h:95:9",
                    structFieldName = NamePair {
                      nameC = CName "x",
                      nameHsIdent = HsIdentifier
                        "rect_upper_right_x"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "manual_examples.h:96:9",
                    structFieldName = NamePair {
                      nameC = CName "y",
                      nameHsIdent = HsIdentifier
                        "rect_upper_right_y"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 32,
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
        storableSizeOf = 8,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Rect_upper_right",
                structConstr = HsName
                  "@NsConstr"
                  "Rect_upper_right",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_upper_right_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:95:9",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "rect_upper_right_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_upper_right_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:96:9",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "rect_upper_right_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "manual_examples.h:94:3",
                      declId = NamePair {
                        nameC = CName
                          "rect_upper_right",
                        nameHsIdent = HsIdentifier
                          "Rect_upper_right"},
                      declOrigin = NameOriginGenerated
                        (AnonId
                          "manual_examples.h:94:3"),
                      declAliases = [],
                      declHeader =
                      "manual_examples.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Rect_upper_right"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:95:9",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "rect_upper_right_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:96:9",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "rect_upper_right_y"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
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
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 4]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Rect_upper_right",
                structConstr = HsName
                  "@NsConstr"
                  "Rect_upper_right",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_upper_right_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:95:9",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "rect_upper_right_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_upper_right_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:96:9",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "rect_upper_right_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "manual_examples.h:94:3",
                      declId = NamePair {
                        nameC = CName
                          "rect_upper_right",
                        nameHsIdent = HsIdentifier
                          "Rect_upper_right"},
                      declOrigin = NameOriginGenerated
                        (AnonId
                          "manual_examples.h:94:3"),
                      declAliases = [],
                      declHeader =
                      "manual_examples.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Rect_upper_right"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:95:9",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "rect_upper_right_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:96:9",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "rect_upper_right_y"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
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
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    4
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Rect_upper_right"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Rect_upper_right"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Rect",
      structConstr = HsName
        "@NsConstr"
        "Rect",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "rect_lower_left",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Rect_lower_left"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "manual_examples.h:92:5",
              structFieldName = NamePair {
                nameC = CName "lower_left",
                nameHsIdent = HsIdentifier
                  "rect_lower_left"},
              structFieldType = TypeStruct
                NamePair {
                  nameC = CName "rect_lower_left",
                  nameHsIdent = HsIdentifier
                    "Rect_lower_left"}
                (NameOriginGenerated
                  (AnonId
                    "manual_examples.h:89:3")),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "rect_upper_right",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Rect_upper_right"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "manual_examples.h:97:5",
              structFieldName = NamePair {
                nameC = CName "upper_right",
                nameHsIdent = HsIdentifier
                  "rect_upper_right"},
              structFieldType = TypeStruct
                NamePair {
                  nameC = CName
                    "rect_upper_right",
                  nameHsIdent = HsIdentifier
                    "Rect_upper_right"}
                (NameOriginGenerated
                  (AnonId
                    "manual_examples.h:94:3")),
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "manual_examples.h:88:8",
            declId = NamePair {
              nameC = CName "rect",
              nameHsIdent = HsIdentifier
                "Rect"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "manual_examples.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Rect"),
              structSizeof = 16,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "manual_examples.h:92:5",
                  structFieldName = NamePair {
                    nameC = CName "lower_left",
                    nameHsIdent = HsIdentifier
                      "rect_lower_left"},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = CName "rect_lower_left",
                      nameHsIdent = HsIdentifier
                        "Rect_lower_left"}
                    (NameOriginGenerated
                      (AnonId
                        "manual_examples.h:89:3")),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "manual_examples.h:97:5",
                  structFieldName = NamePair {
                    nameC = CName "upper_right",
                    nameHsIdent = HsIdentifier
                      "rect_upper_right"},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = CName
                        "rect_upper_right",
                      nameHsIdent = HsIdentifier
                        "Rect_upper_right"}
                    (NameOriginGenerated
                      (AnonId
                        "manual_examples.h:94:3")),
                  structFieldOffset = 64,
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
          "Rect",
        structConstr = HsName
          "@NsConstr"
          "Rect",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "rect_lower_left",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Rect_lower_left"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "manual_examples.h:92:5",
                structFieldName = NamePair {
                  nameC = CName "lower_left",
                  nameHsIdent = HsIdentifier
                    "rect_lower_left"},
                structFieldType = TypeStruct
                  NamePair {
                    nameC = CName "rect_lower_left",
                    nameHsIdent = HsIdentifier
                      "Rect_lower_left"}
                  (NameOriginGenerated
                    (AnonId
                      "manual_examples.h:89:3")),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "rect_upper_right",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Rect_upper_right"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "manual_examples.h:97:5",
                structFieldName = NamePair {
                  nameC = CName "upper_right",
                  nameHsIdent = HsIdentifier
                    "rect_upper_right"},
                structFieldType = TypeStruct
                  NamePair {
                    nameC = CName
                      "rect_upper_right",
                    nameHsIdent = HsIdentifier
                      "Rect_upper_right"}
                  (NameOriginGenerated
                    (AnonId
                      "manual_examples.h:94:3")),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "manual_examples.h:88:8",
              declId = NamePair {
                nameC = CName "rect",
                nameHsIdent = HsIdentifier
                  "Rect"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader =
              "manual_examples.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Rect"),
                structSizeof = 16,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "manual_examples.h:92:5",
                    structFieldName = NamePair {
                      nameC = CName "lower_left",
                      nameHsIdent = HsIdentifier
                        "rect_lower_left"},
                    structFieldType = TypeStruct
                      NamePair {
                        nameC = CName "rect_lower_left",
                        nameHsIdent = HsIdentifier
                          "Rect_lower_left"}
                      (NameOriginGenerated
                        (AnonId
                          "manual_examples.h:89:3")),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "manual_examples.h:97:5",
                    structFieldName = NamePair {
                      nameC = CName "upper_right",
                      nameHsIdent = HsIdentifier
                        "rect_upper_right"},
                    structFieldType = TypeStruct
                      NamePair {
                        nameC = CName
                          "rect_upper_right",
                        nameHsIdent = HsIdentifier
                          "Rect_upper_right"}
                      (NameOriginGenerated
                        (AnonId
                          "manual_examples.h:94:3")),
                    structFieldOffset = 64,
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
        storableSizeOf = 16,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Rect",
                structConstr = HsName
                  "@NsConstr"
                  "Rect",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_lower_left",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Rect_lower_left"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:92:5",
                        structFieldName = NamePair {
                          nameC = CName "lower_left",
                          nameHsIdent = HsIdentifier
                            "rect_lower_left"},
                        structFieldType = TypeStruct
                          NamePair {
                            nameC = CName "rect_lower_left",
                            nameHsIdent = HsIdentifier
                              "Rect_lower_left"}
                          (NameOriginGenerated
                            (AnonId
                              "manual_examples.h:89:3")),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_upper_right",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Rect_upper_right"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:97:5",
                        structFieldName = NamePair {
                          nameC = CName "upper_right",
                          nameHsIdent = HsIdentifier
                            "rect_upper_right"},
                        structFieldType = TypeStruct
                          NamePair {
                            nameC = CName
                              "rect_upper_right",
                            nameHsIdent = HsIdentifier
                              "Rect_upper_right"}
                          (NameOriginGenerated
                            (AnonId
                              "manual_examples.h:94:3")),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "manual_examples.h:88:8",
                      declId = NamePair {
                        nameC = CName "rect",
                        nameHsIdent = HsIdentifier
                          "Rect"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "manual_examples.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Rect"),
                        structSizeof = 16,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:92:5",
                            structFieldName = NamePair {
                              nameC = CName "lower_left",
                              nameHsIdent = HsIdentifier
                                "rect_lower_left"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = CName "rect_lower_left",
                                nameHsIdent = HsIdentifier
                                  "Rect_lower_left"}
                              (NameOriginGenerated
                                (AnonId
                                  "manual_examples.h:89:3")),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:97:5",
                            structFieldName = NamePair {
                              nameC = CName "upper_right",
                              nameHsIdent = HsIdentifier
                                "rect_upper_right"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = CName
                                  "rect_upper_right",
                                nameHsIdent = HsIdentifier
                                  "Rect_upper_right"}
                              (NameOriginGenerated
                                (AnonId
                                  "manual_examples.h:94:3")),
                            structFieldOffset = 64,
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
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 8]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Rect",
                structConstr = HsName
                  "@NsConstr"
                  "Rect",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_lower_left",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Rect_lower_left"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:92:5",
                        structFieldName = NamePair {
                          nameC = CName "lower_left",
                          nameHsIdent = HsIdentifier
                            "rect_lower_left"},
                        structFieldType = TypeStruct
                          NamePair {
                            nameC = CName "rect_lower_left",
                            nameHsIdent = HsIdentifier
                              "Rect_lower_left"}
                          (NameOriginGenerated
                            (AnonId
                              "manual_examples.h:89:3")),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_upper_right",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Rect_upper_right"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:97:5",
                        structFieldName = NamePair {
                          nameC = CName "upper_right",
                          nameHsIdent = HsIdentifier
                            "rect_upper_right"},
                        structFieldType = TypeStruct
                          NamePair {
                            nameC = CName
                              "rect_upper_right",
                            nameHsIdent = HsIdentifier
                              "Rect_upper_right"}
                          (NameOriginGenerated
                            (AnonId
                              "manual_examples.h:94:3")),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "manual_examples.h:88:8",
                      declId = NamePair {
                        nameC = CName "rect",
                        nameHsIdent = HsIdentifier
                          "Rect"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "manual_examples.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Rect"),
                        structSizeof = 16,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:92:5",
                            structFieldName = NamePair {
                              nameC = CName "lower_left",
                              nameHsIdent = HsIdentifier
                                "rect_lower_left"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = CName "rect_lower_left",
                                nameHsIdent = HsIdentifier
                                  "Rect_lower_left"}
                              (NameOriginGenerated
                                (AnonId
                                  "manual_examples.h:89:3")),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:97:5",
                            structFieldName = NamePair {
                              nameC = CName "upper_right",
                              nameHsIdent = HsIdentifier
                                "rect_upper_right"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = CName
                                  "rect_upper_right",
                                nameHsIdent = HsIdentifier
                                  "Rect_upper_right"}
                              (NameOriginGenerated
                                (AnonId
                                  "manual_examples.h:94:3")),
                            structFieldOffset = 64,
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
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    8
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Rect"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Rect"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Config_Deref",
      structConstr = HsName
        "@NsConstr"
        "Config_Deref",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "config_Deref_width",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "manual_examples.h:101:7",
              structFieldName = NamePair {
                nameC = CName "width",
                nameHsIdent = HsIdentifier
                  "config_Deref_width"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "config_Deref_height",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "manual_examples.h:102:7",
              structFieldName = NamePair {
                nameC = CName "height",
                nameHsIdent = HsIdentifier
                  "config_Deref_height"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "manual_examples.h:100:9",
            declId = NamePair {
              nameC = CName "config_Deref",
              nameHsIdent = HsIdentifier
                "Config_Deref"},
            declOrigin = NameOriginGenerated
              (AnonId
                "manual_examples.h:100:9"),
            declAliases = [],
            declHeader =
            "manual_examples.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Config_Deref"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "manual_examples.h:101:7",
                  structFieldName = NamePair {
                    nameC = CName "width",
                    nameHsIdent = HsIdentifier
                      "config_Deref_width"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "manual_examples.h:102:7",
                  structFieldName = NamePair {
                    nameC = CName "height",
                    nameHsIdent = HsIdentifier
                      "config_Deref_height"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
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
          "Config_Deref",
        structConstr = HsName
          "@NsConstr"
          "Config_Deref",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "config_Deref_width",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "manual_examples.h:101:7",
                structFieldName = NamePair {
                  nameC = CName "width",
                  nameHsIdent = HsIdentifier
                    "config_Deref_width"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "config_Deref_height",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "manual_examples.h:102:7",
                structFieldName = NamePair {
                  nameC = CName "height",
                  nameHsIdent = HsIdentifier
                    "config_Deref_height"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 32,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "manual_examples.h:100:9",
              declId = NamePair {
                nameC = CName "config_Deref",
                nameHsIdent = HsIdentifier
                  "Config_Deref"},
              declOrigin = NameOriginGenerated
                (AnonId
                  "manual_examples.h:100:9"),
              declAliases = [],
              declHeader =
              "manual_examples.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "Config_Deref"),
                structSizeof = 8,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "manual_examples.h:101:7",
                    structFieldName = NamePair {
                      nameC = CName "width",
                      nameHsIdent = HsIdentifier
                        "config_Deref_width"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "manual_examples.h:102:7",
                    structFieldName = NamePair {
                      nameC = CName "height",
                      nameHsIdent = HsIdentifier
                        "config_Deref_height"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 32,
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
        storableSizeOf = 8,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Config_Deref",
                structConstr = HsName
                  "@NsConstr"
                  "Config_Deref",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "config_Deref_width",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:101:7",
                        structFieldName = NamePair {
                          nameC = CName "width",
                          nameHsIdent = HsIdentifier
                            "config_Deref_width"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "config_Deref_height",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:102:7",
                        structFieldName = NamePair {
                          nameC = CName "height",
                          nameHsIdent = HsIdentifier
                            "config_Deref_height"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "manual_examples.h:100:9",
                      declId = NamePair {
                        nameC = CName "config_Deref",
                        nameHsIdent = HsIdentifier
                          "Config_Deref"},
                      declOrigin = NameOriginGenerated
                        (AnonId
                          "manual_examples.h:100:9"),
                      declAliases = [],
                      declHeader =
                      "manual_examples.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Config_Deref"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:101:7",
                            structFieldName = NamePair {
                              nameC = CName "width",
                              nameHsIdent = HsIdentifier
                                "config_Deref_width"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:102:7",
                            structFieldName = NamePair {
                              nameC = CName "height",
                              nameHsIdent = HsIdentifier
                                "config_Deref_height"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
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
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 4]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Config_Deref",
                structConstr = HsName
                  "@NsConstr"
                  "Config_Deref",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "config_Deref_width",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:101:7",
                        structFieldName = NamePair {
                          nameC = CName "width",
                          nameHsIdent = HsIdentifier
                            "config_Deref_width"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "config_Deref_height",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "manual_examples.h:102:7",
                        structFieldName = NamePair {
                          nameC = CName "height",
                          nameHsIdent = HsIdentifier
                            "config_Deref_height"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "manual_examples.h:100:9",
                      declId = NamePair {
                        nameC = CName "config_Deref",
                        nameHsIdent = HsIdentifier
                          "Config_Deref"},
                      declOrigin = NameOriginGenerated
                        (AnonId
                          "manual_examples.h:100:9"),
                      declAliases = [],
                      declHeader =
                      "manual_examples.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Config_Deref"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:101:7",
                            structFieldName = NamePair {
                              nameC = CName "width",
                              nameHsIdent = HsIdentifier
                                "config_Deref_width"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "manual_examples.h:102:7",
                            structFieldName = NamePair {
                              nameC = CName "height",
                              nameHsIdent = HsIdentifier
                                "config_Deref_height"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
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
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    4
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Config_Deref"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Config_Deref"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Config",
      newtypeConstr = HsName
        "@NsConstr"
        "Config",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Config",
        fieldType = HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Config_Deref")),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "manual_examples.h:103:4",
          declId = NamePair {
            nameC = CName "config",
            nameHsIdent = HsIdentifier
              "Config"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "manual_examples.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Config",
              newtypeField = HsName
                "@NsVar"
                "un_Config"},
            typedefType = TypePointer
              (TypeStruct
                NamePair {
                  nameC = CName "config_Deref",
                  nameHsIdent = HsIdentifier
                    "Config_Deref"}
                (NameOriginGenerated
                  (AnonId
                    "manual_examples.h:100:9")))},
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
      "Config"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Config"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Config"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Config"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Adio'0301s",
      newtypeConstr = HsName
        "@NsConstr"
        "Adio'0301s",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Adio'0301s",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "manual_examples.h:109:13",
          declId = NamePair {
            nameC = CName "adio\769s",
            nameHsIdent = HsIdentifier
              "Adio'0301s"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "manual_examples.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Adio'0301s",
              newtypeField = HsName
                "@NsVar"
                "un_Adio'0301s"},
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
          Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclInlineCInclude
    "manual_examples.h",
  DeclInlineC
    "void testmodule_\25308\25308 (void) { \25308\25308(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "\25308\25308",
      foreignImportType = HsIO
        (HsPrimType HsPrimUnit),
      foreignImportOrigName =
      "testmodule_\25308\25308",
      foreignImportHeader =
      "manual_examples.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [],
          functionRes = TypeVoid}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "C\25968\23383",
      newtypeConstr = HsName
        "@NsConstr"
        "C\25968\23383",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_C\25968\23383",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "manual_examples.h:116:13",
          declId = NamePair {
            nameC = CName "\25968\23383",
            nameHsIdent = HsIdentifier
              "C\25968\23383"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "manual_examples.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "C\25968\23383",
              newtypeField = HsName
                "@NsVar"
                "un_C\25968\23383"},
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
          Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclInlineCInclude
    "manual_examples.h",
  DeclInlineC
    "void testmodule_\978 (void) { \978(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "c\978",
      foreignImportType = HsIO
        (HsPrimType HsPrimUnit),
      foreignImportOrigName =
      "testmodule_\978",
      foreignImportHeader =
      "manual_examples.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [],
          functionRes = TypeVoid}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Data",
      newtypeConstr = HsName
        "@NsConstr"
        "Data",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Data",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "manual_examples.h:123:13",
          declId = NamePair {
            nameC = CName "data",
            nameHsIdent = HsIdentifier
              "Data"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "manual_examples.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Data",
              newtypeField = HsName
                "@NsVar"
                "un_Data"},
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
          Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "Data"),
  DeclInlineCInclude
    "manual_examples.h",
  DeclInlineC
    "void testmodule_import (void) { import(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "import'",
      foreignImportType = HsIO
        (HsPrimType HsPrimUnit),
      foreignImportOrigName =
      "testmodule_import",
      foreignImportHeader =
      "manual_examples.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [],
          functionRes = TypeVoid}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Signal",
      newtypeConstr = HsName
        "@NsConstr"
        "Signal",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Signal",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "manual_examples.h:130:6",
          declId = NamePair {
            nameC = CName "signal",
            nameHsIdent = HsIdentifier
              "Signal"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "manual_examples.h"},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Signal",
              newtypeField = HsName
                "@NsVar"
                "un_Signal"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:131:3",
                enumConstantName = NamePair {
                  nameC = CName "start",
                  nameHsIdent = HsIdentifier
                    "Start"},
                enumConstantValue = 1},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:132:3",
                enumConstantName = NamePair {
                  nameC = CName "pause",
                  nameHsIdent = HsIdentifier
                    "Pause"},
                enumConstantValue = 2},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:133:3",
                enumConstantName = NamePair {
                  nameC = CName "resume",
                  nameHsIdent = HsIdentifier
                    "Resume"},
                enumConstantValue = 3},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:134:3",
                enumConstantName = NamePair {
                  nameC = CName "stop",
                  nameHsIdent = HsIdentifier
                    "Stop"},
                enumConstantValue = 4}]},
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
          Read,
          Show,
          Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Signal",
        structConstr = HsName
          "@NsConstr"
          "Signal",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Signal",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
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
                  "Signal",
                structConstr = HsName
                  "@NsConstr"
                  "Signal",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Signal",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
                structInstances = Set.fromList
                  [
                    Eq,
                    Ord,
                    Read,
                    Show,
                    Storable]})
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
                  "Signal",
                structConstr = HsName
                  "@NsConstr"
                  "Signal",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Signal",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
                structInstances = Set.fromList
                  [Eq, Ord, Read, Show, Storable]}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Signal"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Signal"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Signal",
        structConstr = HsName
          "@NsConstr"
          "Signal",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Signal",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_ 1 (NE.fromList ["Start"]),
          _×_ 2 (NE.fromList ["Pause"]),
          _×_ 3 (NE.fromList ["Resume"]),
          _×_ 4 (NE.fromList ["Stop"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Signal",
        structConstr = HsName
          "@NsConstr"
          "Signal",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Signal",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsName "@NsConstr" "Start")
      (HsName "@NsConstr" "Stop")),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Signal",
        structConstr = HsName
          "@NsConstr"
          "Signal",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Signal",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclInstance
    (InstanceCEnumRead
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Signal",
        structConstr = HsName
          "@NsConstr"
          "Signal",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Signal",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Start",
      patSynType = HsName
        "@NsTypeConstr"
        "Signal",
      patSynConstr = HsName
        "@NsConstr"
        "Signal",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:131:3",
          enumConstantName = NamePair {
            nameC = CName "start",
            nameHsIdent = HsIdentifier
              "Start"},
          enumConstantValue = 1}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Pause",
      patSynType = HsName
        "@NsTypeConstr"
        "Signal",
      patSynConstr = HsName
        "@NsConstr"
        "Signal",
      patSynValue = 2,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:132:3",
          enumConstantName = NamePair {
            nameC = CName "pause",
            nameHsIdent = HsIdentifier
              "Pause"},
          enumConstantValue = 2}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Resume",
      patSynType = HsName
        "@NsTypeConstr"
        "Signal",
      patSynConstr = HsName
        "@NsConstr"
        "Signal",
      patSynValue = 3,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:133:3",
          enumConstantName = NamePair {
            nameC = CName "resume",
            nameHsIdent = HsIdentifier
              "Resume"},
          enumConstantValue = 3}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Stop",
      patSynType = HsName
        "@NsTypeConstr"
        "Signal",
      patSynConstr = HsName
        "@NsConstr"
        "Signal",
      patSynValue = 4,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:134:3",
          enumConstantName = NamePair {
            nameC = CName "stop",
            nameHsIdent = HsIdentifier
              "Stop"},
          enumConstantValue = 4}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "HTTP_status",
      newtypeConstr = HsName
        "@NsConstr"
        "HTTP_status",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_HTTP_status",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "manual_examples.h:137:6",
          declId = NamePair {
            nameC = CName "HTTP_status",
            nameHsIdent = HsIdentifier
              "HTTP_status"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "manual_examples.h"},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "HTTP_status",
              newtypeField = HsName
                "@NsVar"
                "un_HTTP_status"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:138:3",
                enumConstantName = NamePair {
                  nameC = CName "ok",
                  nameHsIdent = HsIdentifier
                    "Ok"},
                enumConstantValue = 200},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:139:3",
                enumConstantName = NamePair {
                  nameC = CName "moved",
                  nameHsIdent = HsIdentifier
                    "Moved"},
                enumConstantValue = 301},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:140:3",
                enumConstantName = NamePair {
                  nameC = CName "bad_request",
                  nameHsIdent = HsIdentifier
                    "Bad_request"},
                enumConstantValue = 400},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:141:3",
                enumConstantName = NamePair {
                  nameC = CName "unauthorized",
                  nameHsIdent = HsIdentifier
                    "Unauthorized"},
                enumConstantValue = 401},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:142:3",
                enumConstantName = NamePair {
                  nameC = CName "not_found",
                  nameHsIdent = HsIdentifier
                    "Not_found"},
                enumConstantValue = 404}]},
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
          Read,
          Show,
          Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "HTTP_status",
        structConstr = HsName
          "@NsConstr"
          "HTTP_status",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_HTTP_status",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
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
                  "HTTP_status",
                structConstr = HsName
                  "@NsConstr"
                  "HTTP_status",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_HTTP_status",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
                structInstances = Set.fromList
                  [
                    Eq,
                    Ord,
                    Read,
                    Show,
                    Storable]})
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
                  "HTTP_status",
                structConstr = HsName
                  "@NsConstr"
                  "HTTP_status",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_HTTP_status",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
                structInstances = Set.fromList
                  [Eq, Ord, Read, Show, Storable]}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "HTTP_status"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "HTTP_status"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "HTTP_status",
        structConstr = HsName
          "@NsConstr"
          "HTTP_status",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_HTTP_status",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_ 200 (NE.fromList ["Ok"]),
          _×_ 301 (NE.fromList ["Moved"]),
          _×_
            400
            (NE.fromList ["Bad_request"]),
          _×_
            401
            (NE.fromList ["Unauthorized"]),
          _×_
            404
            (NE.fromList ["Not_found"])])
      False),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "HTTP_status",
        structConstr = HsName
          "@NsConstr"
          "HTTP_status",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_HTTP_status",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclInstance
    (InstanceCEnumRead
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "HTTP_status",
        structConstr = HsName
          "@NsConstr"
          "HTTP_status",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_HTTP_status",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Ok",
      patSynType = HsName
        "@NsTypeConstr"
        "HTTP_status",
      patSynConstr = HsName
        "@NsConstr"
        "HTTP_status",
      patSynValue = 200,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:138:3",
          enumConstantName = NamePair {
            nameC = CName "ok",
            nameHsIdent = HsIdentifier
              "Ok"},
          enumConstantValue = 200}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Moved",
      patSynType = HsName
        "@NsTypeConstr"
        "HTTP_status",
      patSynConstr = HsName
        "@NsConstr"
        "HTTP_status",
      patSynValue = 301,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:139:3",
          enumConstantName = NamePair {
            nameC = CName "moved",
            nameHsIdent = HsIdentifier
              "Moved"},
          enumConstantValue = 301}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Bad_request",
      patSynType = HsName
        "@NsTypeConstr"
        "HTTP_status",
      patSynConstr = HsName
        "@NsConstr"
        "HTTP_status",
      patSynValue = 400,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:140:3",
          enumConstantName = NamePair {
            nameC = CName "bad_request",
            nameHsIdent = HsIdentifier
              "Bad_request"},
          enumConstantValue = 400}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Unauthorized",
      patSynType = HsName
        "@NsTypeConstr"
        "HTTP_status",
      patSynConstr = HsName
        "@NsConstr"
        "HTTP_status",
      patSynValue = 401,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:141:3",
          enumConstantName = NamePair {
            nameC = CName "unauthorized",
            nameHsIdent = HsIdentifier
              "Unauthorized"},
          enumConstantValue = 401}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Not_found",
      patSynType = HsName
        "@NsTypeConstr"
        "HTTP_status",
      patSynConstr = HsName
        "@NsConstr"
        "HTTP_status",
      patSynValue = 404,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:142:3",
          enumConstantName = NamePair {
            nameC = CName "not_found",
            nameHsIdent = HsIdentifier
              "Not_found"},
          enumConstantValue = 404}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Descending",
      newtypeConstr = HsName
        "@NsConstr"
        "Descending",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Descending",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "manual_examples.h:145:6",
          declId = NamePair {
            nameC = CName "descending",
            nameHsIdent = HsIdentifier
              "Descending"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "manual_examples.h"},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Descending",
              newtypeField = HsName
                "@NsVar"
                "un_Descending"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:146:3",
                enumConstantName = NamePair {
                  nameC = CName "X",
                  nameHsIdent = HsIdentifier "X"},
                enumConstantValue = 100},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:147:3",
                enumConstantName = NamePair {
                  nameC = CName "Y",
                  nameHsIdent = HsIdentifier "Y"},
                enumConstantValue = 99},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:148:3",
                enumConstantName = NamePair {
                  nameC = CName "Y_alias",
                  nameHsIdent = HsIdentifier
                    "Y_alias"},
                enumConstantValue = 99},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:149:3",
                enumConstantName = NamePair {
                  nameC = CName "Z",
                  nameHsIdent = HsIdentifier "Z"},
                enumConstantValue = 98}]},
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
          Read,
          Show,
          Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Descending",
        structConstr = HsName
          "@NsConstr"
          "Descending",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Descending",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
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
                  "Descending",
                structConstr = HsName
                  "@NsConstr"
                  "Descending",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Descending",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
                structInstances = Set.fromList
                  [
                    Eq,
                    Ord,
                    Read,
                    Show,
                    Storable]})
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
                  "Descending",
                structConstr = HsName
                  "@NsConstr"
                  "Descending",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Descending",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
                structInstances = Set.fromList
                  [Eq, Ord, Read, Show, Storable]}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Descending"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Descending"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Descending",
        structConstr = HsName
          "@NsConstr"
          "Descending",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Descending",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_ 98 (NE.fromList ["Z"]),
          _×_
            99
            (NE.fromList ["Y", "Y_alias"]),
          _×_ 100 (NE.fromList ["X"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Descending",
        structConstr = HsName
          "@NsConstr"
          "Descending",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Descending",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsName "@NsConstr" "Z")
      (HsName "@NsConstr" "X")),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Descending",
        structConstr = HsName
          "@NsConstr"
          "Descending",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Descending",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclInstance
    (InstanceCEnumRead
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Descending",
        structConstr = HsName
          "@NsConstr"
          "Descending",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Descending",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "X",
      patSynType = HsName
        "@NsTypeConstr"
        "Descending",
      patSynConstr = HsName
        "@NsConstr"
        "Descending",
      patSynValue = 100,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:146:3",
          enumConstantName = NamePair {
            nameC = CName "X",
            nameHsIdent = HsIdentifier "X"},
          enumConstantValue = 100}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Y",
      patSynType = HsName
        "@NsTypeConstr"
        "Descending",
      patSynConstr = HsName
        "@NsConstr"
        "Descending",
      patSynValue = 99,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:147:3",
          enumConstantName = NamePair {
            nameC = CName "Y",
            nameHsIdent = HsIdentifier "Y"},
          enumConstantValue = 99}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Y_alias",
      patSynType = HsName
        "@NsTypeConstr"
        "Descending",
      patSynConstr = HsName
        "@NsConstr"
        "Descending",
      patSynValue = 99,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:148:3",
          enumConstantName = NamePair {
            nameC = CName "Y_alias",
            nameHsIdent = HsIdentifier
              "Y_alias"},
          enumConstantValue = 99}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Z",
      patSynType = HsName
        "@NsTypeConstr"
        "Descending",
      patSynConstr = HsName
        "@NsConstr"
        "Descending",
      patSynValue = 98,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:149:3",
          enumConstantName = NamePair {
            nameC = CName "Z",
            nameHsIdent = HsIdentifier "Z"},
          enumConstantValue = 98}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Result",
      newtypeConstr = HsName
        "@NsConstr"
        "Result",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Result",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "manual_examples.h:152:6",
          declId = NamePair {
            nameC = CName "result",
            nameHsIdent = HsIdentifier
              "Result"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "manual_examples.h"},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Result",
              newtypeField = HsName
                "@NsVar"
                "un_Result"},
            enumType = TypePrim
              (PrimIntegral PrimInt Signed),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:153:3",
                enumConstantName = NamePair {
                  nameC = CName "failed",
                  nameHsIdent = HsIdentifier
                    "Failed"},
                enumConstantValue = `-1`},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:154:3",
                enumConstantName = NamePair {
                  nameC = CName "success",
                  nameHsIdent = HsIdentifier
                    "Success"},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:155:3",
                enumConstantName = NamePair {
                  nameC = CName "postponed",
                  nameHsIdent = HsIdentifier
                    "Postponed"},
                enumConstantValue = 1},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:156:3",
                enumConstantName = NamePair {
                  nameC = CName "already_done",
                  nameHsIdent = HsIdentifier
                    "Already_done"},
                enumConstantValue = 2}]},
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
          Read,
          Show,
          Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Result",
        structConstr = HsName
          "@NsConstr"
          "Result",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Result",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
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
                  "Result",
                structConstr = HsName
                  "@NsConstr"
                  "Result",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Result",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
                structInstances = Set.fromList
                  [
                    Eq,
                    Ord,
                    Read,
                    Show,
                    Storable]})
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
                  "Result",
                structConstr = HsName
                  "@NsConstr"
                  "Result",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Result",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
                structInstances = Set.fromList
                  [Eq, Ord, Read, Show, Storable]}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Result"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Result"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Result",
        structConstr = HsName
          "@NsConstr"
          "Result",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Result",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsPrimType HsPrimCInt)
      (Map.fromList
        [
          _×_
            `-1`
            (NE.fromList ["Failed"]),
          _×_ 0 (NE.fromList ["Success"]),
          _×_
            1
            (NE.fromList ["Postponed"]),
          _×_
            2
            (NE.fromList ["Already_done"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Result",
        structConstr = HsName
          "@NsConstr"
          "Result",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Result",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsName "@NsConstr" "Failed")
      (HsName
        "@NsConstr"
        "Already_done")),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Result",
        structConstr = HsName
          "@NsConstr"
          "Result",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Result",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclInstance
    (InstanceCEnumRead
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Result",
        structConstr = HsName
          "@NsConstr"
          "Result",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Result",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Failed",
      patSynType = HsName
        "@NsTypeConstr"
        "Result",
      patSynConstr = HsName
        "@NsConstr"
        "Result",
      patSynValue = `-1`,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:153:3",
          enumConstantName = NamePair {
            nameC = CName "failed",
            nameHsIdent = HsIdentifier
              "Failed"},
          enumConstantValue = `-1`}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Success",
      patSynType = HsName
        "@NsTypeConstr"
        "Result",
      patSynConstr = HsName
        "@NsConstr"
        "Result",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:154:3",
          enumConstantName = NamePair {
            nameC = CName "success",
            nameHsIdent = HsIdentifier
              "Success"},
          enumConstantValue = 0}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Postponed",
      patSynType = HsName
        "@NsTypeConstr"
        "Result",
      patSynConstr = HsName
        "@NsConstr"
        "Result",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:155:3",
          enumConstantName = NamePair {
            nameC = CName "postponed",
            nameHsIdent = HsIdentifier
              "Postponed"},
          enumConstantValue = 1}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Already_done",
      patSynType = HsName
        "@NsTypeConstr"
        "Result",
      patSynConstr = HsName
        "@NsConstr"
        "Result",
      patSynValue = 2,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:156:3",
          enumConstantName = NamePair {
            nameC = CName "already_done",
            nameHsIdent = HsIdentifier
              "Already_done"},
          enumConstantValue = 2}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Vote",
      newtypeConstr = HsName
        "@NsConstr"
        "Vote",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Vote",
        fieldType = HsPrimType
          HsPrimCUChar,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "manual_examples.h:159:6",
          declId = NamePair {
            nameC = CName "vote",
            nameHsIdent = HsIdentifier
              "Vote"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "manual_examples.h"},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Vote",
              newtypeField = HsName
                "@NsVar"
                "un_Vote"},
            enumType = TypePrim
              (PrimChar
                (PrimSignExplicit Unsigned)),
            enumSizeof = 1,
            enumAlignment = 1,
            enumConstants = [
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:160:3",
                enumConstantName = NamePair {
                  nameC = CName "infavour",
                  nameHsIdent = HsIdentifier
                    "Infavour"},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:161:3",
                enumConstantName = NamePair {
                  nameC = CName "against",
                  nameHsIdent = HsIdentifier
                    "Against"},
                enumConstantValue = 1},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:162:3",
                enumConstantName = NamePair {
                  nameC = CName "abstain",
                  nameHsIdent = HsIdentifier
                    "Abstain"},
                enumConstantValue = 2}]},
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
          Read,
          Show,
          Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Vote",
        structConstr = HsName
          "@NsConstr"
          "Vote",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Vote",
            fieldType = HsPrimType
              HsPrimCUChar,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
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
                  "Vote",
                structConstr = HsName
                  "@NsConstr"
                  "Vote",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Vote",
                    fieldType = HsPrimType
                      HsPrimCUChar,
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
                structInstances = Set.fromList
                  [
                    Eq,
                    Ord,
                    Read,
                    Show,
                    Storable]})
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
                  "Vote",
                structConstr = HsName
                  "@NsConstr"
                  "Vote",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Vote",
                    fieldType = HsPrimType
                      HsPrimCUChar,
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
                structInstances = Set.fromList
                  [Eq, Ord, Read, Show, Storable]}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Vote"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "Vote"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Vote",
        structConstr = HsName
          "@NsConstr"
          "Vote",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Vote",
            fieldType = HsPrimType
              HsPrimCUChar,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsPrimType HsPrimCUChar)
      (Map.fromList
        [
          _×_
            0
            (NE.fromList ["Infavour"]),
          _×_ 1 (NE.fromList ["Against"]),
          _×_
            2
            (NE.fromList ["Abstain"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Vote",
        structConstr = HsName
          "@NsConstr"
          "Vote",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Vote",
            fieldType = HsPrimType
              HsPrimCUChar,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsName "@NsConstr" "Infavour")
      (HsName "@NsConstr" "Abstain")),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Vote",
        structConstr = HsName
          "@NsConstr"
          "Vote",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Vote",
            fieldType = HsPrimType
              HsPrimCUChar,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclInstance
    (InstanceCEnumRead
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Vote",
        structConstr = HsName
          "@NsConstr"
          "Vote",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Vote",
            fieldType = HsPrimType
              HsPrimCUChar,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Infavour",
      patSynType = HsName
        "@NsTypeConstr"
        "Vote",
      patSynConstr = HsName
        "@NsConstr"
        "Vote",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:160:3",
          enumConstantName = NamePair {
            nameC = CName "infavour",
            nameHsIdent = HsIdentifier
              "Infavour"},
          enumConstantValue = 0}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Against",
      patSynType = HsName
        "@NsTypeConstr"
        "Vote",
      patSynConstr = HsName
        "@NsConstr"
        "Vote",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:161:3",
          enumConstantName = NamePair {
            nameC = CName "against",
            nameHsIdent = HsIdentifier
              "Against"},
          enumConstantValue = 1}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Abstain",
      patSynType = HsName
        "@NsTypeConstr"
        "Vote",
      patSynConstr = HsName
        "@NsConstr"
        "Vote",
      patSynValue = 2,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:162:3",
          enumConstantName = NamePair {
            nameC = CName "abstain",
            nameHsIdent = HsIdentifier
              "Abstain"},
          enumConstantValue = 2}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      newtypeConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_CXCursorKind",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "manual_examples.h:167:6",
          declId = NamePair {
            nameC = CName "CXCursorKind",
            nameHsIdent = HsIdentifier
              "CXCursorKind"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "manual_examples.h"},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "CXCursorKind",
              newtypeField = HsName
                "@NsVar"
                "un_CXCursorKind"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:168:3",
                enumConstantName = NamePair {
                  nameC = CName
                    "CXCursor_FirstExpr",
                  nameHsIdent = HsIdentifier
                    "CXCursor_FirstExpr"},
                enumConstantValue = 100},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:169:3",
                enumConstantName = NamePair {
                  nameC = CName
                    "CXCursor_UnexposedExpr",
                  nameHsIdent = HsIdentifier
                    "CXCursor_UnexposedExpr"},
                enumConstantValue = 100},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:170:3",
                enumConstantName = NamePair {
                  nameC = CName
                    "CXCursor_DeclRefExpr",
                  nameHsIdent = HsIdentifier
                    "CXCursor_DeclRefExpr"},
                enumConstantValue = 101},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:171:3",
                enumConstantName = NamePair {
                  nameC = CName
                    "CXCursor_MemberRefExpr",
                  nameHsIdent = HsIdentifier
                    "CXCursor_MemberRefExpr"},
                enumConstantValue = 102},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:173:3",
                enumConstantName = NamePair {
                  nameC = CName
                    "CXCursor_PackIndexingExpr",
                  nameHsIdent = HsIdentifier
                    "CXCursor_PackIndexingExpr"},
                enumConstantValue = 156},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:174:3",
                enumConstantName = NamePair {
                  nameC = CName
                    "CXCursor_LastExpr",
                  nameHsIdent = HsIdentifier
                    "CXCursor_LastExpr"},
                enumConstantValue = 156},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:176:3",
                enumConstantName = NamePair {
                  nameC = CName
                    "CXCursor_FirstStmt",
                  nameHsIdent = HsIdentifier
                    "CXCursor_FirstStmt"},
                enumConstantValue = 200},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:177:3",
                enumConstantName = NamePair {
                  nameC = CName
                    "CXCursor_UnexposedStmt",
                  nameHsIdent = HsIdentifier
                    "CXCursor_UnexposedStmt"},
                enumConstantValue = 200},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:178:3",
                enumConstantName = NamePair {
                  nameC = CName
                    "CXCursor_LabelStmt",
                  nameHsIdent = HsIdentifier
                    "CXCursor_LabelStmt"},
                enumConstantValue = 201},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:179:3",
                enumConstantName = NamePair {
                  nameC = CName
                    "CXCursor_CompoundStmt",
                  nameHsIdent = HsIdentifier
                    "CXCursor_CompoundStmt"},
                enumConstantValue = 202},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:181:3",
                enumConstantName = NamePair {
                  nameC = CName
                    "CXCursor_OpenACCUpdateConstruct",
                  nameHsIdent = HsIdentifier
                    "CXCursor_OpenACCUpdateConstruct"},
                enumConstantValue = 331},
              EnumConstant {
                enumConstantLoc =
                "manual_examples.h:182:3",
                enumConstantName = NamePair {
                  nameC = CName
                    "CXCursor_LastStmt",
                  nameHsIdent = HsIdentifier
                    "CXCursor_LastStmt"},
                enumConstantValue = 331}]},
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
          Read,
          Show,
          Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "CXCursorKind",
        structConstr = HsName
          "@NsConstr"
          "CXCursorKind",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_CXCursorKind",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
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
                  "CXCursorKind",
                structConstr = HsName
                  "@NsConstr"
                  "CXCursorKind",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_CXCursorKind",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
                structInstances = Set.fromList
                  [
                    Eq,
                    Ord,
                    Read,
                    Show,
                    Storable]})
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
                  "CXCursorKind",
                structConstr = HsName
                  "@NsConstr"
                  "CXCursorKind",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_CXCursorKind",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
                structInstances = Set.fromList
                  [Eq, Ord, Read, Show, Storable]}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "CXCursorKind"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "CXCursorKind"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "CXCursorKind",
        structConstr = HsName
          "@NsConstr"
          "CXCursorKind",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_CXCursorKind",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_
            100
            (NE.fromList
              [
                "CXCursor_FirstExpr",
                "CXCursor_UnexposedExpr"]),
          _×_
            101
            (NE.fromList
              ["CXCursor_DeclRefExpr"]),
          _×_
            102
            (NE.fromList
              ["CXCursor_MemberRefExpr"]),
          _×_
            156
            (NE.fromList
              [
                "CXCursor_PackIndexingExpr",
                "CXCursor_LastExpr"]),
          _×_
            200
            (NE.fromList
              [
                "CXCursor_FirstStmt",
                "CXCursor_UnexposedStmt"]),
          _×_
            201
            (NE.fromList
              ["CXCursor_LabelStmt"]),
          _×_
            202
            (NE.fromList
              ["CXCursor_CompoundStmt"]),
          _×_
            331
            (NE.fromList
              [
                "CXCursor_OpenACCUpdateConstruct",
                "CXCursor_LastStmt"])])
      False),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "CXCursorKind",
        structConstr = HsName
          "@NsConstr"
          "CXCursorKind",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_CXCursorKind",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclInstance
    (InstanceCEnumRead
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "CXCursorKind",
        structConstr = HsName
          "@NsConstr"
          "CXCursorKind",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_CXCursorKind",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_FirstExpr",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 100,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:168:3",
          enumConstantName = NamePair {
            nameC = CName
              "CXCursor_FirstExpr",
            nameHsIdent = HsIdentifier
              "CXCursor_FirstExpr"},
          enumConstantValue = 100}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_UnexposedExpr",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 100,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:169:3",
          enumConstantName = NamePair {
            nameC = CName
              "CXCursor_UnexposedExpr",
            nameHsIdent = HsIdentifier
              "CXCursor_UnexposedExpr"},
          enumConstantValue = 100}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_DeclRefExpr",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 101,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:170:3",
          enumConstantName = NamePair {
            nameC = CName
              "CXCursor_DeclRefExpr",
            nameHsIdent = HsIdentifier
              "CXCursor_DeclRefExpr"},
          enumConstantValue = 101}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_MemberRefExpr",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 102,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:171:3",
          enumConstantName = NamePair {
            nameC = CName
              "CXCursor_MemberRefExpr",
            nameHsIdent = HsIdentifier
              "CXCursor_MemberRefExpr"},
          enumConstantValue = 102}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_PackIndexingExpr",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 156,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:173:3",
          enumConstantName = NamePair {
            nameC = CName
              "CXCursor_PackIndexingExpr",
            nameHsIdent = HsIdentifier
              "CXCursor_PackIndexingExpr"},
          enumConstantValue = 156}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_LastExpr",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 156,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:174:3",
          enumConstantName = NamePair {
            nameC = CName
              "CXCursor_LastExpr",
            nameHsIdent = HsIdentifier
              "CXCursor_LastExpr"},
          enumConstantValue = 156}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_FirstStmt",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 200,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:176:3",
          enumConstantName = NamePair {
            nameC = CName
              "CXCursor_FirstStmt",
            nameHsIdent = HsIdentifier
              "CXCursor_FirstStmt"},
          enumConstantValue = 200}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_UnexposedStmt",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 200,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:177:3",
          enumConstantName = NamePair {
            nameC = CName
              "CXCursor_UnexposedStmt",
            nameHsIdent = HsIdentifier
              "CXCursor_UnexposedStmt"},
          enumConstantValue = 200}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_LabelStmt",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 201,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:178:3",
          enumConstantName = NamePair {
            nameC = CName
              "CXCursor_LabelStmt",
            nameHsIdent = HsIdentifier
              "CXCursor_LabelStmt"},
          enumConstantValue = 201}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_CompoundStmt",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 202,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:179:3",
          enumConstantName = NamePair {
            nameC = CName
              "CXCursor_CompoundStmt",
            nameHsIdent = HsIdentifier
              "CXCursor_CompoundStmt"},
          enumConstantValue = 202}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_OpenACCUpdateConstruct",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 331,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:181:3",
          enumConstantName = NamePair {
            nameC = CName
              "CXCursor_OpenACCUpdateConstruct",
            nameHsIdent = HsIdentifier
              "CXCursor_OpenACCUpdateConstruct"},
          enumConstantValue = 331}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_LastStmt",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 331,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "manual_examples.h:182:3",
          enumConstantName = NamePair {
            nameC = CName
              "CXCursor_LastStmt",
            nameHsIdent = HsIdentifier
              "CXCursor_LastStmt"},
          enumConstantValue = 331}},
  DeclInlineCInclude
    "manual_examples.h",
  DeclInlineC
    "signed int testmodule_mod_10 (signed int arg1) { return mod_10(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "mod_10",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "testmodule_mod_10",
      foreignImportHeader =
      "manual_examples.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed)],
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)}}]
