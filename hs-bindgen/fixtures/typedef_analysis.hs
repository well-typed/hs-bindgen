[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Struct1_t",
      structConstr = HsName
        "@NsConstr"
        "Struct1_t",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "typedef_analysis.h:7:8",
            declId = NamePair {
              nameC = Name "struct1_t",
              nameHsIdent = HsIdentifier
                "Struct1_t"},
            declOrigin =
            NameOriginRenamedFrom
              (Name "struct1"),
            declAliases = [
              Name "struct1_t"],
            declHeader =
            "typedef_analysis.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Struct1_t"),
              structSizeof = 0,
              structAlignment = 1,
              structFields = [],
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
          "Struct1_t",
        structConstr = HsName
          "@NsConstr"
          "Struct1_t",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "typedef_analysis.h:7:8",
              declId = NamePair {
                nameC = Name "struct1_t",
                nameHsIdent = HsIdentifier
                  "Struct1_t"},
              declOrigin =
              NameOriginRenamedFrom
                (Name "struct1"),
              declAliases = [
                Name "struct1_t"],
              declHeader =
              "typedef_analysis.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "Struct1_t"),
                structSizeof = 0,
                structAlignment = 1,
                structFields = [],
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
        storableSizeOf = 0,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Struct1_t",
                structConstr = HsName
                  "@NsConstr"
                  "Struct1_t",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "typedef_analysis.h:7:8",
                      declId = NamePair {
                        nameC = Name "struct1_t",
                        nameHsIdent = HsIdentifier
                          "Struct1_t"},
                      declOrigin =
                      NameOriginRenamedFrom
                        (Name "struct1"),
                      declAliases = [
                        Name "struct1_t"],
                      declHeader =
                      "typedef_analysis.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Struct1_t"),
                        structSizeof = 0,
                        structAlignment = 1,
                        structFields = [],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]})
            []),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Struct1_t",
                structConstr = HsName
                  "@NsConstr"
                  "Struct1_t",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "typedef_analysis.h:7:8",
                      declId = NamePair {
                        nameC = Name "struct1_t",
                        nameHsIdent = HsIdentifier
                          "Struct1_t"},
                      declOrigin =
                      NameOriginRenamedFrom
                        (Name "struct1"),
                      declAliases = [
                        Name "struct1_t"],
                      declHeader =
                      "typedef_analysis.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Struct1_t"),
                        structSizeof = 0,
                        structAlignment = 1,
                        structFields = [],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]}
              (Add 0)
              (Seq [])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct1_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct1_t"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Struct2_t",
      structConstr = HsName
        "@NsConstr"
        "Struct2_t",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "typedef_analysis.h:11:16",
            declId = NamePair {
              nameC = Name "struct2_t",
              nameHsIdent = HsIdentifier
                "Struct2_t"},
            declOrigin =
            NameOriginRenamedFrom
              (Name "struct2"),
            declAliases = [
              Name "struct2_t"],
            declHeader =
            "typedef_analysis.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Struct2_t"),
              structSizeof = 0,
              structAlignment = 1,
              structFields = [],
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
          "Struct2_t",
        structConstr = HsName
          "@NsConstr"
          "Struct2_t",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "typedef_analysis.h:11:16",
              declId = NamePair {
                nameC = Name "struct2_t",
                nameHsIdent = HsIdentifier
                  "Struct2_t"},
              declOrigin =
              NameOriginRenamedFrom
                (Name "struct2"),
              declAliases = [
                Name "struct2_t"],
              declHeader =
              "typedef_analysis.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "Struct2_t"),
                structSizeof = 0,
                structAlignment = 1,
                structFields = [],
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
        storableSizeOf = 0,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Struct2_t",
                structConstr = HsName
                  "@NsConstr"
                  "Struct2_t",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "typedef_analysis.h:11:16",
                      declId = NamePair {
                        nameC = Name "struct2_t",
                        nameHsIdent = HsIdentifier
                          "Struct2_t"},
                      declOrigin =
                      NameOriginRenamedFrom
                        (Name "struct2"),
                      declAliases = [
                        Name "struct2_t"],
                      declHeader =
                      "typedef_analysis.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Struct2_t"),
                        structSizeof = 0,
                        structAlignment = 1,
                        structFields = [],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]})
            []),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Struct2_t",
                structConstr = HsName
                  "@NsConstr"
                  "Struct2_t",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "typedef_analysis.h:11:16",
                      declId = NamePair {
                        nameC = Name "struct2_t",
                        nameHsIdent = HsIdentifier
                          "Struct2_t"},
                      declOrigin =
                      NameOriginRenamedFrom
                        (Name "struct2"),
                      declAliases = [
                        Name "struct2_t"],
                      declHeader =
                      "typedef_analysis.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Struct2_t"),
                        structSizeof = 0,
                        structAlignment = 1,
                        structFields = [],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]}
              (Add 0)
              (Seq [])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct2_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct2_t"),
  DeclEmpty
    EmptyData {
      emptyDataName = HsName
        "@NsTypeConstr"
        "Struct3_t",
      emptyDataOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "typedef_analysis.h:14:8",
          declId = NamePair {
            nameC = Name "struct3_t",
            nameHsIdent = HsIdentifier
              "Struct3_t"},
          declOrigin =
          NameOriginRenamedFrom
            (Name "struct3"),
          declAliases = [
            Name "struct3_t"],
          declHeader =
          "typedef_analysis.h"},
        declKind = OpaqueStruct,
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}}},
  DeclEmpty
    EmptyData {
      emptyDataName = HsName
        "@NsTypeConstr"
        "Struct4_t",
      emptyDataOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "typedef_analysis.h:18:16",
          declId = NamePair {
            nameC = Name "struct4_t",
            nameHsIdent = HsIdentifier
              "Struct4_t"},
          declOrigin =
          NameOriginRenamedFrom
            (Name "struct4"),
          declAliases = [
            Name "struct4_t"],
          declHeader =
          "typedef_analysis.h"},
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
        "Struct5",
      structConstr = HsName
        "@NsConstr"
        "Struct5",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "typedef_analysis.h:21:8",
            declId = NamePair {
              nameC = Name "struct5",
              nameHsIdent = HsIdentifier
                "Struct5"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "typedef_analysis.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Struct5"),
              structSizeof = 0,
              structAlignment = 1,
              structFields = [],
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
          "Struct5",
        structConstr = HsName
          "@NsConstr"
          "Struct5",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "typedef_analysis.h:21:8",
              declId = NamePair {
                nameC = Name "struct5",
                nameHsIdent = HsIdentifier
                  "Struct5"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader =
              "typedef_analysis.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Struct5"),
                structSizeof = 0,
                structAlignment = 1,
                structFields = [],
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
        storableSizeOf = 0,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Struct5",
                structConstr = HsName
                  "@NsConstr"
                  "Struct5",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "typedef_analysis.h:21:8",
                      declId = NamePair {
                        nameC = Name "struct5",
                        nameHsIdent = HsIdentifier
                          "Struct5"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "typedef_analysis.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Struct5"),
                        structSizeof = 0,
                        structAlignment = 1,
                        structFields = [],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]})
            []),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Struct5",
                structConstr = HsName
                  "@NsConstr"
                  "Struct5",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "typedef_analysis.h:21:8",
                      declId = NamePair {
                        nameC = Name "struct5",
                        nameHsIdent = HsIdentifier
                          "Struct5"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "typedef_analysis.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Struct5"),
                        structSizeof = 0,
                        structAlignment = 1,
                        structFields = [],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]}
              (Add 0)
              (Seq [])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct5"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct5"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Struct5_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Struct5_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Struct5_t",
        fieldType = HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct5")),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "typedef_analysis.h:22:25",
          declId = NamePair {
            nameC = Name "struct5_t",
            nameHsIdent = HsIdentifier
              "Struct5_t"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "typedef_analysis.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Struct5_t",
              newtypeField = HsName
                "@NsVar"
                "un_Struct5_t"},
            typedefType = TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name "struct5",
                  nameHsIdent = HsIdentifier
                    "Struct5"}
                NameOriginInSource)},
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
      "Struct5_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct5_t"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Struct5_t"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct5_t"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Struct6_Deref",
      structConstr = HsName
        "@NsConstr"
        "Struct6_Deref",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "typedef_analysis.h:25:16",
            declId = NamePair {
              nameC = Name "struct6_Deref",
              nameHsIdent = HsIdentifier
                "Struct6_Deref"},
            declOrigin =
            NameOriginRenamedFrom
              (Name "struct6"),
            declAliases = [],
            declHeader =
            "typedef_analysis.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Struct6_Deref"),
              structSizeof = 0,
              structAlignment = 1,
              structFields = [],
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
          "Struct6_Deref",
        structConstr = HsName
          "@NsConstr"
          "Struct6_Deref",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "typedef_analysis.h:25:16",
              declId = NamePair {
                nameC = Name "struct6_Deref",
                nameHsIdent = HsIdentifier
                  "Struct6_Deref"},
              declOrigin =
              NameOriginRenamedFrom
                (Name "struct6"),
              declAliases = [],
              declHeader =
              "typedef_analysis.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "Struct6_Deref"),
                structSizeof = 0,
                structAlignment = 1,
                structFields = [],
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
        storableSizeOf = 0,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Struct6_Deref",
                structConstr = HsName
                  "@NsConstr"
                  "Struct6_Deref",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "typedef_analysis.h:25:16",
                      declId = NamePair {
                        nameC = Name "struct6_Deref",
                        nameHsIdent = HsIdentifier
                          "Struct6_Deref"},
                      declOrigin =
                      NameOriginRenamedFrom
                        (Name "struct6"),
                      declAliases = [],
                      declHeader =
                      "typedef_analysis.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Struct6_Deref"),
                        structSizeof = 0,
                        structAlignment = 1,
                        structFields = [],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]})
            []),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Struct6_Deref",
                structConstr = HsName
                  "@NsConstr"
                  "Struct6_Deref",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "typedef_analysis.h:25:16",
                      declId = NamePair {
                        nameC = Name "struct6_Deref",
                        nameHsIdent = HsIdentifier
                          "Struct6_Deref"},
                      declOrigin =
                      NameOriginRenamedFrom
                        (Name "struct6"),
                      declAliases = [],
                      declHeader =
                      "typedef_analysis.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Struct6_Deref"),
                        structSizeof = 0,
                        structAlignment = 1,
                        structFields = [],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]}
              (Add 0)
              (Seq [])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct6_Deref"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct6_Deref"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Struct6",
      newtypeConstr = HsName
        "@NsConstr"
        "Struct6",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Struct6",
        fieldType = HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct6_Deref")),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "typedef_analysis.h:25:28",
          declId = NamePair {
            nameC = Name "struct6",
            nameHsIdent = HsIdentifier
              "Struct6"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "typedef_analysis.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Struct6",
              newtypeField = HsName
                "@NsVar"
                "un_Struct6"},
            typedefType = TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name "struct6_Deref",
                  nameHsIdent = HsIdentifier
                    "Struct6_Deref"}
                (NameOriginRenamedFrom
                  (Name "struct6")))},
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
      "Struct6"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct6"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Struct6"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct6"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Struct7",
      structConstr = HsName
        "@NsConstr"
        "Struct7",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "typedef_analysis.h:28:8",
            declId = NamePair {
              nameC = Name "struct7",
              nameHsIdent = HsIdentifier
                "Struct7"},
            declOrigin = NameOriginInSource,
            declAliases = [
              Name "struct7a",
              Name "struct7b"],
            declHeader =
            "typedef_analysis.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Struct7"),
              structSizeof = 0,
              structAlignment = 1,
              structFields = [],
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
          "Struct7",
        structConstr = HsName
          "@NsConstr"
          "Struct7",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "typedef_analysis.h:28:8",
              declId = NamePair {
                nameC = Name "struct7",
                nameHsIdent = HsIdentifier
                  "Struct7"},
              declOrigin = NameOriginInSource,
              declAliases = [
                Name "struct7a",
                Name "struct7b"],
              declHeader =
              "typedef_analysis.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Struct7"),
                structSizeof = 0,
                structAlignment = 1,
                structFields = [],
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
        storableSizeOf = 0,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Struct7",
                structConstr = HsName
                  "@NsConstr"
                  "Struct7",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "typedef_analysis.h:28:8",
                      declId = NamePair {
                        nameC = Name "struct7",
                        nameHsIdent = HsIdentifier
                          "Struct7"},
                      declOrigin = NameOriginInSource,
                      declAliases = [
                        Name "struct7a",
                        Name "struct7b"],
                      declHeader =
                      "typedef_analysis.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Struct7"),
                        structSizeof = 0,
                        structAlignment = 1,
                        structFields = [],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]})
            []),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Struct7",
                structConstr = HsName
                  "@NsConstr"
                  "Struct7",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "typedef_analysis.h:28:8",
                      declId = NamePair {
                        nameC = Name "struct7",
                        nameHsIdent = HsIdentifier
                          "Struct7"},
                      declOrigin = NameOriginInSource,
                      declAliases = [
                        Name "struct7a",
                        Name "struct7b"],
                      declHeader =
                      "typedef_analysis.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Struct7"),
                        structSizeof = 0,
                        structAlignment = 1,
                        structFields = [],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]}
              (Add 0)
              (Seq [])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct7"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct7"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Struct7a",
      newtypeConstr = HsName
        "@NsConstr"
        "Struct7a",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Struct7a",
        fieldType = HsTypRef
          (HsName
            "@NsTypeConstr"
            "Struct7"),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "typedef_analysis.h:29:24",
          declId = NamePair {
            nameC = Name "struct7a",
            nameHsIdent = HsIdentifier
              "Struct7a"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "typedef_analysis.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Struct7a",
              newtypeField = HsName
                "@NsVar"
                "un_Struct7a"},
            typedefType = TypeStruct
              NamePair {
                nameC = Name "struct7",
                nameHsIdent = HsIdentifier
                  "Struct7"}
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
      "Struct7a"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct7a"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct7a"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Struct7b",
      newtypeConstr = HsName
        "@NsConstr"
        "Struct7b",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Struct7b",
        fieldType = HsTypRef
          (HsName
            "@NsTypeConstr"
            "Struct7"),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "typedef_analysis.h:30:24",
          declId = NamePair {
            nameC = Name "struct7b",
            nameHsIdent = HsIdentifier
              "Struct7b"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "typedef_analysis.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Struct7b",
              newtypeField = HsName
                "@NsVar"
                "un_Struct7b"},
            typedefType = TypeStruct
              NamePair {
                nameC = Name "struct7",
                nameHsIdent = HsIdentifier
                  "Struct7"}
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
      "Struct7b"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct7b"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct7b"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Struct8",
      structConstr = HsName
        "@NsConstr"
        "Struct8",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "typedef_analysis.h:33:8",
            declId = NamePair {
              nameC = Name "struct8",
              nameHsIdent = HsIdentifier
                "Struct8"},
            declOrigin = NameOriginInSource,
            declAliases = [
              Name "struct8",
              Name "struct8b"],
            declHeader =
            "typedef_analysis.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Struct8"),
              structSizeof = 0,
              structAlignment = 1,
              structFields = [],
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
          "Struct8",
        structConstr = HsName
          "@NsConstr"
          "Struct8",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "typedef_analysis.h:33:8",
              declId = NamePair {
                nameC = Name "struct8",
                nameHsIdent = HsIdentifier
                  "Struct8"},
              declOrigin = NameOriginInSource,
              declAliases = [
                Name "struct8",
                Name "struct8b"],
              declHeader =
              "typedef_analysis.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Struct8"),
                structSizeof = 0,
                structAlignment = 1,
                structFields = [],
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
        storableSizeOf = 0,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Struct8",
                structConstr = HsName
                  "@NsConstr"
                  "Struct8",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "typedef_analysis.h:33:8",
                      declId = NamePair {
                        nameC = Name "struct8",
                        nameHsIdent = HsIdentifier
                          "Struct8"},
                      declOrigin = NameOriginInSource,
                      declAliases = [
                        Name "struct8",
                        Name "struct8b"],
                      declHeader =
                      "typedef_analysis.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Struct8"),
                        structSizeof = 0,
                        structAlignment = 1,
                        structFields = [],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]})
            []),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Struct8",
                structConstr = HsName
                  "@NsConstr"
                  "Struct8",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "typedef_analysis.h:33:8",
                      declId = NamePair {
                        nameC = Name "struct8",
                        nameHsIdent = HsIdentifier
                          "Struct8"},
                      declOrigin = NameOriginInSource,
                      declAliases = [
                        Name "struct8",
                        Name "struct8b"],
                      declHeader =
                      "typedef_analysis.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Struct8"),
                        structSizeof = 0,
                        structAlignment = 1,
                        structFields = [],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]}
              (Add 0)
              (Seq [])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct8"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct8"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Struct8b",
      newtypeConstr = HsName
        "@NsConstr"
        "Struct8b",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Struct8b",
        fieldType = HsTypRef
          (HsName
            "@NsTypeConstr"
            "Struct8"),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "typedef_analysis.h:35:24",
          declId = NamePair {
            nameC = Name "struct8b",
            nameHsIdent = HsIdentifier
              "Struct8b"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "typedef_analysis.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Struct8b",
              newtypeField = HsName
                "@NsVar"
                "un_Struct8b"},
            typedefType = TypeStruct
              NamePair {
                nameC = Name "struct8",
                nameHsIdent = HsIdentifier
                  "Struct8"}
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
      "Struct8b"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct8b"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct8b"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Struct9",
      structConstr = HsName
        "@NsConstr"
        "Struct9",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "typedef_analysis.h:38:8",
            declId = NamePair {
              nameC = Name "struct9",
              nameHsIdent = HsIdentifier
                "Struct9"},
            declOrigin = NameOriginInSource,
            declAliases = [Name "struct9"],
            declHeader =
            "typedef_analysis.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Struct9"),
              structSizeof = 0,
              structAlignment = 1,
              structFields = [],
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
          "Struct9",
        structConstr = HsName
          "@NsConstr"
          "Struct9",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "typedef_analysis.h:38:8",
              declId = NamePair {
                nameC = Name "struct9",
                nameHsIdent = HsIdentifier
                  "Struct9"},
              declOrigin = NameOriginInSource,
              declAliases = [Name "struct9"],
              declHeader =
              "typedef_analysis.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Struct9"),
                structSizeof = 0,
                structAlignment = 1,
                structFields = [],
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
        storableSizeOf = 0,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Struct9",
                structConstr = HsName
                  "@NsConstr"
                  "Struct9",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "typedef_analysis.h:38:8",
                      declId = NamePair {
                        nameC = Name "struct9",
                        nameHsIdent = HsIdentifier
                          "Struct9"},
                      declOrigin = NameOriginInSource,
                      declAliases = [Name "struct9"],
                      declHeader =
                      "typedef_analysis.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Struct9"),
                        structSizeof = 0,
                        structAlignment = 1,
                        structFields = [],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]})
            []),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Struct9",
                structConstr = HsName
                  "@NsConstr"
                  "Struct9",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "typedef_analysis.h:38:8",
                      declId = NamePair {
                        nameC = Name "struct9",
                        nameHsIdent = HsIdentifier
                          "Struct9"},
                      declOrigin = NameOriginInSource,
                      declAliases = [Name "struct9"],
                      declHeader =
                      "typedef_analysis.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Struct9"),
                        structSizeof = 0,
                        structAlignment = 1,
                        structFields = [],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]}
              (Add 0)
              (Seq [])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct9"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct9"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Struct9_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Struct9_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Struct9_t",
        fieldType = HsTypRef
          (HsName
            "@NsTypeConstr"
            "Struct9"),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "typedef_analysis.h:40:17",
          declId = NamePair {
            nameC = Name "struct9_t",
            nameHsIdent = HsIdentifier
              "Struct9_t"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "typedef_analysis.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Struct9_t",
              newtypeField = HsName
                "@NsVar"
                "un_Struct9_t"},
            typedefType = TypeTypedef
              (TypedefSquashed
                (Name "struct9")
                (TypeStruct
                  NamePair {
                    nameC = Name "struct9",
                    nameHsIdent = HsIdentifier
                      "Struct9"}
                  NameOriginInSource))},
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
      "Struct9_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct9_t"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct9_t"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Struct10_t",
      structConstr = HsName
        "@NsConstr"
        "Struct10_t",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "typedef_analysis.h:46:8",
            declId = NamePair {
              nameC = Name "struct10_t",
              nameHsIdent = HsIdentifier
                "Struct10_t"},
            declOrigin =
            NameOriginRenamedFrom
              (Name "struct10"),
            declAliases = [
              Name "struct10_t"],
            declHeader =
            "typedef_analysis.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Struct10_t"),
              structSizeof = 0,
              structAlignment = 1,
              structFields = [],
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
          "Struct10_t",
        structConstr = HsName
          "@NsConstr"
          "Struct10_t",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "typedef_analysis.h:46:8",
              declId = NamePair {
                nameC = Name "struct10_t",
                nameHsIdent = HsIdentifier
                  "Struct10_t"},
              declOrigin =
              NameOriginRenamedFrom
                (Name "struct10"),
              declAliases = [
                Name "struct10_t"],
              declHeader =
              "typedef_analysis.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "Struct10_t"),
                structSizeof = 0,
                structAlignment = 1,
                structFields = [],
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
        storableSizeOf = 0,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Struct10_t",
                structConstr = HsName
                  "@NsConstr"
                  "Struct10_t",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "typedef_analysis.h:46:8",
                      declId = NamePair {
                        nameC = Name "struct10_t",
                        nameHsIdent = HsIdentifier
                          "Struct10_t"},
                      declOrigin =
                      NameOriginRenamedFrom
                        (Name "struct10"),
                      declAliases = [
                        Name "struct10_t"],
                      declHeader =
                      "typedef_analysis.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Struct10_t"),
                        structSizeof = 0,
                        structAlignment = 1,
                        structFields = [],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]})
            []),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Struct10_t",
                structConstr = HsName
                  "@NsConstr"
                  "Struct10_t",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "typedef_analysis.h:46:8",
                      declId = NamePair {
                        nameC = Name "struct10_t",
                        nameHsIdent = HsIdentifier
                          "Struct10_t"},
                      declOrigin =
                      NameOriginRenamedFrom
                        (Name "struct10"),
                      declAliases = [
                        Name "struct10_t"],
                      declHeader =
                      "typedef_analysis.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Struct10_t"),
                        structSizeof = 0,
                        structAlignment = 1,
                        structFields = [],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]}
              (Add 0)
              (Seq [])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct10_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct10_t"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Struct10_t_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Struct10_t_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Struct10_t_t",
        fieldType = HsTypRef
          (HsName
            "@NsTypeConstr"
            "Struct10_t"),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "typedef_analysis.h:48:20",
          declId = NamePair {
            nameC = Name "struct10_t_t",
            nameHsIdent = HsIdentifier
              "Struct10_t_t"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "typedef_analysis.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Struct10_t_t",
              newtypeField = HsName
                "@NsVar"
                "un_Struct10_t_t"},
            typedefType = TypeTypedef
              (TypedefSquashed
                (Name "struct10_t")
                (TypeStruct
                  NamePair {
                    nameC = Name "struct10_t",
                    nameHsIdent = HsIdentifier
                      "Struct10_t"}
                  (NameOriginRenamedFrom
                    (Name "struct10"))))},
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
      "Struct10_t_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct10_t_t"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct10_t_t"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Struct11_t",
      structConstr = HsName
        "@NsConstr"
        "Struct11_t",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "struct11_t_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "typedef_analysis.h:52:7",
              structFieldName = NamePair {
                nameC = Name "x",
                nameHsIdent = HsIdentifier
                  "struct11_t_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "struct11_t_self",
          fieldType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Struct11_t")),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "typedef_analysis.h:53:20",
              structFieldName = NamePair {
                nameC = Name "self",
                nameHsIdent = HsIdentifier
                  "struct11_t_self"},
              structFieldType = TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "struct11_t",
                    nameHsIdent = HsIdentifier
                      "Struct11_t"}
                  (NameOriginRenamedFrom
                    (Name "struct11"))),
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "typedef_analysis.h:51:8",
            declId = NamePair {
              nameC = Name "struct11_t",
              nameHsIdent = HsIdentifier
                "Struct11_t"},
            declOrigin =
            NameOriginRenamedFrom
              (Name "struct11"),
            declAliases = [
              Name "struct11_t"],
            declHeader =
            "typedef_analysis.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Struct11_t"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "typedef_analysis.h:52:7",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "struct11_t_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "typedef_analysis.h:53:20",
                  structFieldName = NamePair {
                    nameC = Name "self",
                    nameHsIdent = HsIdentifier
                      "struct11_t_self"},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "struct11_t",
                        nameHsIdent = HsIdentifier
                          "Struct11_t"}
                      (NameOriginRenamedFrom
                        (Name "struct11"))),
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
          "Struct11_t",
        structConstr = HsName
          "@NsConstr"
          "Struct11_t",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "struct11_t_x",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "typedef_analysis.h:52:7",
                structFieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier
                    "struct11_t_x"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "struct11_t_self",
            fieldType = HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Struct11_t")),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "typedef_analysis.h:53:20",
                structFieldName = NamePair {
                  nameC = Name "self",
                  nameHsIdent = HsIdentifier
                    "struct11_t_self"},
                structFieldType = TypePointer
                  (TypeStruct
                    NamePair {
                      nameC = Name "struct11_t",
                      nameHsIdent = HsIdentifier
                        "Struct11_t"}
                    (NameOriginRenamedFrom
                      (Name "struct11"))),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "typedef_analysis.h:51:8",
              declId = NamePair {
                nameC = Name "struct11_t",
                nameHsIdent = HsIdentifier
                  "Struct11_t"},
              declOrigin =
              NameOriginRenamedFrom
                (Name "struct11"),
              declAliases = [
                Name "struct11_t"],
              declHeader =
              "typedef_analysis.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "Struct11_t"),
                structSizeof = 16,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "typedef_analysis.h:52:7",
                    structFieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "struct11_t_x"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "typedef_analysis.h:53:20",
                    structFieldName = NamePair {
                      nameC = Name "self",
                      nameHsIdent = HsIdentifier
                        "struct11_t_self"},
                    structFieldType = TypePointer
                      (TypeStruct
                        NamePair {
                          nameC = Name "struct11_t",
                          nameHsIdent = HsIdentifier
                            "Struct11_t"}
                        (NameOriginRenamedFrom
                          (Name "struct11"))),
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
                  "Struct11_t",
                structConstr = HsName
                  "@NsConstr"
                  "Struct11_t",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "struct11_t_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:52:7",
                        structFieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "struct11_t_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "struct11_t_self",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Struct11_t")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:53:20",
                        structFieldName = NamePair {
                          nameC = Name "self",
                          nameHsIdent = HsIdentifier
                            "struct11_t_self"},
                        structFieldType = TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = Name "struct11_t",
                              nameHsIdent = HsIdentifier
                                "Struct11_t"}
                            (NameOriginRenamedFrom
                              (Name "struct11"))),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "typedef_analysis.h:51:8",
                      declId = NamePair {
                        nameC = Name "struct11_t",
                        nameHsIdent = HsIdentifier
                          "Struct11_t"},
                      declOrigin =
                      NameOriginRenamedFrom
                        (Name "struct11"),
                      declAliases = [
                        Name "struct11_t"],
                      declHeader =
                      "typedef_analysis.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Struct11_t"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:52:7",
                            structFieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "struct11_t_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:53:20",
                            structFieldName = NamePair {
                              nameC = Name "self",
                              nameHsIdent = HsIdentifier
                                "struct11_t_self"},
                            structFieldType = TypePointer
                              (TypeStruct
                                NamePair {
                                  nameC = Name "struct11_t",
                                  nameHsIdent = HsIdentifier
                                    "Struct11_t"}
                                (NameOriginRenamedFrom
                                  (Name "struct11"))),
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
                  "Struct11_t",
                structConstr = HsName
                  "@NsConstr"
                  "Struct11_t",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "struct11_t_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:52:7",
                        structFieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "struct11_t_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "struct11_t_self",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Struct11_t")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:53:20",
                        structFieldName = NamePair {
                          nameC = Name "self",
                          nameHsIdent = HsIdentifier
                            "struct11_t_self"},
                        structFieldType = TypePointer
                          (TypeStruct
                            NamePair {
                              nameC = Name "struct11_t",
                              nameHsIdent = HsIdentifier
                                "Struct11_t"}
                            (NameOriginRenamedFrom
                              (Name "struct11"))),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "typedef_analysis.h:51:8",
                      declId = NamePair {
                        nameC = Name "struct11_t",
                        nameHsIdent = HsIdentifier
                          "Struct11_t"},
                      declOrigin =
                      NameOriginRenamedFrom
                        (Name "struct11"),
                      declAliases = [
                        Name "struct11_t"],
                      declHeader =
                      "typedef_analysis.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Struct11_t"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:52:7",
                            structFieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "struct11_t_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:53:20",
                            structFieldName = NamePair {
                              nameC = Name "self",
                              nameHsIdent = HsIdentifier
                                "struct11_t_self"},
                            structFieldType = TypePointer
                              (TypeStruct
                                NamePair {
                                  nameC = Name "struct11_t",
                                  nameHsIdent = HsIdentifier
                                    "Struct11_t"}
                                (NameOriginRenamedFrom
                                  (Name "struct11"))),
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
      "Struct11_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct11_t"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Struct12_t",
      structConstr = HsName
        "@NsConstr"
        "Struct12_t",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "struct12_t_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "typedef_analysis.h:61:7",
              structFieldName = NamePair {
                nameC = Name "x",
                nameHsIdent = HsIdentifier
                  "struct12_t_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "struct12_t_self",
          fieldType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Struct12_t")),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "typedef_analysis.h:62:15",
              structFieldName = NamePair {
                nameC = Name "self",
                nameHsIdent = HsIdentifier
                  "struct12_t_self"},
              structFieldType = TypePointer
                (TypeTypedef
                  (TypedefSquashed
                    (Name "struct12_t")
                    (TypeStruct
                      NamePair {
                        nameC = Name "struct12_t",
                        nameHsIdent = HsIdentifier
                          "Struct12_t"}
                      (NameOriginRenamedFrom
                        (Name "struct12"))))),
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "typedef_analysis.h:60:8",
            declId = NamePair {
              nameC = Name "struct12_t",
              nameHsIdent = HsIdentifier
                "Struct12_t"},
            declOrigin =
            NameOriginRenamedFrom
              (Name "struct12"),
            declAliases = [
              Name "struct12_t"],
            declHeader =
            "typedef_analysis.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Struct12_t"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "typedef_analysis.h:61:7",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "struct12_t_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "typedef_analysis.h:62:15",
                  structFieldName = NamePair {
                    nameC = Name "self",
                    nameHsIdent = HsIdentifier
                      "struct12_t_self"},
                  structFieldType = TypePointer
                    (TypeTypedef
                      (TypedefSquashed
                        (Name "struct12_t")
                        (TypeStruct
                          NamePair {
                            nameC = Name "struct12_t",
                            nameHsIdent = HsIdentifier
                              "Struct12_t"}
                          (NameOriginRenamedFrom
                            (Name "struct12"))))),
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
          "Struct12_t",
        structConstr = HsName
          "@NsConstr"
          "Struct12_t",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "struct12_t_x",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "typedef_analysis.h:61:7",
                structFieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier
                    "struct12_t_x"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "struct12_t_self",
            fieldType = HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Struct12_t")),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "typedef_analysis.h:62:15",
                structFieldName = NamePair {
                  nameC = Name "self",
                  nameHsIdent = HsIdentifier
                    "struct12_t_self"},
                structFieldType = TypePointer
                  (TypeTypedef
                    (TypedefSquashed
                      (Name "struct12_t")
                      (TypeStruct
                        NamePair {
                          nameC = Name "struct12_t",
                          nameHsIdent = HsIdentifier
                            "Struct12_t"}
                        (NameOriginRenamedFrom
                          (Name "struct12"))))),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "typedef_analysis.h:60:8",
              declId = NamePair {
                nameC = Name "struct12_t",
                nameHsIdent = HsIdentifier
                  "Struct12_t"},
              declOrigin =
              NameOriginRenamedFrom
                (Name "struct12"),
              declAliases = [
                Name "struct12_t"],
              declHeader =
              "typedef_analysis.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "Struct12_t"),
                structSizeof = 16,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "typedef_analysis.h:61:7",
                    structFieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "struct12_t_x"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "typedef_analysis.h:62:15",
                    structFieldName = NamePair {
                      nameC = Name "self",
                      nameHsIdent = HsIdentifier
                        "struct12_t_self"},
                    structFieldType = TypePointer
                      (TypeTypedef
                        (TypedefSquashed
                          (Name "struct12_t")
                          (TypeStruct
                            NamePair {
                              nameC = Name "struct12_t",
                              nameHsIdent = HsIdentifier
                                "Struct12_t"}
                            (NameOriginRenamedFrom
                              (Name "struct12"))))),
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
                  "Struct12_t",
                structConstr = HsName
                  "@NsConstr"
                  "Struct12_t",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "struct12_t_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:61:7",
                        structFieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "struct12_t_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "struct12_t_self",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Struct12_t")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:62:15",
                        structFieldName = NamePair {
                          nameC = Name "self",
                          nameHsIdent = HsIdentifier
                            "struct12_t_self"},
                        structFieldType = TypePointer
                          (TypeTypedef
                            (TypedefSquashed
                              (Name "struct12_t")
                              (TypeStruct
                                NamePair {
                                  nameC = Name "struct12_t",
                                  nameHsIdent = HsIdentifier
                                    "Struct12_t"}
                                (NameOriginRenamedFrom
                                  (Name "struct12"))))),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "typedef_analysis.h:60:8",
                      declId = NamePair {
                        nameC = Name "struct12_t",
                        nameHsIdent = HsIdentifier
                          "Struct12_t"},
                      declOrigin =
                      NameOriginRenamedFrom
                        (Name "struct12"),
                      declAliases = [
                        Name "struct12_t"],
                      declHeader =
                      "typedef_analysis.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Struct12_t"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:61:7",
                            structFieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "struct12_t_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:62:15",
                            structFieldName = NamePair {
                              nameC = Name "self",
                              nameHsIdent = HsIdentifier
                                "struct12_t_self"},
                            structFieldType = TypePointer
                              (TypeTypedef
                                (TypedefSquashed
                                  (Name "struct12_t")
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "struct12_t",
                                      nameHsIdent = HsIdentifier
                                        "Struct12_t"}
                                    (NameOriginRenamedFrom
                                      (Name "struct12"))))),
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
                  "Struct12_t",
                structConstr = HsName
                  "@NsConstr"
                  "Struct12_t",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "struct12_t_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:61:7",
                        structFieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "struct12_t_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "struct12_t_self",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Struct12_t")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:62:15",
                        structFieldName = NamePair {
                          nameC = Name "self",
                          nameHsIdent = HsIdentifier
                            "struct12_t_self"},
                        structFieldType = TypePointer
                          (TypeTypedef
                            (TypedefSquashed
                              (Name "struct12_t")
                              (TypeStruct
                                NamePair {
                                  nameC = Name "struct12_t",
                                  nameHsIdent = HsIdentifier
                                    "Struct12_t"}
                                (NameOriginRenamedFrom
                                  (Name "struct12"))))),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "typedef_analysis.h:60:8",
                      declId = NamePair {
                        nameC = Name "struct12_t",
                        nameHsIdent = HsIdentifier
                          "Struct12_t"},
                      declOrigin =
                      NameOriginRenamedFrom
                        (Name "struct12"),
                      declAliases = [
                        Name "struct12_t"],
                      declHeader =
                      "typedef_analysis.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Struct12_t"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:61:7",
                            structFieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "struct12_t_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:62:15",
                            structFieldName = NamePair {
                              nameC = Name "self",
                              nameHsIdent = HsIdentifier
                                "struct12_t_self"},
                            structFieldType = TypePointer
                              (TypeTypedef
                                (TypedefSquashed
                                  (Name "struct12_t")
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "struct12_t",
                                      nameHsIdent = HsIdentifier
                                        "Struct12_t"}
                                    (NameOriginRenamedFrom
                                      (Name "struct12"))))),
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
      "Struct12_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct12_t"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Use_sites",
      structConstr = HsName
        "@NsConstr"
        "Use_sites",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "use_sites_useTypedef_struct1_t",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct1_t"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "typedef_analysis.h:68:13",
              structFieldName = NamePair {
                nameC = Name
                  "useTypedef_struct1_t",
                nameHsIdent = HsIdentifier
                  "use_sites_useTypedef_struct1_t"},
              structFieldType = TypeTypedef
                (TypedefSquashed
                  (Name "struct1_t")
                  (TypeStruct
                    NamePair {
                      nameC = Name "struct1_t",
                      nameHsIdent = HsIdentifier
                        "Struct1_t"}
                    (NameOriginRenamedFrom
                      (Name "struct1")))),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "use_sites_useTypedef_struct2_t",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct2_t"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "typedef_analysis.h:71:13",
              structFieldName = NamePair {
                nameC = Name
                  "useTypedef_struct2_t",
                nameHsIdent = HsIdentifier
                  "use_sites_useTypedef_struct2_t"},
              structFieldType = TypeTypedef
                (TypedefSquashed
                  (Name "struct2_t")
                  (TypeStruct
                    NamePair {
                      nameC = Name "struct2_t",
                      nameHsIdent = HsIdentifier
                        "Struct2_t"}
                    (NameOriginRenamedFrom
                      (Name "struct2")))),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "use_sites_useTypedef_struct3_t",
          fieldType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Struct3_t")),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "typedef_analysis.h:74:14",
              structFieldName = NamePair {
                nameC = Name
                  "useTypedef_struct3_t",
                nameHsIdent = HsIdentifier
                  "use_sites_useTypedef_struct3_t"},
              structFieldType = TypePointer
                (TypeTypedef
                  (TypedefSquashed
                    (Name "struct3_t")
                    (TypeStruct
                      NamePair {
                        nameC = Name "struct3_t",
                        nameHsIdent = HsIdentifier
                          "Struct3_t"}
                      (NameOriginRenamedFrom
                        (Name "struct3"))))),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "use_sites_useTypedef_struct4_t",
          fieldType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Struct4_t")),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "typedef_analysis.h:75:14",
              structFieldName = NamePair {
                nameC = Name
                  "useTypedef_struct4_t",
                nameHsIdent = HsIdentifier
                  "use_sites_useTypedef_struct4_t"},
              structFieldType = TypePointer
                (TypeTypedef
                  (TypedefSquashed
                    (Name "struct4_t")
                    (TypeStruct
                      NamePair {
                        nameC = Name "struct4_t",
                        nameHsIdent = HsIdentifier
                          "Struct4_t"}
                      (NameOriginRenamedFrom
                        (Name "struct4"))))),
              structFieldOffset = 64,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "use_sites_useStruct_struct5",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct5"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "typedef_analysis.h:78:18",
              structFieldName = NamePair {
                nameC = Name
                  "useStruct_struct5",
                nameHsIdent = HsIdentifier
                  "use_sites_useStruct_struct5"},
              structFieldType = TypeStruct
                NamePair {
                  nameC = Name "struct5",
                  nameHsIdent = HsIdentifier
                    "Struct5"}
                NameOriginInSource,
              structFieldOffset = 128,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "use_sites_useTypedef_struct5_t",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct5_t"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "typedef_analysis.h:79:13",
              structFieldName = NamePair {
                nameC = Name
                  "useTypedef_struct5_t",
                nameHsIdent = HsIdentifier
                  "use_sites_useTypedef_struct5_t"},
              structFieldType = TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "struct5_t",
                    nameHsIdent = HsIdentifier
                      "Struct5_t"}),
              structFieldOffset = 128,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "use_sites_useStruct_struct6",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct6_Deref"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "typedef_analysis.h:82:18",
              structFieldName = NamePair {
                nameC = Name
                  "useStruct_struct6",
                nameHsIdent = HsIdentifier
                  "use_sites_useStruct_struct6"},
              structFieldType = TypeStruct
                NamePair {
                  nameC = Name "struct6_Deref",
                  nameHsIdent = HsIdentifier
                    "Struct6_Deref"}
                (NameOriginRenamedFrom
                  (Name "struct6")),
              structFieldOffset = 192,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "use_sites_useTypedef_struct6",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct6"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "typedef_analysis.h:83:11",
              structFieldName = NamePair {
                nameC = Name
                  "useTypedef_struct6",
                nameHsIdent = HsIdentifier
                  "use_sites_useTypedef_struct6"},
              structFieldType = TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "struct6",
                    nameHsIdent = HsIdentifier
                      "Struct6"}),
              structFieldOffset = 192,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "use_sites_useTypedef_struct7a",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct7a"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "typedef_analysis.h:86:12",
              structFieldName = NamePair {
                nameC = Name
                  "useTypedef_struct7a",
                nameHsIdent = HsIdentifier
                  "use_sites_useTypedef_struct7a"},
              structFieldType = TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "struct7a",
                    nameHsIdent = HsIdentifier
                      "Struct7a"}),
              structFieldOffset = 256,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "use_sites_useTypedef_struct7b",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct7b"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "typedef_analysis.h:87:12",
              structFieldName = NamePair {
                nameC = Name
                  "useTypedef_struct7b",
                nameHsIdent = HsIdentifier
                  "use_sites_useTypedef_struct7b"},
              structFieldType = TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "struct7b",
                    nameHsIdent = HsIdentifier
                      "Struct7b"}),
              structFieldOffset = 256,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "use_sites_useTypedef_struct8",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct8"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "typedef_analysis.h:91:11",
              structFieldName = NamePair {
                nameC = Name
                  "useTypedef_struct8",
                nameHsIdent = HsIdentifier
                  "use_sites_useTypedef_struct8"},
              structFieldType = TypeTypedef
                (TypedefSquashed
                  (Name "struct8")
                  (TypeStruct
                    NamePair {
                      nameC = Name "struct8",
                      nameHsIdent = HsIdentifier
                        "Struct8"}
                    NameOriginInSource)),
              structFieldOffset = 256,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "use_sites_useTypedef_struct8b",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct8b"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "typedef_analysis.h:92:12",
              structFieldName = NamePair {
                nameC = Name
                  "useTypedef_struct8b",
                nameHsIdent = HsIdentifier
                  "use_sites_useTypedef_struct8b"},
              structFieldType = TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "struct8b",
                    nameHsIdent = HsIdentifier
                      "Struct8b"}),
              structFieldOffset = 256,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "use_sites_useTypedef_struct9",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct9"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "typedef_analysis.h:96:11",
              structFieldName = NamePair {
                nameC = Name
                  "useTypedef_struct9",
                nameHsIdent = HsIdentifier
                  "use_sites_useTypedef_struct9"},
              structFieldType = TypeTypedef
                (TypedefSquashed
                  (Name "struct9")
                  (TypeStruct
                    NamePair {
                      nameC = Name "struct9",
                      nameHsIdent = HsIdentifier
                        "Struct9"}
                    NameOriginInSource)),
              structFieldOffset = 256,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "use_sites_useTypedef_struct9_t",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct9_t"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "typedef_analysis.h:97:13",
              structFieldName = NamePair {
                nameC = Name
                  "useTypedef_struct9_t",
                nameHsIdent = HsIdentifier
                  "use_sites_useTypedef_struct9_t"},
              structFieldType = TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "struct9_t",
                    nameHsIdent = HsIdentifier
                      "Struct9_t"}),
              structFieldOffset = 256,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "use_sites_useTypedef_struct10_t",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct10_t"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "typedef_analysis.h:98:14",
              structFieldName = NamePair {
                nameC = Name
                  "useTypedef_struct10_t",
                nameHsIdent = HsIdentifier
                  "use_sites_useTypedef_struct10_t"},
              structFieldType = TypeTypedef
                (TypedefSquashed
                  (Name "struct10_t")
                  (TypeStruct
                    NamePair {
                      nameC = Name "struct10_t",
                      nameHsIdent = HsIdentifier
                        "Struct10_t"}
                    (NameOriginRenamedFrom
                      (Name "struct10")))),
              structFieldOffset = 256,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "use_sites_useTypedef_struct10_t_t",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct10_t_t"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "typedef_analysis.h:99:16",
              structFieldName = NamePair {
                nameC = Name
                  "useTypedef_struct10_t_t",
                nameHsIdent = HsIdentifier
                  "use_sites_useTypedef_struct10_t_t"},
              structFieldType = TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "struct10_t_t",
                    nameHsIdent = HsIdentifier
                      "Struct10_t_t"}),
              structFieldOffset = 256,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "use_sites_useTypedef_struct11_t",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct11_t"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "typedef_analysis.h:102:14",
              structFieldName = NamePair {
                nameC = Name
                  "useTypedef_struct11_t",
                nameHsIdent = HsIdentifier
                  "use_sites_useTypedef_struct11_t"},
              structFieldType = TypeTypedef
                (TypedefSquashed
                  (Name "struct11_t")
                  (TypeStruct
                    NamePair {
                      nameC = Name "struct11_t",
                      nameHsIdent = HsIdentifier
                        "Struct11_t"}
                    (NameOriginRenamedFrom
                      (Name "struct11")))),
              structFieldOffset = 256,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "use_sites_useTypedef_struct12_t",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct12_t"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "typedef_analysis.h:103:14",
              structFieldName = NamePair {
                nameC = Name
                  "useTypedef_struct12_t",
                nameHsIdent = HsIdentifier
                  "use_sites_useTypedef_struct12_t"},
              structFieldType = TypeTypedef
                (TypedefSquashed
                  (Name "struct12_t")
                  (TypeStruct
                    NamePair {
                      nameC = Name "struct12_t",
                      nameHsIdent = HsIdentifier
                        "Struct12_t"}
                    (NameOriginRenamedFrom
                      (Name "struct12")))),
              structFieldOffset = 384,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "typedef_analysis.h:66:8",
            declId = NamePair {
              nameC = Name "use_sites",
              nameHsIdent = HsIdentifier
                "Use_sites"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "typedef_analysis.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Use_sites"),
              structSizeof = 64,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "typedef_analysis.h:68:13",
                  structFieldName = NamePair {
                    nameC = Name
                      "useTypedef_struct1_t",
                    nameHsIdent = HsIdentifier
                      "use_sites_useTypedef_struct1_t"},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name "struct1_t")
                      (TypeStruct
                        NamePair {
                          nameC = Name "struct1_t",
                          nameHsIdent = HsIdentifier
                            "Struct1_t"}
                        (NameOriginRenamedFrom
                          (Name "struct1")))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "typedef_analysis.h:71:13",
                  structFieldName = NamePair {
                    nameC = Name
                      "useTypedef_struct2_t",
                    nameHsIdent = HsIdentifier
                      "use_sites_useTypedef_struct2_t"},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name "struct2_t")
                      (TypeStruct
                        NamePair {
                          nameC = Name "struct2_t",
                          nameHsIdent = HsIdentifier
                            "Struct2_t"}
                        (NameOriginRenamedFrom
                          (Name "struct2")))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "typedef_analysis.h:74:14",
                  structFieldName = NamePair {
                    nameC = Name
                      "useTypedef_struct3_t",
                    nameHsIdent = HsIdentifier
                      "use_sites_useTypedef_struct3_t"},
                  structFieldType = TypePointer
                    (TypeTypedef
                      (TypedefSquashed
                        (Name "struct3_t")
                        (TypeStruct
                          NamePair {
                            nameC = Name "struct3_t",
                            nameHsIdent = HsIdentifier
                              "Struct3_t"}
                          (NameOriginRenamedFrom
                            (Name "struct3"))))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "typedef_analysis.h:75:14",
                  structFieldName = NamePair {
                    nameC = Name
                      "useTypedef_struct4_t",
                    nameHsIdent = HsIdentifier
                      "use_sites_useTypedef_struct4_t"},
                  structFieldType = TypePointer
                    (TypeTypedef
                      (TypedefSquashed
                        (Name "struct4_t")
                        (TypeStruct
                          NamePair {
                            nameC = Name "struct4_t",
                            nameHsIdent = HsIdentifier
                              "Struct4_t"}
                          (NameOriginRenamedFrom
                            (Name "struct4"))))),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "typedef_analysis.h:78:18",
                  structFieldName = NamePair {
                    nameC = Name
                      "useStruct_struct5",
                    nameHsIdent = HsIdentifier
                      "use_sites_useStruct_struct5"},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "struct5",
                      nameHsIdent = HsIdentifier
                        "Struct5"}
                    NameOriginInSource,
                  structFieldOffset = 128,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "typedef_analysis.h:79:13",
                  structFieldName = NamePair {
                    nameC = Name
                      "useTypedef_struct5_t",
                    nameHsIdent = HsIdentifier
                      "use_sites_useTypedef_struct5_t"},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "struct5_t",
                        nameHsIdent = HsIdentifier
                          "Struct5_t"}),
                  structFieldOffset = 128,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "typedef_analysis.h:82:18",
                  structFieldName = NamePair {
                    nameC = Name
                      "useStruct_struct6",
                    nameHsIdent = HsIdentifier
                      "use_sites_useStruct_struct6"},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "struct6_Deref",
                      nameHsIdent = HsIdentifier
                        "Struct6_Deref"}
                    (NameOriginRenamedFrom
                      (Name "struct6")),
                  structFieldOffset = 192,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "typedef_analysis.h:83:11",
                  structFieldName = NamePair {
                    nameC = Name
                      "useTypedef_struct6",
                    nameHsIdent = HsIdentifier
                      "use_sites_useTypedef_struct6"},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "struct6",
                        nameHsIdent = HsIdentifier
                          "Struct6"}),
                  structFieldOffset = 192,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "typedef_analysis.h:86:12",
                  structFieldName = NamePair {
                    nameC = Name
                      "useTypedef_struct7a",
                    nameHsIdent = HsIdentifier
                      "use_sites_useTypedef_struct7a"},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "struct7a",
                        nameHsIdent = HsIdentifier
                          "Struct7a"}),
                  structFieldOffset = 256,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "typedef_analysis.h:87:12",
                  structFieldName = NamePair {
                    nameC = Name
                      "useTypedef_struct7b",
                    nameHsIdent = HsIdentifier
                      "use_sites_useTypedef_struct7b"},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "struct7b",
                        nameHsIdent = HsIdentifier
                          "Struct7b"}),
                  structFieldOffset = 256,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "typedef_analysis.h:91:11",
                  structFieldName = NamePair {
                    nameC = Name
                      "useTypedef_struct8",
                    nameHsIdent = HsIdentifier
                      "use_sites_useTypedef_struct8"},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name "struct8")
                      (TypeStruct
                        NamePair {
                          nameC = Name "struct8",
                          nameHsIdent = HsIdentifier
                            "Struct8"}
                        NameOriginInSource)),
                  structFieldOffset = 256,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "typedef_analysis.h:92:12",
                  structFieldName = NamePair {
                    nameC = Name
                      "useTypedef_struct8b",
                    nameHsIdent = HsIdentifier
                      "use_sites_useTypedef_struct8b"},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "struct8b",
                        nameHsIdent = HsIdentifier
                          "Struct8b"}),
                  structFieldOffset = 256,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "typedef_analysis.h:96:11",
                  structFieldName = NamePair {
                    nameC = Name
                      "useTypedef_struct9",
                    nameHsIdent = HsIdentifier
                      "use_sites_useTypedef_struct9"},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name "struct9")
                      (TypeStruct
                        NamePair {
                          nameC = Name "struct9",
                          nameHsIdent = HsIdentifier
                            "Struct9"}
                        NameOriginInSource)),
                  structFieldOffset = 256,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "typedef_analysis.h:97:13",
                  structFieldName = NamePair {
                    nameC = Name
                      "useTypedef_struct9_t",
                    nameHsIdent = HsIdentifier
                      "use_sites_useTypedef_struct9_t"},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "struct9_t",
                        nameHsIdent = HsIdentifier
                          "Struct9_t"}),
                  structFieldOffset = 256,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "typedef_analysis.h:98:14",
                  structFieldName = NamePair {
                    nameC = Name
                      "useTypedef_struct10_t",
                    nameHsIdent = HsIdentifier
                      "use_sites_useTypedef_struct10_t"},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name "struct10_t")
                      (TypeStruct
                        NamePair {
                          nameC = Name "struct10_t",
                          nameHsIdent = HsIdentifier
                            "Struct10_t"}
                        (NameOriginRenamedFrom
                          (Name "struct10")))),
                  structFieldOffset = 256,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "typedef_analysis.h:99:16",
                  structFieldName = NamePair {
                    nameC = Name
                      "useTypedef_struct10_t_t",
                    nameHsIdent = HsIdentifier
                      "use_sites_useTypedef_struct10_t_t"},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "struct10_t_t",
                        nameHsIdent = HsIdentifier
                          "Struct10_t_t"}),
                  structFieldOffset = 256,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "typedef_analysis.h:102:14",
                  structFieldName = NamePair {
                    nameC = Name
                      "useTypedef_struct11_t",
                    nameHsIdent = HsIdentifier
                      "use_sites_useTypedef_struct11_t"},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name "struct11_t")
                      (TypeStruct
                        NamePair {
                          nameC = Name "struct11_t",
                          nameHsIdent = HsIdentifier
                            "Struct11_t"}
                        (NameOriginRenamedFrom
                          (Name "struct11")))),
                  structFieldOffset = 256,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "typedef_analysis.h:103:14",
                  structFieldName = NamePair {
                    nameC = Name
                      "useTypedef_struct12_t",
                    nameHsIdent = HsIdentifier
                      "use_sites_useTypedef_struct12_t"},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name "struct12_t")
                      (TypeStruct
                        NamePair {
                          nameC = Name "struct12_t",
                          nameHsIdent = HsIdentifier
                            "Struct12_t"}
                        (NameOriginRenamedFrom
                          (Name "struct12")))),
                  structFieldOffset = 384,
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
          "Use_sites",
        structConstr = HsName
          "@NsConstr"
          "Use_sites",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "use_sites_useTypedef_struct1_t",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Struct1_t"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "typedef_analysis.h:68:13",
                structFieldName = NamePair {
                  nameC = Name
                    "useTypedef_struct1_t",
                  nameHsIdent = HsIdentifier
                    "use_sites_useTypedef_struct1_t"},
                structFieldType = TypeTypedef
                  (TypedefSquashed
                    (Name "struct1_t")
                    (TypeStruct
                      NamePair {
                        nameC = Name "struct1_t",
                        nameHsIdent = HsIdentifier
                          "Struct1_t"}
                      (NameOriginRenamedFrom
                        (Name "struct1")))),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "use_sites_useTypedef_struct2_t",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Struct2_t"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "typedef_analysis.h:71:13",
                structFieldName = NamePair {
                  nameC = Name
                    "useTypedef_struct2_t",
                  nameHsIdent = HsIdentifier
                    "use_sites_useTypedef_struct2_t"},
                structFieldType = TypeTypedef
                  (TypedefSquashed
                    (Name "struct2_t")
                    (TypeStruct
                      NamePair {
                        nameC = Name "struct2_t",
                        nameHsIdent = HsIdentifier
                          "Struct2_t"}
                      (NameOriginRenamedFrom
                        (Name "struct2")))),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "use_sites_useTypedef_struct3_t",
            fieldType = HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Struct3_t")),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "typedef_analysis.h:74:14",
                structFieldName = NamePair {
                  nameC = Name
                    "useTypedef_struct3_t",
                  nameHsIdent = HsIdentifier
                    "use_sites_useTypedef_struct3_t"},
                structFieldType = TypePointer
                  (TypeTypedef
                    (TypedefSquashed
                      (Name "struct3_t")
                      (TypeStruct
                        NamePair {
                          nameC = Name "struct3_t",
                          nameHsIdent = HsIdentifier
                            "Struct3_t"}
                        (NameOriginRenamedFrom
                          (Name "struct3"))))),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "use_sites_useTypedef_struct4_t",
            fieldType = HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Struct4_t")),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "typedef_analysis.h:75:14",
                structFieldName = NamePair {
                  nameC = Name
                    "useTypedef_struct4_t",
                  nameHsIdent = HsIdentifier
                    "use_sites_useTypedef_struct4_t"},
                structFieldType = TypePointer
                  (TypeTypedef
                    (TypedefSquashed
                      (Name "struct4_t")
                      (TypeStruct
                        NamePair {
                          nameC = Name "struct4_t",
                          nameHsIdent = HsIdentifier
                            "Struct4_t"}
                        (NameOriginRenamedFrom
                          (Name "struct4"))))),
                structFieldOffset = 64,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "use_sites_useStruct_struct5",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Struct5"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "typedef_analysis.h:78:18",
                structFieldName = NamePair {
                  nameC = Name
                    "useStruct_struct5",
                  nameHsIdent = HsIdentifier
                    "use_sites_useStruct_struct5"},
                structFieldType = TypeStruct
                  NamePair {
                    nameC = Name "struct5",
                    nameHsIdent = HsIdentifier
                      "Struct5"}
                  NameOriginInSource,
                structFieldOffset = 128,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "use_sites_useTypedef_struct5_t",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Struct5_t"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "typedef_analysis.h:79:13",
                structFieldName = NamePair {
                  nameC = Name
                    "useTypedef_struct5_t",
                  nameHsIdent = HsIdentifier
                    "use_sites_useTypedef_struct5_t"},
                structFieldType = TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "struct5_t",
                      nameHsIdent = HsIdentifier
                        "Struct5_t"}),
                structFieldOffset = 128,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "use_sites_useStruct_struct6",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Struct6_Deref"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "typedef_analysis.h:82:18",
                structFieldName = NamePair {
                  nameC = Name
                    "useStruct_struct6",
                  nameHsIdent = HsIdentifier
                    "use_sites_useStruct_struct6"},
                structFieldType = TypeStruct
                  NamePair {
                    nameC = Name "struct6_Deref",
                    nameHsIdent = HsIdentifier
                      "Struct6_Deref"}
                  (NameOriginRenamedFrom
                    (Name "struct6")),
                structFieldOffset = 192,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "use_sites_useTypedef_struct6",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Struct6"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "typedef_analysis.h:83:11",
                structFieldName = NamePair {
                  nameC = Name
                    "useTypedef_struct6",
                  nameHsIdent = HsIdentifier
                    "use_sites_useTypedef_struct6"},
                structFieldType = TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "struct6",
                      nameHsIdent = HsIdentifier
                        "Struct6"}),
                structFieldOffset = 192,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "use_sites_useTypedef_struct7a",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Struct7a"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "typedef_analysis.h:86:12",
                structFieldName = NamePair {
                  nameC = Name
                    "useTypedef_struct7a",
                  nameHsIdent = HsIdentifier
                    "use_sites_useTypedef_struct7a"},
                structFieldType = TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "struct7a",
                      nameHsIdent = HsIdentifier
                        "Struct7a"}),
                structFieldOffset = 256,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "use_sites_useTypedef_struct7b",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Struct7b"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "typedef_analysis.h:87:12",
                structFieldName = NamePair {
                  nameC = Name
                    "useTypedef_struct7b",
                  nameHsIdent = HsIdentifier
                    "use_sites_useTypedef_struct7b"},
                structFieldType = TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "struct7b",
                      nameHsIdent = HsIdentifier
                        "Struct7b"}),
                structFieldOffset = 256,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "use_sites_useTypedef_struct8",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Struct8"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "typedef_analysis.h:91:11",
                structFieldName = NamePair {
                  nameC = Name
                    "useTypedef_struct8",
                  nameHsIdent = HsIdentifier
                    "use_sites_useTypedef_struct8"},
                structFieldType = TypeTypedef
                  (TypedefSquashed
                    (Name "struct8")
                    (TypeStruct
                      NamePair {
                        nameC = Name "struct8",
                        nameHsIdent = HsIdentifier
                          "Struct8"}
                      NameOriginInSource)),
                structFieldOffset = 256,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "use_sites_useTypedef_struct8b",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Struct8b"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "typedef_analysis.h:92:12",
                structFieldName = NamePair {
                  nameC = Name
                    "useTypedef_struct8b",
                  nameHsIdent = HsIdentifier
                    "use_sites_useTypedef_struct8b"},
                structFieldType = TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "struct8b",
                      nameHsIdent = HsIdentifier
                        "Struct8b"}),
                structFieldOffset = 256,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "use_sites_useTypedef_struct9",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Struct9"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "typedef_analysis.h:96:11",
                structFieldName = NamePair {
                  nameC = Name
                    "useTypedef_struct9",
                  nameHsIdent = HsIdentifier
                    "use_sites_useTypedef_struct9"},
                structFieldType = TypeTypedef
                  (TypedefSquashed
                    (Name "struct9")
                    (TypeStruct
                      NamePair {
                        nameC = Name "struct9",
                        nameHsIdent = HsIdentifier
                          "Struct9"}
                      NameOriginInSource)),
                structFieldOffset = 256,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "use_sites_useTypedef_struct9_t",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Struct9_t"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "typedef_analysis.h:97:13",
                structFieldName = NamePair {
                  nameC = Name
                    "useTypedef_struct9_t",
                  nameHsIdent = HsIdentifier
                    "use_sites_useTypedef_struct9_t"},
                structFieldType = TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "struct9_t",
                      nameHsIdent = HsIdentifier
                        "Struct9_t"}),
                structFieldOffset = 256,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "use_sites_useTypedef_struct10_t",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Struct10_t"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "typedef_analysis.h:98:14",
                structFieldName = NamePair {
                  nameC = Name
                    "useTypedef_struct10_t",
                  nameHsIdent = HsIdentifier
                    "use_sites_useTypedef_struct10_t"},
                structFieldType = TypeTypedef
                  (TypedefSquashed
                    (Name "struct10_t")
                    (TypeStruct
                      NamePair {
                        nameC = Name "struct10_t",
                        nameHsIdent = HsIdentifier
                          "Struct10_t"}
                      (NameOriginRenamedFrom
                        (Name "struct10")))),
                structFieldOffset = 256,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "use_sites_useTypedef_struct10_t_t",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Struct10_t_t"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "typedef_analysis.h:99:16",
                structFieldName = NamePair {
                  nameC = Name
                    "useTypedef_struct10_t_t",
                  nameHsIdent = HsIdentifier
                    "use_sites_useTypedef_struct10_t_t"},
                structFieldType = TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "struct10_t_t",
                      nameHsIdent = HsIdentifier
                        "Struct10_t_t"}),
                structFieldOffset = 256,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "use_sites_useTypedef_struct11_t",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Struct11_t"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "typedef_analysis.h:102:14",
                structFieldName = NamePair {
                  nameC = Name
                    "useTypedef_struct11_t",
                  nameHsIdent = HsIdentifier
                    "use_sites_useTypedef_struct11_t"},
                structFieldType = TypeTypedef
                  (TypedefSquashed
                    (Name "struct11_t")
                    (TypeStruct
                      NamePair {
                        nameC = Name "struct11_t",
                        nameHsIdent = HsIdentifier
                          "Struct11_t"}
                      (NameOriginRenamedFrom
                        (Name "struct11")))),
                structFieldOffset = 256,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "use_sites_useTypedef_struct12_t",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Struct12_t"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "typedef_analysis.h:103:14",
                structFieldName = NamePair {
                  nameC = Name
                    "useTypedef_struct12_t",
                  nameHsIdent = HsIdentifier
                    "use_sites_useTypedef_struct12_t"},
                structFieldType = TypeTypedef
                  (TypedefSquashed
                    (Name "struct12_t")
                    (TypeStruct
                      NamePair {
                        nameC = Name "struct12_t",
                        nameHsIdent = HsIdentifier
                          "Struct12_t"}
                      (NameOriginRenamedFrom
                        (Name "struct12")))),
                structFieldOffset = 384,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "typedef_analysis.h:66:8",
              declId = NamePair {
                nameC = Name "use_sites",
                nameHsIdent = HsIdentifier
                  "Use_sites"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader =
              "typedef_analysis.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "Use_sites"),
                structSizeof = 64,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "typedef_analysis.h:68:13",
                    structFieldName = NamePair {
                      nameC = Name
                        "useTypedef_struct1_t",
                      nameHsIdent = HsIdentifier
                        "use_sites_useTypedef_struct1_t"},
                    structFieldType = TypeTypedef
                      (TypedefSquashed
                        (Name "struct1_t")
                        (TypeStruct
                          NamePair {
                            nameC = Name "struct1_t",
                            nameHsIdent = HsIdentifier
                              "Struct1_t"}
                          (NameOriginRenamedFrom
                            (Name "struct1")))),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "typedef_analysis.h:71:13",
                    structFieldName = NamePair {
                      nameC = Name
                        "useTypedef_struct2_t",
                      nameHsIdent = HsIdentifier
                        "use_sites_useTypedef_struct2_t"},
                    structFieldType = TypeTypedef
                      (TypedefSquashed
                        (Name "struct2_t")
                        (TypeStruct
                          NamePair {
                            nameC = Name "struct2_t",
                            nameHsIdent = HsIdentifier
                              "Struct2_t"}
                          (NameOriginRenamedFrom
                            (Name "struct2")))),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "typedef_analysis.h:74:14",
                    structFieldName = NamePair {
                      nameC = Name
                        "useTypedef_struct3_t",
                      nameHsIdent = HsIdentifier
                        "use_sites_useTypedef_struct3_t"},
                    structFieldType = TypePointer
                      (TypeTypedef
                        (TypedefSquashed
                          (Name "struct3_t")
                          (TypeStruct
                            NamePair {
                              nameC = Name "struct3_t",
                              nameHsIdent = HsIdentifier
                                "Struct3_t"}
                            (NameOriginRenamedFrom
                              (Name "struct3"))))),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "typedef_analysis.h:75:14",
                    structFieldName = NamePair {
                      nameC = Name
                        "useTypedef_struct4_t",
                      nameHsIdent = HsIdentifier
                        "use_sites_useTypedef_struct4_t"},
                    structFieldType = TypePointer
                      (TypeTypedef
                        (TypedefSquashed
                          (Name "struct4_t")
                          (TypeStruct
                            NamePair {
                              nameC = Name "struct4_t",
                              nameHsIdent = HsIdentifier
                                "Struct4_t"}
                            (NameOriginRenamedFrom
                              (Name "struct4"))))),
                    structFieldOffset = 64,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "typedef_analysis.h:78:18",
                    structFieldName = NamePair {
                      nameC = Name
                        "useStruct_struct5",
                      nameHsIdent = HsIdentifier
                        "use_sites_useStruct_struct5"},
                    structFieldType = TypeStruct
                      NamePair {
                        nameC = Name "struct5",
                        nameHsIdent = HsIdentifier
                          "Struct5"}
                      NameOriginInSource,
                    structFieldOffset = 128,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "typedef_analysis.h:79:13",
                    structFieldName = NamePair {
                      nameC = Name
                        "useTypedef_struct5_t",
                      nameHsIdent = HsIdentifier
                        "use_sites_useTypedef_struct5_t"},
                    structFieldType = TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "struct5_t",
                          nameHsIdent = HsIdentifier
                            "Struct5_t"}),
                    structFieldOffset = 128,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "typedef_analysis.h:82:18",
                    structFieldName = NamePair {
                      nameC = Name
                        "useStruct_struct6",
                      nameHsIdent = HsIdentifier
                        "use_sites_useStruct_struct6"},
                    structFieldType = TypeStruct
                      NamePair {
                        nameC = Name "struct6_Deref",
                        nameHsIdent = HsIdentifier
                          "Struct6_Deref"}
                      (NameOriginRenamedFrom
                        (Name "struct6")),
                    structFieldOffset = 192,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "typedef_analysis.h:83:11",
                    structFieldName = NamePair {
                      nameC = Name
                        "useTypedef_struct6",
                      nameHsIdent = HsIdentifier
                        "use_sites_useTypedef_struct6"},
                    structFieldType = TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "struct6",
                          nameHsIdent = HsIdentifier
                            "Struct6"}),
                    structFieldOffset = 192,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "typedef_analysis.h:86:12",
                    structFieldName = NamePair {
                      nameC = Name
                        "useTypedef_struct7a",
                      nameHsIdent = HsIdentifier
                        "use_sites_useTypedef_struct7a"},
                    structFieldType = TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "struct7a",
                          nameHsIdent = HsIdentifier
                            "Struct7a"}),
                    structFieldOffset = 256,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "typedef_analysis.h:87:12",
                    structFieldName = NamePair {
                      nameC = Name
                        "useTypedef_struct7b",
                      nameHsIdent = HsIdentifier
                        "use_sites_useTypedef_struct7b"},
                    structFieldType = TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "struct7b",
                          nameHsIdent = HsIdentifier
                            "Struct7b"}),
                    structFieldOffset = 256,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "typedef_analysis.h:91:11",
                    structFieldName = NamePair {
                      nameC = Name
                        "useTypedef_struct8",
                      nameHsIdent = HsIdentifier
                        "use_sites_useTypedef_struct8"},
                    structFieldType = TypeTypedef
                      (TypedefSquashed
                        (Name "struct8")
                        (TypeStruct
                          NamePair {
                            nameC = Name "struct8",
                            nameHsIdent = HsIdentifier
                              "Struct8"}
                          NameOriginInSource)),
                    structFieldOffset = 256,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "typedef_analysis.h:92:12",
                    structFieldName = NamePair {
                      nameC = Name
                        "useTypedef_struct8b",
                      nameHsIdent = HsIdentifier
                        "use_sites_useTypedef_struct8b"},
                    structFieldType = TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "struct8b",
                          nameHsIdent = HsIdentifier
                            "Struct8b"}),
                    structFieldOffset = 256,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "typedef_analysis.h:96:11",
                    structFieldName = NamePair {
                      nameC = Name
                        "useTypedef_struct9",
                      nameHsIdent = HsIdentifier
                        "use_sites_useTypedef_struct9"},
                    structFieldType = TypeTypedef
                      (TypedefSquashed
                        (Name "struct9")
                        (TypeStruct
                          NamePair {
                            nameC = Name "struct9",
                            nameHsIdent = HsIdentifier
                              "Struct9"}
                          NameOriginInSource)),
                    structFieldOffset = 256,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "typedef_analysis.h:97:13",
                    structFieldName = NamePair {
                      nameC = Name
                        "useTypedef_struct9_t",
                      nameHsIdent = HsIdentifier
                        "use_sites_useTypedef_struct9_t"},
                    structFieldType = TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "struct9_t",
                          nameHsIdent = HsIdentifier
                            "Struct9_t"}),
                    structFieldOffset = 256,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "typedef_analysis.h:98:14",
                    structFieldName = NamePair {
                      nameC = Name
                        "useTypedef_struct10_t",
                      nameHsIdent = HsIdentifier
                        "use_sites_useTypedef_struct10_t"},
                    structFieldType = TypeTypedef
                      (TypedefSquashed
                        (Name "struct10_t")
                        (TypeStruct
                          NamePair {
                            nameC = Name "struct10_t",
                            nameHsIdent = HsIdentifier
                              "Struct10_t"}
                          (NameOriginRenamedFrom
                            (Name "struct10")))),
                    structFieldOffset = 256,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "typedef_analysis.h:99:16",
                    structFieldName = NamePair {
                      nameC = Name
                        "useTypedef_struct10_t_t",
                      nameHsIdent = HsIdentifier
                        "use_sites_useTypedef_struct10_t_t"},
                    structFieldType = TypeTypedef
                      (TypedefRegular
                        NamePair {
                          nameC = Name "struct10_t_t",
                          nameHsIdent = HsIdentifier
                            "Struct10_t_t"}),
                    structFieldOffset = 256,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "typedef_analysis.h:102:14",
                    structFieldName = NamePair {
                      nameC = Name
                        "useTypedef_struct11_t",
                      nameHsIdent = HsIdentifier
                        "use_sites_useTypedef_struct11_t"},
                    structFieldType = TypeTypedef
                      (TypedefSquashed
                        (Name "struct11_t")
                        (TypeStruct
                          NamePair {
                            nameC = Name "struct11_t",
                            nameHsIdent = HsIdentifier
                              "Struct11_t"}
                          (NameOriginRenamedFrom
                            (Name "struct11")))),
                    structFieldOffset = 256,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "typedef_analysis.h:103:14",
                    structFieldName = NamePair {
                      nameC = Name
                        "useTypedef_struct12_t",
                      nameHsIdent = HsIdentifier
                        "use_sites_useTypedef_struct12_t"},
                    structFieldType = TypeTypedef
                      (TypedefSquashed
                        (Name "struct12_t")
                        (TypeStruct
                          NamePair {
                            nameC = Name "struct12_t",
                            nameHsIdent = HsIdentifier
                              "Struct12_t"}
                          (NameOriginRenamedFrom
                            (Name "struct12")))),
                    structFieldOffset = 384,
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
        storableSizeOf = 64,
        storableAlignment = 8,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Use_sites",
                structConstr = HsName
                  "@NsConstr"
                  "Use_sites",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct1_t",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct1_t"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:68:13",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct1_t",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct1_t"},
                        structFieldType = TypeTypedef
                          (TypedefSquashed
                            (Name "struct1_t")
                            (TypeStruct
                              NamePair {
                                nameC = Name "struct1_t",
                                nameHsIdent = HsIdentifier
                                  "Struct1_t"}
                              (NameOriginRenamedFrom
                                (Name "struct1")))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct2_t",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct2_t"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:71:13",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct2_t",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct2_t"},
                        structFieldType = TypeTypedef
                          (TypedefSquashed
                            (Name "struct2_t")
                            (TypeStruct
                              NamePair {
                                nameC = Name "struct2_t",
                                nameHsIdent = HsIdentifier
                                  "Struct2_t"}
                              (NameOriginRenamedFrom
                                (Name "struct2")))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct3_t",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Struct3_t")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:74:14",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct3_t",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct3_t"},
                        structFieldType = TypePointer
                          (TypeTypedef
                            (TypedefSquashed
                              (Name "struct3_t")
                              (TypeStruct
                                NamePair {
                                  nameC = Name "struct3_t",
                                  nameHsIdent = HsIdentifier
                                    "Struct3_t"}
                                (NameOriginRenamedFrom
                                  (Name "struct3"))))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct4_t",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Struct4_t")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:75:14",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct4_t",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct4_t"},
                        structFieldType = TypePointer
                          (TypeTypedef
                            (TypedefSquashed
                              (Name "struct4_t")
                              (TypeStruct
                                NamePair {
                                  nameC = Name "struct4_t",
                                  nameHsIdent = HsIdentifier
                                    "Struct4_t"}
                                (NameOriginRenamedFrom
                                  (Name "struct4"))))),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useStruct_struct5",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct5"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:78:18",
                        structFieldName = NamePair {
                          nameC = Name
                            "useStruct_struct5",
                          nameHsIdent = HsIdentifier
                            "use_sites_useStruct_struct5"},
                        structFieldType = TypeStruct
                          NamePair {
                            nameC = Name "struct5",
                            nameHsIdent = HsIdentifier
                              "Struct5"}
                          NameOriginInSource,
                        structFieldOffset = 128,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct5_t",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct5_t"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:79:13",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct5_t",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct5_t"},
                        structFieldType = TypeTypedef
                          (TypedefRegular
                            NamePair {
                              nameC = Name "struct5_t",
                              nameHsIdent = HsIdentifier
                                "Struct5_t"}),
                        structFieldOffset = 128,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useStruct_struct6",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct6_Deref"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:82:18",
                        structFieldName = NamePair {
                          nameC = Name
                            "useStruct_struct6",
                          nameHsIdent = HsIdentifier
                            "use_sites_useStruct_struct6"},
                        structFieldType = TypeStruct
                          NamePair {
                            nameC = Name "struct6_Deref",
                            nameHsIdent = HsIdentifier
                              "Struct6_Deref"}
                          (NameOriginRenamedFrom
                            (Name "struct6")),
                        structFieldOffset = 192,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct6",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct6"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:83:11",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct6",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct6"},
                        structFieldType = TypeTypedef
                          (TypedefRegular
                            NamePair {
                              nameC = Name "struct6",
                              nameHsIdent = HsIdentifier
                                "Struct6"}),
                        structFieldOffset = 192,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct7a",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct7a"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:86:12",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct7a",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct7a"},
                        structFieldType = TypeTypedef
                          (TypedefRegular
                            NamePair {
                              nameC = Name "struct7a",
                              nameHsIdent = HsIdentifier
                                "Struct7a"}),
                        structFieldOffset = 256,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct7b",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct7b"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:87:12",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct7b",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct7b"},
                        structFieldType = TypeTypedef
                          (TypedefRegular
                            NamePair {
                              nameC = Name "struct7b",
                              nameHsIdent = HsIdentifier
                                "Struct7b"}),
                        structFieldOffset = 256,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct8",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct8"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:91:11",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct8",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct8"},
                        structFieldType = TypeTypedef
                          (TypedefSquashed
                            (Name "struct8")
                            (TypeStruct
                              NamePair {
                                nameC = Name "struct8",
                                nameHsIdent = HsIdentifier
                                  "Struct8"}
                              NameOriginInSource)),
                        structFieldOffset = 256,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct8b",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct8b"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:92:12",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct8b",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct8b"},
                        structFieldType = TypeTypedef
                          (TypedefRegular
                            NamePair {
                              nameC = Name "struct8b",
                              nameHsIdent = HsIdentifier
                                "Struct8b"}),
                        structFieldOffset = 256,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct9",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct9"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:96:11",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct9",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct9"},
                        structFieldType = TypeTypedef
                          (TypedefSquashed
                            (Name "struct9")
                            (TypeStruct
                              NamePair {
                                nameC = Name "struct9",
                                nameHsIdent = HsIdentifier
                                  "Struct9"}
                              NameOriginInSource)),
                        structFieldOffset = 256,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct9_t",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct9_t"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:97:13",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct9_t",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct9_t"},
                        structFieldType = TypeTypedef
                          (TypedefRegular
                            NamePair {
                              nameC = Name "struct9_t",
                              nameHsIdent = HsIdentifier
                                "Struct9_t"}),
                        structFieldOffset = 256,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct10_t",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct10_t"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:98:14",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct10_t",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct10_t"},
                        structFieldType = TypeTypedef
                          (TypedefSquashed
                            (Name "struct10_t")
                            (TypeStruct
                              NamePair {
                                nameC = Name "struct10_t",
                                nameHsIdent = HsIdentifier
                                  "Struct10_t"}
                              (NameOriginRenamedFrom
                                (Name "struct10")))),
                        structFieldOffset = 256,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct10_t_t",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct10_t_t"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:99:16",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct10_t_t",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct10_t_t"},
                        structFieldType = TypeTypedef
                          (TypedefRegular
                            NamePair {
                              nameC = Name "struct10_t_t",
                              nameHsIdent = HsIdentifier
                                "Struct10_t_t"}),
                        structFieldOffset = 256,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct11_t",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct11_t"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:102:14",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct11_t",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct11_t"},
                        structFieldType = TypeTypedef
                          (TypedefSquashed
                            (Name "struct11_t")
                            (TypeStruct
                              NamePair {
                                nameC = Name "struct11_t",
                                nameHsIdent = HsIdentifier
                                  "Struct11_t"}
                              (NameOriginRenamedFrom
                                (Name "struct11")))),
                        structFieldOffset = 256,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct12_t",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct12_t"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:103:14",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct12_t",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct12_t"},
                        structFieldType = TypeTypedef
                          (TypedefSquashed
                            (Name "struct12_t")
                            (TypeStruct
                              NamePair {
                                nameC = Name "struct12_t",
                                nameHsIdent = HsIdentifier
                                  "Struct12_t"}
                              (NameOriginRenamedFrom
                                (Name "struct12")))),
                        structFieldOffset = 384,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "typedef_analysis.h:66:8",
                      declId = NamePair {
                        nameC = Name "use_sites",
                        nameHsIdent = HsIdentifier
                          "Use_sites"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "typedef_analysis.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Use_sites"),
                        structSizeof = 64,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:68:13",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct1_t",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct1_t"},
                            structFieldType = TypeTypedef
                              (TypedefSquashed
                                (Name "struct1_t")
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "struct1_t",
                                    nameHsIdent = HsIdentifier
                                      "Struct1_t"}
                                  (NameOriginRenamedFrom
                                    (Name "struct1")))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:71:13",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct2_t",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct2_t"},
                            structFieldType = TypeTypedef
                              (TypedefSquashed
                                (Name "struct2_t")
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "struct2_t",
                                    nameHsIdent = HsIdentifier
                                      "Struct2_t"}
                                  (NameOriginRenamedFrom
                                    (Name "struct2")))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:74:14",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct3_t",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct3_t"},
                            structFieldType = TypePointer
                              (TypeTypedef
                                (TypedefSquashed
                                  (Name "struct3_t")
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "struct3_t",
                                      nameHsIdent = HsIdentifier
                                        "Struct3_t"}
                                    (NameOriginRenamedFrom
                                      (Name "struct3"))))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:75:14",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct4_t",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct4_t"},
                            structFieldType = TypePointer
                              (TypeTypedef
                                (TypedefSquashed
                                  (Name "struct4_t")
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "struct4_t",
                                      nameHsIdent = HsIdentifier
                                        "Struct4_t"}
                                    (NameOriginRenamedFrom
                                      (Name "struct4"))))),
                            structFieldOffset = 64,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:78:18",
                            structFieldName = NamePair {
                              nameC = Name
                                "useStruct_struct5",
                              nameHsIdent = HsIdentifier
                                "use_sites_useStruct_struct5"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = Name "struct5",
                                nameHsIdent = HsIdentifier
                                  "Struct5"}
                              NameOriginInSource,
                            structFieldOffset = 128,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:79:13",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct5_t",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct5_t"},
                            structFieldType = TypeTypedef
                              (TypedefRegular
                                NamePair {
                                  nameC = Name "struct5_t",
                                  nameHsIdent = HsIdentifier
                                    "Struct5_t"}),
                            structFieldOffset = 128,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:82:18",
                            structFieldName = NamePair {
                              nameC = Name
                                "useStruct_struct6",
                              nameHsIdent = HsIdentifier
                                "use_sites_useStruct_struct6"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = Name "struct6_Deref",
                                nameHsIdent = HsIdentifier
                                  "Struct6_Deref"}
                              (NameOriginRenamedFrom
                                (Name "struct6")),
                            structFieldOffset = 192,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:83:11",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct6",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct6"},
                            structFieldType = TypeTypedef
                              (TypedefRegular
                                NamePair {
                                  nameC = Name "struct6",
                                  nameHsIdent = HsIdentifier
                                    "Struct6"}),
                            structFieldOffset = 192,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:86:12",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct7a",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct7a"},
                            structFieldType = TypeTypedef
                              (TypedefRegular
                                NamePair {
                                  nameC = Name "struct7a",
                                  nameHsIdent = HsIdentifier
                                    "Struct7a"}),
                            structFieldOffset = 256,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:87:12",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct7b",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct7b"},
                            structFieldType = TypeTypedef
                              (TypedefRegular
                                NamePair {
                                  nameC = Name "struct7b",
                                  nameHsIdent = HsIdentifier
                                    "Struct7b"}),
                            structFieldOffset = 256,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:91:11",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct8",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct8"},
                            structFieldType = TypeTypedef
                              (TypedefSquashed
                                (Name "struct8")
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "struct8",
                                    nameHsIdent = HsIdentifier
                                      "Struct8"}
                                  NameOriginInSource)),
                            structFieldOffset = 256,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:92:12",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct8b",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct8b"},
                            structFieldType = TypeTypedef
                              (TypedefRegular
                                NamePair {
                                  nameC = Name "struct8b",
                                  nameHsIdent = HsIdentifier
                                    "Struct8b"}),
                            structFieldOffset = 256,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:96:11",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct9",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct9"},
                            structFieldType = TypeTypedef
                              (TypedefSquashed
                                (Name "struct9")
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "struct9",
                                    nameHsIdent = HsIdentifier
                                      "Struct9"}
                                  NameOriginInSource)),
                            structFieldOffset = 256,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:97:13",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct9_t",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct9_t"},
                            structFieldType = TypeTypedef
                              (TypedefRegular
                                NamePair {
                                  nameC = Name "struct9_t",
                                  nameHsIdent = HsIdentifier
                                    "Struct9_t"}),
                            structFieldOffset = 256,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:98:14",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct10_t",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct10_t"},
                            structFieldType = TypeTypedef
                              (TypedefSquashed
                                (Name "struct10_t")
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "struct10_t",
                                    nameHsIdent = HsIdentifier
                                      "Struct10_t"}
                                  (NameOriginRenamedFrom
                                    (Name "struct10")))),
                            structFieldOffset = 256,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:99:16",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct10_t_t",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct10_t_t"},
                            structFieldType = TypeTypedef
                              (TypedefRegular
                                NamePair {
                                  nameC = Name "struct10_t_t",
                                  nameHsIdent = HsIdentifier
                                    "Struct10_t_t"}),
                            structFieldOffset = 256,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:102:14",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct11_t",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct11_t"},
                            structFieldType = TypeTypedef
                              (TypedefSquashed
                                (Name "struct11_t")
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "struct11_t",
                                    nameHsIdent = HsIdentifier
                                      "Struct11_t"}
                                  (NameOriginRenamedFrom
                                    (Name "struct11")))),
                            structFieldOffset = 256,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:103:14",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct12_t",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct12_t"},
                            structFieldType = TypeTypedef
                              (TypedefSquashed
                                (Name "struct12_t")
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "struct12_t",
                                    nameHsIdent = HsIdentifier
                                      "Struct12_t"}
                                  (NameOriginRenamedFrom
                                    (Name "struct12")))),
                            structFieldOffset = 384,
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
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 8,
              PeekByteOff (Idx 0) 16,
              PeekByteOff (Idx 0) 16,
              PeekByteOff (Idx 0) 24,
              PeekByteOff (Idx 0) 24,
              PeekByteOff (Idx 0) 32,
              PeekByteOff (Idx 0) 32,
              PeekByteOff (Idx 0) 32,
              PeekByteOff (Idx 0) 32,
              PeekByteOff (Idx 0) 32,
              PeekByteOff (Idx 0) 32,
              PeekByteOff (Idx 0) 32,
              PeekByteOff (Idx 0) 32,
              PeekByteOff (Idx 0) 32,
              PeekByteOff (Idx 0) 48]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Use_sites",
                structConstr = HsName
                  "@NsConstr"
                  "Use_sites",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct1_t",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct1_t"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:68:13",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct1_t",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct1_t"},
                        structFieldType = TypeTypedef
                          (TypedefSquashed
                            (Name "struct1_t")
                            (TypeStruct
                              NamePair {
                                nameC = Name "struct1_t",
                                nameHsIdent = HsIdentifier
                                  "Struct1_t"}
                              (NameOriginRenamedFrom
                                (Name "struct1")))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct2_t",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct2_t"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:71:13",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct2_t",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct2_t"},
                        structFieldType = TypeTypedef
                          (TypedefSquashed
                            (Name "struct2_t")
                            (TypeStruct
                              NamePair {
                                nameC = Name "struct2_t",
                                nameHsIdent = HsIdentifier
                                  "Struct2_t"}
                              (NameOriginRenamedFrom
                                (Name "struct2")))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct3_t",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Struct3_t")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:74:14",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct3_t",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct3_t"},
                        structFieldType = TypePointer
                          (TypeTypedef
                            (TypedefSquashed
                              (Name "struct3_t")
                              (TypeStruct
                                NamePair {
                                  nameC = Name "struct3_t",
                                  nameHsIdent = HsIdentifier
                                    "Struct3_t"}
                                (NameOriginRenamedFrom
                                  (Name "struct3"))))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct4_t",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Struct4_t")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:75:14",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct4_t",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct4_t"},
                        structFieldType = TypePointer
                          (TypeTypedef
                            (TypedefSquashed
                              (Name "struct4_t")
                              (TypeStruct
                                NamePair {
                                  nameC = Name "struct4_t",
                                  nameHsIdent = HsIdentifier
                                    "Struct4_t"}
                                (NameOriginRenamedFrom
                                  (Name "struct4"))))),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useStruct_struct5",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct5"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:78:18",
                        structFieldName = NamePair {
                          nameC = Name
                            "useStruct_struct5",
                          nameHsIdent = HsIdentifier
                            "use_sites_useStruct_struct5"},
                        structFieldType = TypeStruct
                          NamePair {
                            nameC = Name "struct5",
                            nameHsIdent = HsIdentifier
                              "Struct5"}
                          NameOriginInSource,
                        structFieldOffset = 128,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct5_t",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct5_t"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:79:13",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct5_t",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct5_t"},
                        structFieldType = TypeTypedef
                          (TypedefRegular
                            NamePair {
                              nameC = Name "struct5_t",
                              nameHsIdent = HsIdentifier
                                "Struct5_t"}),
                        structFieldOffset = 128,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useStruct_struct6",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct6_Deref"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:82:18",
                        structFieldName = NamePair {
                          nameC = Name
                            "useStruct_struct6",
                          nameHsIdent = HsIdentifier
                            "use_sites_useStruct_struct6"},
                        structFieldType = TypeStruct
                          NamePair {
                            nameC = Name "struct6_Deref",
                            nameHsIdent = HsIdentifier
                              "Struct6_Deref"}
                          (NameOriginRenamedFrom
                            (Name "struct6")),
                        structFieldOffset = 192,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct6",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct6"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:83:11",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct6",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct6"},
                        structFieldType = TypeTypedef
                          (TypedefRegular
                            NamePair {
                              nameC = Name "struct6",
                              nameHsIdent = HsIdentifier
                                "Struct6"}),
                        structFieldOffset = 192,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct7a",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct7a"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:86:12",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct7a",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct7a"},
                        structFieldType = TypeTypedef
                          (TypedefRegular
                            NamePair {
                              nameC = Name "struct7a",
                              nameHsIdent = HsIdentifier
                                "Struct7a"}),
                        structFieldOffset = 256,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct7b",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct7b"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:87:12",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct7b",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct7b"},
                        structFieldType = TypeTypedef
                          (TypedefRegular
                            NamePair {
                              nameC = Name "struct7b",
                              nameHsIdent = HsIdentifier
                                "Struct7b"}),
                        structFieldOffset = 256,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct8",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct8"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:91:11",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct8",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct8"},
                        structFieldType = TypeTypedef
                          (TypedefSquashed
                            (Name "struct8")
                            (TypeStruct
                              NamePair {
                                nameC = Name "struct8",
                                nameHsIdent = HsIdentifier
                                  "Struct8"}
                              NameOriginInSource)),
                        structFieldOffset = 256,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct8b",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct8b"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:92:12",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct8b",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct8b"},
                        structFieldType = TypeTypedef
                          (TypedefRegular
                            NamePair {
                              nameC = Name "struct8b",
                              nameHsIdent = HsIdentifier
                                "Struct8b"}),
                        structFieldOffset = 256,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct9",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct9"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:96:11",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct9",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct9"},
                        structFieldType = TypeTypedef
                          (TypedefSquashed
                            (Name "struct9")
                            (TypeStruct
                              NamePair {
                                nameC = Name "struct9",
                                nameHsIdent = HsIdentifier
                                  "Struct9"}
                              NameOriginInSource)),
                        structFieldOffset = 256,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct9_t",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct9_t"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:97:13",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct9_t",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct9_t"},
                        structFieldType = TypeTypedef
                          (TypedefRegular
                            NamePair {
                              nameC = Name "struct9_t",
                              nameHsIdent = HsIdentifier
                                "Struct9_t"}),
                        structFieldOffset = 256,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct10_t",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct10_t"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:98:14",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct10_t",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct10_t"},
                        structFieldType = TypeTypedef
                          (TypedefSquashed
                            (Name "struct10_t")
                            (TypeStruct
                              NamePair {
                                nameC = Name "struct10_t",
                                nameHsIdent = HsIdentifier
                                  "Struct10_t"}
                              (NameOriginRenamedFrom
                                (Name "struct10")))),
                        structFieldOffset = 256,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct10_t_t",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct10_t_t"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:99:16",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct10_t_t",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct10_t_t"},
                        structFieldType = TypeTypedef
                          (TypedefRegular
                            NamePair {
                              nameC = Name "struct10_t_t",
                              nameHsIdent = HsIdentifier
                                "Struct10_t_t"}),
                        structFieldOffset = 256,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct11_t",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct11_t"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:102:14",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct11_t",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct11_t"},
                        structFieldType = TypeTypedef
                          (TypedefSquashed
                            (Name "struct11_t")
                            (TypeStruct
                              NamePair {
                                nameC = Name "struct11_t",
                                nameHsIdent = HsIdentifier
                                  "Struct11_t"}
                              (NameOriginRenamedFrom
                                (Name "struct11")))),
                        structFieldOffset = 256,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "use_sites_useTypedef_struct12_t",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Struct12_t"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "typedef_analysis.h:103:14",
                        structFieldName = NamePair {
                          nameC = Name
                            "useTypedef_struct12_t",
                          nameHsIdent = HsIdentifier
                            "use_sites_useTypedef_struct12_t"},
                        structFieldType = TypeTypedef
                          (TypedefSquashed
                            (Name "struct12_t")
                            (TypeStruct
                              NamePair {
                                nameC = Name "struct12_t",
                                nameHsIdent = HsIdentifier
                                  "Struct12_t"}
                              (NameOriginRenamedFrom
                                (Name "struct12")))),
                        structFieldOffset = 384,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "typedef_analysis.h:66:8",
                      declId = NamePair {
                        nameC = Name "use_sites",
                        nameHsIdent = HsIdentifier
                          "Use_sites"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "typedef_analysis.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Use_sites"),
                        structSizeof = 64,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:68:13",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct1_t",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct1_t"},
                            structFieldType = TypeTypedef
                              (TypedefSquashed
                                (Name "struct1_t")
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "struct1_t",
                                    nameHsIdent = HsIdentifier
                                      "Struct1_t"}
                                  (NameOriginRenamedFrom
                                    (Name "struct1")))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:71:13",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct2_t",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct2_t"},
                            structFieldType = TypeTypedef
                              (TypedefSquashed
                                (Name "struct2_t")
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "struct2_t",
                                    nameHsIdent = HsIdentifier
                                      "Struct2_t"}
                                  (NameOriginRenamedFrom
                                    (Name "struct2")))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:74:14",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct3_t",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct3_t"},
                            structFieldType = TypePointer
                              (TypeTypedef
                                (TypedefSquashed
                                  (Name "struct3_t")
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "struct3_t",
                                      nameHsIdent = HsIdentifier
                                        "Struct3_t"}
                                    (NameOriginRenamedFrom
                                      (Name "struct3"))))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:75:14",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct4_t",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct4_t"},
                            structFieldType = TypePointer
                              (TypeTypedef
                                (TypedefSquashed
                                  (Name "struct4_t")
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "struct4_t",
                                      nameHsIdent = HsIdentifier
                                        "Struct4_t"}
                                    (NameOriginRenamedFrom
                                      (Name "struct4"))))),
                            structFieldOffset = 64,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:78:18",
                            structFieldName = NamePair {
                              nameC = Name
                                "useStruct_struct5",
                              nameHsIdent = HsIdentifier
                                "use_sites_useStruct_struct5"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = Name "struct5",
                                nameHsIdent = HsIdentifier
                                  "Struct5"}
                              NameOriginInSource,
                            structFieldOffset = 128,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:79:13",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct5_t",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct5_t"},
                            structFieldType = TypeTypedef
                              (TypedefRegular
                                NamePair {
                                  nameC = Name "struct5_t",
                                  nameHsIdent = HsIdentifier
                                    "Struct5_t"}),
                            structFieldOffset = 128,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:82:18",
                            structFieldName = NamePair {
                              nameC = Name
                                "useStruct_struct6",
                              nameHsIdent = HsIdentifier
                                "use_sites_useStruct_struct6"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = Name "struct6_Deref",
                                nameHsIdent = HsIdentifier
                                  "Struct6_Deref"}
                              (NameOriginRenamedFrom
                                (Name "struct6")),
                            structFieldOffset = 192,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:83:11",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct6",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct6"},
                            structFieldType = TypeTypedef
                              (TypedefRegular
                                NamePair {
                                  nameC = Name "struct6",
                                  nameHsIdent = HsIdentifier
                                    "Struct6"}),
                            structFieldOffset = 192,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:86:12",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct7a",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct7a"},
                            structFieldType = TypeTypedef
                              (TypedefRegular
                                NamePair {
                                  nameC = Name "struct7a",
                                  nameHsIdent = HsIdentifier
                                    "Struct7a"}),
                            structFieldOffset = 256,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:87:12",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct7b",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct7b"},
                            structFieldType = TypeTypedef
                              (TypedefRegular
                                NamePair {
                                  nameC = Name "struct7b",
                                  nameHsIdent = HsIdentifier
                                    "Struct7b"}),
                            structFieldOffset = 256,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:91:11",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct8",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct8"},
                            structFieldType = TypeTypedef
                              (TypedefSquashed
                                (Name "struct8")
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "struct8",
                                    nameHsIdent = HsIdentifier
                                      "Struct8"}
                                  NameOriginInSource)),
                            structFieldOffset = 256,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:92:12",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct8b",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct8b"},
                            structFieldType = TypeTypedef
                              (TypedefRegular
                                NamePair {
                                  nameC = Name "struct8b",
                                  nameHsIdent = HsIdentifier
                                    "Struct8b"}),
                            structFieldOffset = 256,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:96:11",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct9",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct9"},
                            structFieldType = TypeTypedef
                              (TypedefSquashed
                                (Name "struct9")
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "struct9",
                                    nameHsIdent = HsIdentifier
                                      "Struct9"}
                                  NameOriginInSource)),
                            structFieldOffset = 256,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:97:13",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct9_t",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct9_t"},
                            structFieldType = TypeTypedef
                              (TypedefRegular
                                NamePair {
                                  nameC = Name "struct9_t",
                                  nameHsIdent = HsIdentifier
                                    "Struct9_t"}),
                            structFieldOffset = 256,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:98:14",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct10_t",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct10_t"},
                            structFieldType = TypeTypedef
                              (TypedefSquashed
                                (Name "struct10_t")
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "struct10_t",
                                    nameHsIdent = HsIdentifier
                                      "Struct10_t"}
                                  (NameOriginRenamedFrom
                                    (Name "struct10")))),
                            structFieldOffset = 256,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:99:16",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct10_t_t",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct10_t_t"},
                            structFieldType = TypeTypedef
                              (TypedefRegular
                                NamePair {
                                  nameC = Name "struct10_t_t",
                                  nameHsIdent = HsIdentifier
                                    "Struct10_t_t"}),
                            structFieldOffset = 256,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:102:14",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct11_t",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct11_t"},
                            structFieldType = TypeTypedef
                              (TypedefSquashed
                                (Name "struct11_t")
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "struct11_t",
                                    nameHsIdent = HsIdentifier
                                      "Struct11_t"}
                                  (NameOriginRenamedFrom
                                    (Name "struct11")))),
                            structFieldOffset = 256,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "typedef_analysis.h:103:14",
                            structFieldName = NamePair {
                              nameC = Name
                                "useTypedef_struct12_t",
                              nameHsIdent = HsIdentifier
                                "use_sites_useTypedef_struct12_t"},
                            structFieldType = TypeTypedef
                              (TypedefSquashed
                                (Name "struct12_t")
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "struct12_t",
                                    nameHsIdent = HsIdentifier
                                      "Struct12_t"}
                                  (NameOriginRenamedFrom
                                    (Name "struct12")))),
                            structFieldOffset = 384,
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
              (Add 18)
              (Seq
                [
                  PokeByteOff (Idx 19) 0 (Idx 0),
                  PokeByteOff (Idx 19) 0 (Idx 1),
                  PokeByteOff (Idx 19) 0 (Idx 2),
                  PokeByteOff (Idx 19) 8 (Idx 3),
                  PokeByteOff (Idx 19) 16 (Idx 4),
                  PokeByteOff (Idx 19) 16 (Idx 5),
                  PokeByteOff (Idx 19) 24 (Idx 6),
                  PokeByteOff (Idx 19) 24 (Idx 7),
                  PokeByteOff (Idx 19) 32 (Idx 8),
                  PokeByteOff (Idx 19) 32 (Idx 9),
                  PokeByteOff
                    (Idx 19)
                    32
                    (Idx 10),
                  PokeByteOff
                    (Idx 19)
                    32
                    (Idx 11),
                  PokeByteOff
                    (Idx 19)
                    32
                    (Idx 12),
                  PokeByteOff
                    (Idx 19)
                    32
                    (Idx 13),
                  PokeByteOff
                    (Idx 19)
                    32
                    (Idx 14),
                  PokeByteOff
                    (Idx 19)
                    32
                    (Idx 15),
                  PokeByteOff
                    (Idx 19)
                    32
                    (Idx 16),
                  PokeByteOff
                    (Idx 19)
                    48
                    (Idx 17)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Use_sites"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Use_sites")]
