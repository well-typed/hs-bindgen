[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "A",
      structConstr = HsName
        "@NsConstr"
        "A",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "named_vs_anon.h:15:8",
            declId = NamePair {
              nameC = Name "a",
              nameHsIdent = HsIdentifier "A"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "named_vs_anon.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "A"),
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
          "A",
        structConstr = HsName
          "@NsConstr"
          "A",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "named_vs_anon.h:15:8",
              declId = NamePair {
                nameC = Name "a",
                nameHsIdent = HsIdentifier "A"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "named_vs_anon.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "A"),
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
                  "A",
                structConstr = HsName
                  "@NsConstr"
                  "A",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:15:8",
                      declId = NamePair {
                        nameC = Name "a",
                        nameHsIdent = HsIdentifier "A"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "A"),
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
                  "A",
                structConstr = HsName
                  "@NsConstr"
                  "A",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:15:8",
                      declId = NamePair {
                        nameC = Name "a",
                        nameHsIdent = HsIdentifier "A"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "A"),
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
    (HsName "@NsTypeConstr" "A"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "A"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Struct1",
      structConstr = HsName
        "@NsConstr"
        "Struct1",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "named_vs_anon.h:16:8",
            declId = NamePair {
              nameC = Name "struct1",
              nameHsIdent = HsIdentifier
                "Struct1"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "named_vs_anon.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Struct1"),
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
          "Struct1",
        structConstr = HsName
          "@NsConstr"
          "Struct1",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "named_vs_anon.h:16:8",
              declId = NamePair {
                nameC = Name "struct1",
                nameHsIdent = HsIdentifier
                  "Struct1"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "named_vs_anon.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Struct1"),
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
                  "Struct1",
                structConstr = HsName
                  "@NsConstr"
                  "Struct1",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:16:8",
                      declId = NamePair {
                        nameC = Name "struct1",
                        nameHsIdent = HsIdentifier
                          "Struct1"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Struct1"),
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
                  "Struct1",
                structConstr = HsName
                  "@NsConstr"
                  "Struct1",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:16:8",
                      declId = NamePair {
                        nameC = Name "struct1",
                        nameHsIdent = HsIdentifier
                          "Struct1"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Struct1"),
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
        "B_s",
      structConstr = HsName
        "@NsConstr"
        "B_s",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "named_vs_anon.h:19:8",
            declId = NamePair {
              nameC = Name "b_s",
              nameHsIdent = HsIdentifier
                "B_s"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "named_vs_anon.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "B_s"),
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
          "B_s",
        structConstr = HsName
          "@NsConstr"
          "B_s",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "named_vs_anon.h:19:8",
              declId = NamePair {
                nameC = Name "b_s",
                nameHsIdent = HsIdentifier
                  "B_s"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "named_vs_anon.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "B_s"),
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
                  "B_s",
                structConstr = HsName
                  "@NsConstr"
                  "B_s",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:19:8",
                      declId = NamePair {
                        nameC = Name "b_s",
                        nameHsIdent = HsIdentifier
                          "B_s"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "B_s"),
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
                  "B_s",
                structConstr = HsName
                  "@NsConstr"
                  "B_s",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:19:8",
                      declId = NamePair {
                        nameC = Name "b_s",
                        nameHsIdent = HsIdentifier
                          "B_s"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "B_s"),
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
    (HsName "@NsTypeConstr" "B_s"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "B_s"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Struct2_s",
      structConstr = HsName
        "@NsConstr"
        "Struct2_s",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "named_vs_anon.h:20:8",
            declId = NamePair {
              nameC = Name "struct2_s",
              nameHsIdent = HsIdentifier
                "Struct2_s"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "named_vs_anon.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Struct2_s"),
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
          "Struct2_s",
        structConstr = HsName
          "@NsConstr"
          "Struct2_s",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "named_vs_anon.h:20:8",
              declId = NamePair {
                nameC = Name "struct2_s",
                nameHsIdent = HsIdentifier
                  "Struct2_s"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "named_vs_anon.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "Struct2_s"),
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
                  "Struct2_s",
                structConstr = HsName
                  "@NsConstr"
                  "Struct2_s",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:20:8",
                      declId = NamePair {
                        nameC = Name "struct2_s",
                        nameHsIdent = HsIdentifier
                          "Struct2_s"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Struct2_s"),
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
                  "Struct2_s",
                structConstr = HsName
                  "@NsConstr"
                  "Struct2_s",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:20:8",
                      declId = NamePair {
                        nameC = Name "struct2_s",
                        nameHsIdent = HsIdentifier
                          "Struct2_s"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Struct2_s"),
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
      "Struct2_s"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct2_s"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "C",
      structConstr = HsName
        "@NsConstr"
        "C",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "named_vs_anon.h:23:36",
            declId = NamePair {
              nameC = Name "c",
              nameHsIdent = HsIdentifier "C"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "named_vs_anon.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "C"),
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
          "C",
        structConstr = HsName
          "@NsConstr"
          "C",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "named_vs_anon.h:23:36",
              declId = NamePair {
                nameC = Name "c",
                nameHsIdent = HsIdentifier "C"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "named_vs_anon.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "C"),
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
                  "C",
                structConstr = HsName
                  "@NsConstr"
                  "C",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:23:36",
                      declId = NamePair {
                        nameC = Name "c",
                        nameHsIdent = HsIdentifier "C"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "C"),
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
                  "C",
                structConstr = HsName
                  "@NsConstr"
                  "C",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:23:36",
                      declId = NamePair {
                        nameC = Name "c",
                        nameHsIdent = HsIdentifier "C"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "C"),
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
    (HsName "@NsTypeConstr" "C"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "C"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Struct3",
      structConstr = HsName
        "@NsConstr"
        "Struct3",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "named_vs_anon.h:24:36",
            declId = NamePair {
              nameC = Name "struct3",
              nameHsIdent = HsIdentifier
                "Struct3"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "named_vs_anon.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Struct3"),
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
          "Struct3",
        structConstr = HsName
          "@NsConstr"
          "Struct3",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "named_vs_anon.h:24:36",
              declId = NamePair {
                nameC = Name "struct3",
                nameHsIdent = HsIdentifier
                  "Struct3"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "named_vs_anon.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Struct3"),
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
                  "Struct3",
                structConstr = HsName
                  "@NsConstr"
                  "Struct3",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:24:36",
                      declId = NamePair {
                        nameC = Name "struct3",
                        nameHsIdent = HsIdentifier
                          "Struct3"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Struct3"),
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
                  "Struct3",
                structConstr = HsName
                  "@NsConstr"
                  "Struct3",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:24:36",
                      declId = NamePair {
                        nameC = Name "struct3",
                        nameHsIdent = HsIdentifier
                          "Struct3"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Struct3"),
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
      "Struct3"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct3"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "D",
      structConstr = HsName
        "@NsConstr"
        "D",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "named_vs_anon.h:27:15",
            declId = NamePair {
              nameC = Name "d",
              nameHsIdent = HsIdentifier "D"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "named_vs_anon.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "D"),
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
          "D",
        structConstr = HsName
          "@NsConstr"
          "D",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "named_vs_anon.h:27:15",
              declId = NamePair {
                nameC = Name "d",
                nameHsIdent = HsIdentifier "D"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "named_vs_anon.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "D"),
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
                  "D",
                structConstr = HsName
                  "@NsConstr"
                  "D",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:27:15",
                      declId = NamePair {
                        nameC = Name "d",
                        nameHsIdent = HsIdentifier "D"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "D"),
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
                  "D",
                structConstr = HsName
                  "@NsConstr"
                  "D",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:27:15",
                      declId = NamePair {
                        nameC = Name "d",
                        nameHsIdent = HsIdentifier "D"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "D"),
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
    (HsName "@NsTypeConstr" "D"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "D"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Struct4",
      structConstr = HsName
        "@NsConstr"
        "Struct4",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "named_vs_anon.h:28:15",
            declId = NamePair {
              nameC = Name "struct4",
              nameHsIdent = HsIdentifier
                "Struct4"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "named_vs_anon.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Struct4"),
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
          "Struct4",
        structConstr = HsName
          "@NsConstr"
          "Struct4",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "named_vs_anon.h:28:15",
              declId = NamePair {
                nameC = Name "struct4",
                nameHsIdent = HsIdentifier
                  "Struct4"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "named_vs_anon.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Struct4"),
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
                  "Struct4",
                structConstr = HsName
                  "@NsConstr"
                  "Struct4",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:28:15",
                      declId = NamePair {
                        nameC = Name "struct4",
                        nameHsIdent = HsIdentifier
                          "Struct4"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Struct4"),
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
                  "Struct4",
                structConstr = HsName
                  "@NsConstr"
                  "Struct4",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:28:15",
                      declId = NamePair {
                        nameC = Name "struct4",
                        nameHsIdent = HsIdentifier
                          "Struct4"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Struct4"),
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
      "Struct4"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct4"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "E_s",
      structConstr = HsName
        "@NsConstr"
        "E_s",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "named_vs_anon.h:31:15",
            declId = NamePair {
              nameC = Name "e_s",
              nameHsIdent = HsIdentifier
                "E_s"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "named_vs_anon.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "E_s"),
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
          "E_s",
        structConstr = HsName
          "@NsConstr"
          "E_s",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "named_vs_anon.h:31:15",
              declId = NamePair {
                nameC = Name "e_s",
                nameHsIdent = HsIdentifier
                  "E_s"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "named_vs_anon.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "E_s"),
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
                  "E_s",
                structConstr = HsName
                  "@NsConstr"
                  "E_s",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:31:15",
                      declId = NamePair {
                        nameC = Name "e_s",
                        nameHsIdent = HsIdentifier
                          "E_s"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "E_s"),
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
                  "E_s",
                structConstr = HsName
                  "@NsConstr"
                  "E_s",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:31:15",
                      declId = NamePair {
                        nameC = Name "e_s",
                        nameHsIdent = HsIdentifier
                          "E_s"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "E_s"),
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
    (HsName "@NsTypeConstr" "E_s"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "E_s"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Struct5_s",
      structConstr = HsName
        "@NsConstr"
        "Struct5_s",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "named_vs_anon.h:32:15",
            declId = NamePair {
              nameC = Name "struct5_s",
              nameHsIdent = HsIdentifier
                "Struct5_s"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "named_vs_anon.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Struct5_s"),
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
          "Struct5_s",
        structConstr = HsName
          "@NsConstr"
          "Struct5_s",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "named_vs_anon.h:32:15",
              declId = NamePair {
                nameC = Name "struct5_s",
                nameHsIdent = HsIdentifier
                  "Struct5_s"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "named_vs_anon.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "Struct5_s"),
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
                  "Struct5_s",
                structConstr = HsName
                  "@NsConstr"
                  "Struct5_s",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:32:15",
                      declId = NamePair {
                        nameC = Name "struct5_s",
                        nameHsIdent = HsIdentifier
                          "Struct5_s"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Struct5_s"),
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
                  "Struct5_s",
                structConstr = HsName
                  "@NsConstr"
                  "Struct5_s",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:32:15",
                      declId = NamePair {
                        nameC = Name "struct5_s",
                        nameHsIdent = HsIdentifier
                          "Struct5_s"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Struct5_s"),
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
      "Struct5_s"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct5_s"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "F",
      structConstr = HsName
        "@NsConstr"
        "F",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "named_vs_anon.h:35:9",
            declId = NamePair {
              nameC = Name "f",
              nameHsIdent = HsIdentifier "F"},
            declOrigin = NameOriginGenerated
              (AnonId "named_vs_anon.h:35:9"),
            declAliases = [Name "f"],
            declHeader = "named_vs_anon.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "F"),
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
          "F",
        structConstr = HsName
          "@NsConstr"
          "F",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "named_vs_anon.h:35:9",
              declId = NamePair {
                nameC = Name "f",
                nameHsIdent = HsIdentifier "F"},
              declOrigin = NameOriginGenerated
                (AnonId "named_vs_anon.h:35:9"),
              declAliases = [Name "f"],
              declHeader = "named_vs_anon.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "F"),
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
                  "F",
                structConstr = HsName
                  "@NsConstr"
                  "F",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:35:9",
                      declId = NamePair {
                        nameC = Name "f",
                        nameHsIdent = HsIdentifier "F"},
                      declOrigin = NameOriginGenerated
                        (AnonId "named_vs_anon.h:35:9"),
                      declAliases = [Name "f"],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "F"),
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
                  "F",
                structConstr = HsName
                  "@NsConstr"
                  "F",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:35:9",
                      declId = NamePair {
                        nameC = Name "f",
                        nameHsIdent = HsIdentifier "F"},
                      declOrigin = NameOriginGenerated
                        (AnonId "named_vs_anon.h:35:9"),
                      declAliases = [Name "f"],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "F"),
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
    (HsName "@NsTypeConstr" "F"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "F"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Typedef1",
      structConstr = HsName
        "@NsConstr"
        "Typedef1",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "named_vs_anon.h:36:9",
            declId = NamePair {
              nameC = Name "typedef1",
              nameHsIdent = HsIdentifier
                "Typedef1"},
            declOrigin = NameOriginGenerated
              (AnonId "named_vs_anon.h:36:9"),
            declAliases = [Name "typedef1"],
            declHeader = "named_vs_anon.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Typedef1"),
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
          "Typedef1",
        structConstr = HsName
          "@NsConstr"
          "Typedef1",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "named_vs_anon.h:36:9",
              declId = NamePair {
                nameC = Name "typedef1",
                nameHsIdent = HsIdentifier
                  "Typedef1"},
              declOrigin = NameOriginGenerated
                (AnonId "named_vs_anon.h:36:9"),
              declAliases = [Name "typedef1"],
              declHeader = "named_vs_anon.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Typedef1"),
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
                  "Typedef1",
                structConstr = HsName
                  "@NsConstr"
                  "Typedef1",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:36:9",
                      declId = NamePair {
                        nameC = Name "typedef1",
                        nameHsIdent = HsIdentifier
                          "Typedef1"},
                      declOrigin = NameOriginGenerated
                        (AnonId "named_vs_anon.h:36:9"),
                      declAliases = [Name "typedef1"],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Typedef1"),
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
                  "Typedef1",
                structConstr = HsName
                  "@NsConstr"
                  "Typedef1",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:36:9",
                      declId = NamePair {
                        nameC = Name "typedef1",
                        nameHsIdent = HsIdentifier
                          "Typedef1"},
                      declOrigin = NameOriginGenerated
                        (AnonId "named_vs_anon.h:36:9"),
                      declAliases = [Name "typedef1"],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Typedef1"),
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
      "Typedef1"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Typedef1"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "G",
      structConstr = HsName
        "@NsConstr"
        "G",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "named_vs_anon.h:39:9",
            declId = NamePair {
              nameC = Name "g",
              nameHsIdent = HsIdentifier "G"},
            declOrigin = NameOriginGenerated
              (AnonId "named_vs_anon.h:39:9"),
            declAliases = [Name "g"],
            declHeader = "named_vs_anon.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "G"),
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
          "G",
        structConstr = HsName
          "@NsConstr"
          "G",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "named_vs_anon.h:39:9",
              declId = NamePair {
                nameC = Name "g",
                nameHsIdent = HsIdentifier "G"},
              declOrigin = NameOriginGenerated
                (AnonId "named_vs_anon.h:39:9"),
              declAliases = [Name "g"],
              declHeader = "named_vs_anon.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "G"),
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
                  "G",
                structConstr = HsName
                  "@NsConstr"
                  "G",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:39:9",
                      declId = NamePair {
                        nameC = Name "g",
                        nameHsIdent = HsIdentifier "G"},
                      declOrigin = NameOriginGenerated
                        (AnonId "named_vs_anon.h:39:9"),
                      declAliases = [Name "g"],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "G"),
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
                  "G",
                structConstr = HsName
                  "@NsConstr"
                  "G",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:39:9",
                      declId = NamePair {
                        nameC = Name "g",
                        nameHsIdent = HsIdentifier "G"},
                      declOrigin = NameOriginGenerated
                        (AnonId "named_vs_anon.h:39:9"),
                      declAliases = [Name "g"],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "G"),
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
    (HsName "@NsTypeConstr" "G"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "G"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Typedef2",
      structConstr = HsName
        "@NsConstr"
        "Typedef2",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "named_vs_anon.h:40:9",
            declId = NamePair {
              nameC = Name "typedef2",
              nameHsIdent = HsIdentifier
                "Typedef2"},
            declOrigin = NameOriginGenerated
              (AnonId "named_vs_anon.h:40:9"),
            declAliases = [Name "typedef2"],
            declHeader = "named_vs_anon.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Typedef2"),
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
          "Typedef2",
        structConstr = HsName
          "@NsConstr"
          "Typedef2",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "named_vs_anon.h:40:9",
              declId = NamePair {
                nameC = Name "typedef2",
                nameHsIdent = HsIdentifier
                  "Typedef2"},
              declOrigin = NameOriginGenerated
                (AnonId "named_vs_anon.h:40:9"),
              declAliases = [Name "typedef2"],
              declHeader = "named_vs_anon.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Typedef2"),
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
                  "Typedef2",
                structConstr = HsName
                  "@NsConstr"
                  "Typedef2",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:40:9",
                      declId = NamePair {
                        nameC = Name "typedef2",
                        nameHsIdent = HsIdentifier
                          "Typedef2"},
                      declOrigin = NameOriginGenerated
                        (AnonId "named_vs_anon.h:40:9"),
                      declAliases = [Name "typedef2"],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Typedef2"),
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
                  "Typedef2",
                structConstr = HsName
                  "@NsConstr"
                  "Typedef2",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:40:9",
                      declId = NamePair {
                        nameC = Name "typedef2",
                        nameHsIdent = HsIdentifier
                          "Typedef2"},
                      declOrigin = NameOriginGenerated
                        (AnonId "named_vs_anon.h:40:9"),
                      declAliases = [Name "typedef2"],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Typedef2"),
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
      "Typedef2"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Typedef2"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "H",
      structConstr = HsName
        "@NsConstr"
        "H",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "named_vs_anon.h:43:9",
            declId = NamePair {
              nameC = Name "h",
              nameHsIdent = HsIdentifier "H"},
            declOrigin = NameOriginGenerated
              (AnonId "named_vs_anon.h:43:9"),
            declAliases = [Name "h"],
            declHeader = "named_vs_anon.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "H"),
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
          "H",
        structConstr = HsName
          "@NsConstr"
          "H",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "named_vs_anon.h:43:9",
              declId = NamePair {
                nameC = Name "h",
                nameHsIdent = HsIdentifier "H"},
              declOrigin = NameOriginGenerated
                (AnonId "named_vs_anon.h:43:9"),
              declAliases = [Name "h"],
              declHeader = "named_vs_anon.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "H"),
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
                  "H",
                structConstr = HsName
                  "@NsConstr"
                  "H",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:43:9",
                      declId = NamePair {
                        nameC = Name "h",
                        nameHsIdent = HsIdentifier "H"},
                      declOrigin = NameOriginGenerated
                        (AnonId "named_vs_anon.h:43:9"),
                      declAliases = [Name "h"],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "H"),
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
                  "H",
                structConstr = HsName
                  "@NsConstr"
                  "H",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:43:9",
                      declId = NamePair {
                        nameC = Name "h",
                        nameHsIdent = HsIdentifier "H"},
                      declOrigin = NameOriginGenerated
                        (AnonId "named_vs_anon.h:43:9"),
                      declAliases = [Name "h"],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "H"),
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
    (HsName "@NsTypeConstr" "H"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "H"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Typedef3",
      structConstr = HsName
        "@NsConstr"
        "Typedef3",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "named_vs_anon.h:44:9",
            declId = NamePair {
              nameC = Name "typedef3",
              nameHsIdent = HsIdentifier
                "Typedef3"},
            declOrigin = NameOriginGenerated
              (AnonId "named_vs_anon.h:44:9"),
            declAliases = [Name "typedef3"],
            declHeader = "named_vs_anon.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Typedef3"),
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
          "Typedef3",
        structConstr = HsName
          "@NsConstr"
          "Typedef3",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "named_vs_anon.h:44:9",
              declId = NamePair {
                nameC = Name "typedef3",
                nameHsIdent = HsIdentifier
                  "Typedef3"},
              declOrigin = NameOriginGenerated
                (AnonId "named_vs_anon.h:44:9"),
              declAliases = [Name "typedef3"],
              declHeader = "named_vs_anon.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Typedef3"),
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
                  "Typedef3",
                structConstr = HsName
                  "@NsConstr"
                  "Typedef3",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:44:9",
                      declId = NamePair {
                        nameC = Name "typedef3",
                        nameHsIdent = HsIdentifier
                          "Typedef3"},
                      declOrigin = NameOriginGenerated
                        (AnonId "named_vs_anon.h:44:9"),
                      declAliases = [Name "typedef3"],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Typedef3"),
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
                  "Typedef3",
                structConstr = HsName
                  "@NsConstr"
                  "Typedef3",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "named_vs_anon.h:44:9",
                      declId = NamePair {
                        nameC = Name "typedef3",
                        nameHsIdent = HsIdentifier
                          "Typedef3"},
                      declOrigin = NameOriginGenerated
                        (AnonId "named_vs_anon.h:44:9"),
                      declAliases = [Name "typedef3"],
                      declHeader = "named_vs_anon.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Typedef3"),
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
      "Typedef3"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Typedef3")]
