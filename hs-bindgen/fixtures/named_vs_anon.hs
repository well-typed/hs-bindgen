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
            declHeader = "named_vs_anon.h",
            declComment = Nothing},
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
        [Eq, Show, Storable],
      structComment = Just
        (Comment
          Nothing
          (Just "named_vs_anon.h:15:8")
          (Just "named_vs_anon.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
                declHeader = "named_vs_anon.h",
                declComment = Nothing},
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
            [Eq, Show, Storable],
          structComment = Just
            (Comment
              Nothing
              (Just "named_vs_anon.h:15:8")
              (Just "named_vs_anon.h")
              [])}
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:15:8")
                      (Just "named_vs_anon.h")
                      [])})
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:15:8")
                      (Just "named_vs_anon.h")
                      [])}
                (Add 0)
                (Seq [])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "A",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "A",
      deriveInstanceComment =
      Nothing},
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
            declHeader = "named_vs_anon.h",
            declComment = Nothing},
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
        [Eq, Show, Storable],
      structComment = Just
        (Comment
          Nothing
          (Just "named_vs_anon.h:16:8")
          (Just "named_vs_anon.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
                declHeader = "named_vs_anon.h",
                declComment = Nothing},
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
            [Eq, Show, Storable],
          structComment = Just
            (Comment
              Nothing
              (Just "named_vs_anon.h:16:8")
              (Just "named_vs_anon.h")
              [])}
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:16:8")
                      (Just "named_vs_anon.h")
                      [])})
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:16:8")
                      (Just "named_vs_anon.h")
                      [])}
                (Add 0)
                (Seq [])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Struct1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Struct1",
      deriveInstanceComment =
      Nothing},
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
            declHeader = "named_vs_anon.h",
            declComment = Nothing},
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
        [Eq, Show, Storable],
      structComment = Just
        (Comment
          Nothing
          (Just "named_vs_anon.h:19:8")
          (Just "named_vs_anon.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
                declHeader = "named_vs_anon.h",
                declComment = Nothing},
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
            [Eq, Show, Storable],
          structComment = Just
            (Comment
              Nothing
              (Just "named_vs_anon.h:19:8")
              (Just "named_vs_anon.h")
              [])}
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:19:8")
                      (Just "named_vs_anon.h")
                      [])})
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:19:8")
                      (Just "named_vs_anon.h")
                      [])}
                (Add 0)
                (Seq [])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "B_s",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "B_s",
      deriveInstanceComment =
      Nothing},
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
            declHeader = "named_vs_anon.h",
            declComment = Nothing},
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
        [Eq, Show, Storable],
      structComment = Just
        (Comment
          Nothing
          (Just "named_vs_anon.h:20:8")
          (Just "named_vs_anon.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
                declHeader = "named_vs_anon.h",
                declComment = Nothing},
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
            [Eq, Show, Storable],
          structComment = Just
            (Comment
              Nothing
              (Just "named_vs_anon.h:20:8")
              (Just "named_vs_anon.h")
              [])}
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:20:8")
                      (Just "named_vs_anon.h")
                      [])})
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:20:8")
                      (Just "named_vs_anon.h")
                      [])}
                (Add 0)
                (Seq [])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Struct2_s",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Struct2_s",
      deriveInstanceComment =
      Nothing},
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
            declHeader = "named_vs_anon.h",
            declComment = Nothing},
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
        [Eq, Show, Storable],
      structComment = Just
        (Comment
          Nothing
          (Just "named_vs_anon.h:23:36")
          (Just "named_vs_anon.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
                declHeader = "named_vs_anon.h",
                declComment = Nothing},
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
            [Eq, Show, Storable],
          structComment = Just
            (Comment
              Nothing
              (Just "named_vs_anon.h:23:36")
              (Just "named_vs_anon.h")
              [])}
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:23:36")
                      (Just "named_vs_anon.h")
                      [])})
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:23:36")
                      (Just "named_vs_anon.h")
                      [])}
                (Add 0)
                (Seq [])))},
      defineInstanceComment =
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
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "C",
      deriveInstanceComment =
      Nothing},
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
            declHeader = "named_vs_anon.h",
            declComment = Nothing},
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
        [Eq, Show, Storable],
      structComment = Just
        (Comment
          Nothing
          (Just "named_vs_anon.h:24:36")
          (Just "named_vs_anon.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
                declHeader = "named_vs_anon.h",
                declComment = Nothing},
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
            [Eq, Show, Storable],
          structComment = Just
            (Comment
              Nothing
              (Just "named_vs_anon.h:24:36")
              (Just "named_vs_anon.h")
              [])}
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:24:36")
                      (Just "named_vs_anon.h")
                      [])})
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:24:36")
                      (Just "named_vs_anon.h")
                      [])}
                (Add 0)
                (Seq [])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Struct3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Struct3",
      deriveInstanceComment =
      Nothing},
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
            declHeader = "named_vs_anon.h",
            declComment = Nothing},
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
        [Eq, Show, Storable],
      structComment = Just
        (Comment
          Nothing
          (Just "named_vs_anon.h:27:15")
          (Just "named_vs_anon.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
                declHeader = "named_vs_anon.h",
                declComment = Nothing},
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
            [Eq, Show, Storable],
          structComment = Just
            (Comment
              Nothing
              (Just "named_vs_anon.h:27:15")
              (Just "named_vs_anon.h")
              [])}
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:27:15")
                      (Just "named_vs_anon.h")
                      [])})
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:27:15")
                      (Just "named_vs_anon.h")
                      [])}
                (Add 0)
                (Seq [])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "D",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "D",
      deriveInstanceComment =
      Nothing},
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
            declHeader = "named_vs_anon.h",
            declComment = Nothing},
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
        [Eq, Show, Storable],
      structComment = Just
        (Comment
          Nothing
          (Just "named_vs_anon.h:28:15")
          (Just "named_vs_anon.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
                declHeader = "named_vs_anon.h",
                declComment = Nothing},
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
            [Eq, Show, Storable],
          structComment = Just
            (Comment
              Nothing
              (Just "named_vs_anon.h:28:15")
              (Just "named_vs_anon.h")
              [])}
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:28:15")
                      (Just "named_vs_anon.h")
                      [])})
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:28:15")
                      (Just "named_vs_anon.h")
                      [])}
                (Add 0)
                (Seq [])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Struct4",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Struct4",
      deriveInstanceComment =
      Nothing},
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
            declHeader = "named_vs_anon.h",
            declComment = Nothing},
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
        [Eq, Show, Storable],
      structComment = Just
        (Comment
          Nothing
          (Just "named_vs_anon.h:31:15")
          (Just "named_vs_anon.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
                declHeader = "named_vs_anon.h",
                declComment = Nothing},
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
            [Eq, Show, Storable],
          structComment = Just
            (Comment
              Nothing
              (Just "named_vs_anon.h:31:15")
              (Just "named_vs_anon.h")
              [])}
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:31:15")
                      (Just "named_vs_anon.h")
                      [])})
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:31:15")
                      (Just "named_vs_anon.h")
                      [])}
                (Add 0)
                (Seq [])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "E_s",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "E_s",
      deriveInstanceComment =
      Nothing},
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
            declHeader = "named_vs_anon.h",
            declComment = Nothing},
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
        [Eq, Show, Storable],
      structComment = Just
        (Comment
          Nothing
          (Just "named_vs_anon.h:32:15")
          (Just "named_vs_anon.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
                declHeader = "named_vs_anon.h",
                declComment = Nothing},
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
            [Eq, Show, Storable],
          structComment = Just
            (Comment
              Nothing
              (Just "named_vs_anon.h:32:15")
              (Just "named_vs_anon.h")
              [])}
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:32:15")
                      (Just "named_vs_anon.h")
                      [])})
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:32:15")
                      (Just "named_vs_anon.h")
                      [])}
                (Add 0)
                (Seq [])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Struct5_s",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Struct5_s",
      deriveInstanceComment =
      Nothing},
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
            declHeader = "named_vs_anon.h",
            declComment = Nothing},
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
        [Eq, Show, Storable],
      structComment = Just
        (Comment
          Nothing
          (Just "named_vs_anon.h:35:9")
          (Just "named_vs_anon.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
                declHeader = "named_vs_anon.h",
                declComment = Nothing},
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
            [Eq, Show, Storable],
          structComment = Just
            (Comment
              Nothing
              (Just "named_vs_anon.h:35:9")
              (Just "named_vs_anon.h")
              [])}
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:35:9")
                      (Just "named_vs_anon.h")
                      [])})
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:35:9")
                      (Just "named_vs_anon.h")
                      [])}
                (Add 0)
                (Seq [])))},
      defineInstanceComment =
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
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "F",
      deriveInstanceComment =
      Nothing},
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
            declHeader = "named_vs_anon.h",
            declComment = Nothing},
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
        [Eq, Show, Storable],
      structComment = Just
        (Comment
          Nothing
          (Just "named_vs_anon.h:36:9")
          (Just "named_vs_anon.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
                declHeader = "named_vs_anon.h",
                declComment = Nothing},
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
            [Eq, Show, Storable],
          structComment = Just
            (Comment
              Nothing
              (Just "named_vs_anon.h:36:9")
              (Just "named_vs_anon.h")
              [])}
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:36:9")
                      (Just "named_vs_anon.h")
                      [])})
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:36:9")
                      (Just "named_vs_anon.h")
                      [])}
                (Add 0)
                (Seq [])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Typedef1",
      deriveInstanceComment =
      Nothing},
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
            declHeader = "named_vs_anon.h",
            declComment = Nothing},
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
        [Eq, Show, Storable],
      structComment = Just
        (Comment
          Nothing
          (Just "named_vs_anon.h:39:9")
          (Just "named_vs_anon.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
                declHeader = "named_vs_anon.h",
                declComment = Nothing},
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
            [Eq, Show, Storable],
          structComment = Just
            (Comment
              Nothing
              (Just "named_vs_anon.h:39:9")
              (Just "named_vs_anon.h")
              [])}
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:39:9")
                      (Just "named_vs_anon.h")
                      [])})
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:39:9")
                      (Just "named_vs_anon.h")
                      [])}
                (Add 0)
                (Seq [])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "G",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "G",
      deriveInstanceComment =
      Nothing},
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
            declHeader = "named_vs_anon.h",
            declComment = Nothing},
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
        [Eq, Show, Storable],
      structComment = Just
        (Comment
          Nothing
          (Just "named_vs_anon.h:40:9")
          (Just "named_vs_anon.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
                declHeader = "named_vs_anon.h",
                declComment = Nothing},
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
            [Eq, Show, Storable],
          structComment = Just
            (Comment
              Nothing
              (Just "named_vs_anon.h:40:9")
              (Just "named_vs_anon.h")
              [])}
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:40:9")
                      (Just "named_vs_anon.h")
                      [])})
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:40:9")
                      (Just "named_vs_anon.h")
                      [])}
                (Add 0)
                (Seq [])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Typedef2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Typedef2",
      deriveInstanceComment =
      Nothing},
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
            declHeader = "named_vs_anon.h",
            declComment = Nothing},
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
        [Eq, Show, Storable],
      structComment = Just
        (Comment
          Nothing
          (Just "named_vs_anon.h:43:9")
          (Just "named_vs_anon.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
                declHeader = "named_vs_anon.h",
                declComment = Nothing},
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
            [Eq, Show, Storable],
          structComment = Just
            (Comment
              Nothing
              (Just "named_vs_anon.h:43:9")
              (Just "named_vs_anon.h")
              [])}
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:43:9")
                      (Just "named_vs_anon.h")
                      [])})
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:43:9")
                      (Just "named_vs_anon.h")
                      [])}
                (Add 0)
                (Seq [])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "H",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "H",
      deriveInstanceComment =
      Nothing},
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
            declHeader = "named_vs_anon.h",
            declComment = Nothing},
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
        [Eq, Show, Storable],
      structComment = Just
        (Comment
          Nothing
          (Just "named_vs_anon.h:44:9")
          (Just "named_vs_anon.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
                declHeader = "named_vs_anon.h",
                declComment = Nothing},
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
            [Eq, Show, Storable],
          structComment = Just
            (Comment
              Nothing
              (Just "named_vs_anon.h:44:9")
              (Just "named_vs_anon.h")
              [])}
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:44:9")
                      (Just "named_vs_anon.h")
                      [])})
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
                        declHeader = "named_vs_anon.h",
                        declComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    (Comment
                      Nothing
                      (Just "named_vs_anon.h:44:9")
                      (Just "named_vs_anon.h")
                      [])}
                (Add 0)
                (Seq [])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Typedef3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Typedef3",
      deriveInstanceComment =
      Nothing}]
