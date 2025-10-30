[
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Linked_list_A_t",
      structConstr = Name
        "@NsConstr"
        "Linked_list_A_t",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "linked_list_A_t_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "recursive_struct.h:2:7",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "linked_list_A_t_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Just
                "recursive_struct.h:2:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["recursive_struct.h"],
                  headerInclude =
                  "recursive_struct.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "linked_list_A_t_next",
          fieldType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Linked_list_A_t")),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "recursive_struct.h:3:27",
                fieldName = NamePair {
                  nameC = Name "next",
                  nameHsIdent = Identifier
                    "linked_list_A_t_next"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "linked_list_A_t",
                    nameHsIdent = Identifier
                      "Linked_list_A_t"}
                  (NameOriginRenamedFrom
                    (Name "linked_list_A_s"))),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "next",
              commentLocation = Just
                "recursive_struct.h:3:27",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["recursive_struct.h"],
                  headerInclude =
                  "recursive_struct.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "recursive_struct.h:1:16",
            declId = NamePair {
              nameC = Name "linked_list_A_t",
              nameHsIdent = Identifier
                "Linked_list_A_t"},
            declOrigin =
            NameOriginRenamedFrom
              (Name "linked_list_A_s"),
            declAliases = [
              Name "linked_list_A_t"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["recursive_struct.h"],
                headerInclude =
                "recursive_struct.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name
                  "@NsConstr"
                  "Linked_list_A_t"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "recursive_struct.h:2:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "linked_list_A_t_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "recursive_struct.h:3:27",
                    fieldName = NamePair {
                      nameC = Name "next",
                      nameHsIdent = Identifier
                        "linked_list_A_t_next"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "linked_list_A_t",
                        nameHsIdent = Identifier
                          "Linked_list_A_t"}
                      (NameOriginRenamedFrom
                        (Name "linked_list_A_s"))),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecModule = ModuleName
                "Example",
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "linked_list_A_t",
          commentLocation = Just
            "recursive_struct.h:1:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["recursive_struct.h"],
              headerInclude =
              "recursive_struct.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Linked_list_A_t",
          structConstr = Name
            "@NsConstr"
            "Linked_list_A_t",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "linked_list_A_t_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "recursive_struct.h:2:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "linked_list_A_t_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "x",
                  commentLocation = Just
                    "recursive_struct.h:2:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["recursive_struct.h"],
                      headerInclude =
                      "recursive_struct.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "linked_list_A_t_next",
              fieldType = HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Linked_list_A_t")),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "recursive_struct.h:3:27",
                    fieldName = NamePair {
                      nameC = Name "next",
                      nameHsIdent = Identifier
                        "linked_list_A_t_next"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "linked_list_A_t",
                        nameHsIdent = Identifier
                          "Linked_list_A_t"}
                      (NameOriginRenamedFrom
                        (Name "linked_list_A_s"))),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "next",
                  commentLocation = Just
                    "recursive_struct.h:3:27",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["recursive_struct.h"],
                      headerInclude =
                      "recursive_struct.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "recursive_struct.h:1:16",
                declId = NamePair {
                  nameC = Name "linked_list_A_t",
                  nameHsIdent = Identifier
                    "Linked_list_A_t"},
                declOrigin =
                NameOriginRenamedFrom
                  (Name "linked_list_A_s"),
                declAliases = [
                  Name "linked_list_A_t"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["recursive_struct.h"],
                    headerInclude =
                    "recursive_struct.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name
                      "@NsConstr"
                      "Linked_list_A_t"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "recursive_struct.h:2:7",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "linked_list_A_t_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "recursive_struct.h:3:27",
                        fieldName = NamePair {
                          nameC = Name "next",
                          nameHsIdent = Identifier
                            "linked_list_A_t_next"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeStruct
                          NamePair {
                            nameC = Name "linked_list_A_t",
                            nameHsIdent = Identifier
                              "Linked_list_A_t"}
                          (NameOriginRenamedFrom
                            (Name "linked_list_A_s"))),
                      structFieldOffset = 64,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecModule = ModuleName
                    "Example",
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "linked_list_A_t",
              commentLocation = Just
                "recursive_struct.h:1:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["recursive_struct.h"],
                  headerInclude =
                  "recursive_struct.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 16,
          storableAlignment = 8,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Linked_list_A_t",
                  structConstr = Name
                    "@NsConstr"
                    "Linked_list_A_t",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "linked_list_A_t_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "recursive_struct.h:2:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "linked_list_A_t_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "recursive_struct.h:2:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["recursive_struct.h"],
                              headerInclude =
                              "recursive_struct.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "linked_list_A_t_next",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name
                            "@NsTypeConstr"
                            "Linked_list_A_t")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "recursive_struct.h:3:27",
                            fieldName = NamePair {
                              nameC = Name "next",
                              nameHsIdent = Identifier
                                "linked_list_A_t_next"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "linked_list_A_t",
                                nameHsIdent = Identifier
                                  "Linked_list_A_t"}
                              (NameOriginRenamedFrom
                                (Name "linked_list_A_s"))),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "next",
                          commentLocation = Just
                            "recursive_struct.h:3:27",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["recursive_struct.h"],
                              headerInclude =
                              "recursive_struct.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "recursive_struct.h:1:16",
                        declId = NamePair {
                          nameC = Name "linked_list_A_t",
                          nameHsIdent = Identifier
                            "Linked_list_A_t"},
                        declOrigin =
                        NameOriginRenamedFrom
                          (Name "linked_list_A_s"),
                        declAliases = [
                          Name "linked_list_A_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["recursive_struct.h"],
                            headerInclude =
                            "recursive_struct.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Linked_list_A_t"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "recursive_struct.h:2:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "linked_list_A_t_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "recursive_struct.h:3:27",
                                fieldName = NamePair {
                                  nameC = Name "next",
                                  nameHsIdent = Identifier
                                    "linked_list_A_t_next"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "linked_list_A_t",
                                    nameHsIdent = Identifier
                                      "Linked_list_A_t"}
                                  (NameOriginRenamedFrom
                                    (Name "linked_list_A_s"))),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = ModuleName
                            "Example",
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "linked_list_A_t",
                      commentLocation = Just
                        "recursive_struct.h:1:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["recursive_struct.h"],
                          headerInclude =
                          "recursive_struct.h"},
                      commentChildren = []}})
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
                  structName = Name
                    "@NsTypeConstr"
                    "Linked_list_A_t",
                  structConstr = Name
                    "@NsConstr"
                    "Linked_list_A_t",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "linked_list_A_t_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "recursive_struct.h:2:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "linked_list_A_t_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "recursive_struct.h:2:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["recursive_struct.h"],
                              headerInclude =
                              "recursive_struct.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "linked_list_A_t_next",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name
                            "@NsTypeConstr"
                            "Linked_list_A_t")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "recursive_struct.h:3:27",
                            fieldName = NamePair {
                              nameC = Name "next",
                              nameHsIdent = Identifier
                                "linked_list_A_t_next"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "linked_list_A_t",
                                nameHsIdent = Identifier
                                  "Linked_list_A_t"}
                              (NameOriginRenamedFrom
                                (Name "linked_list_A_s"))),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "next",
                          commentLocation = Just
                            "recursive_struct.h:3:27",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["recursive_struct.h"],
                              headerInclude =
                              "recursive_struct.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "recursive_struct.h:1:16",
                        declId = NamePair {
                          nameC = Name "linked_list_A_t",
                          nameHsIdent = Identifier
                            "Linked_list_A_t"},
                        declOrigin =
                        NameOriginRenamedFrom
                          (Name "linked_list_A_s"),
                        declAliases = [
                          Name "linked_list_A_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["recursive_struct.h"],
                            headerInclude =
                            "recursive_struct.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Linked_list_A_t"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "recursive_struct.h:2:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "linked_list_A_t_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "recursive_struct.h:3:27",
                                fieldName = NamePair {
                                  nameC = Name "next",
                                  nameHsIdent = Identifier
                                    "linked_list_A_t_next"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "linked_list_A_t",
                                    nameHsIdent = Identifier
                                      "Linked_list_A_t"}
                                  (NameOriginRenamedFrom
                                    (Name "linked_list_A_s"))),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = ModuleName
                            "Example",
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "linked_list_A_t",
                      commentLocation = Just
                        "recursive_struct.h:1:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["recursive_struct.h"],
                          headerInclude =
                          "recursive_struct.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      8
                      (Idx 1)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Linked_list_A_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Linked_list_A_t",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Linked_list_B_t",
      structConstr = Name
        "@NsConstr"
        "Linked_list_B_t",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "linked_list_B_t_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "recursive_struct.h:10:7",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "linked_list_B_t_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Just
                "recursive_struct.h:10:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["recursive_struct.h"],
                  headerInclude =
                  "recursive_struct.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "linked_list_B_t_next",
          fieldType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Linked_list_B_t")),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "recursive_struct.h:11:20",
                fieldName = NamePair {
                  nameC = Name "next",
                  nameHsIdent = Identifier
                    "linked_list_B_t_next"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeTypedef
                  (TypedefSquashed
                    (Name "linked_list_B_t")
                    (TypeStruct
                      NamePair {
                        nameC = Name "linked_list_B_t",
                        nameHsIdent = Identifier
                          "Linked_list_B_t"}
                      NameOriginInSource))),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "next",
              commentLocation = Just
                "recursive_struct.h:11:20",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["recursive_struct.h"],
                  headerInclude =
                  "recursive_struct.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "recursive_struct.h:9:8",
            declId = NamePair {
              nameC = Name "linked_list_B_t",
              nameHsIdent = Identifier
                "Linked_list_B_t"},
            declOrigin = NameOriginInSource,
            declAliases = [
              Name "linked_list_B_t"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["recursive_struct.h"],
                headerInclude =
                "recursive_struct.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name
                  "@NsConstr"
                  "Linked_list_B_t"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "recursive_struct.h:10:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "linked_list_B_t_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "recursive_struct.h:11:20",
                    fieldName = NamePair {
                      nameC = Name "next",
                      nameHsIdent = Identifier
                        "linked_list_B_t_next"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeTypedef
                      (TypedefSquashed
                        (Name "linked_list_B_t")
                        (TypeStruct
                          NamePair {
                            nameC = Name "linked_list_B_t",
                            nameHsIdent = Identifier
                              "Linked_list_B_t"}
                          NameOriginInSource))),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecModule = ModuleName
                "Example",
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "linked_list_B_t",
          commentLocation = Just
            "recursive_struct.h:9:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["recursive_struct.h"],
              headerInclude =
              "recursive_struct.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Linked_list_B_t",
          structConstr = Name
            "@NsConstr"
            "Linked_list_B_t",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "linked_list_B_t_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "recursive_struct.h:10:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "linked_list_B_t_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "x",
                  commentLocation = Just
                    "recursive_struct.h:10:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["recursive_struct.h"],
                      headerInclude =
                      "recursive_struct.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "linked_list_B_t_next",
              fieldType = HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Linked_list_B_t")),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "recursive_struct.h:11:20",
                    fieldName = NamePair {
                      nameC = Name "next",
                      nameHsIdent = Identifier
                        "linked_list_B_t_next"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeTypedef
                      (TypedefSquashed
                        (Name "linked_list_B_t")
                        (TypeStruct
                          NamePair {
                            nameC = Name "linked_list_B_t",
                            nameHsIdent = Identifier
                              "Linked_list_B_t"}
                          NameOriginInSource))),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "next",
                  commentLocation = Just
                    "recursive_struct.h:11:20",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["recursive_struct.h"],
                      headerInclude =
                      "recursive_struct.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "recursive_struct.h:9:8",
                declId = NamePair {
                  nameC = Name "linked_list_B_t",
                  nameHsIdent = Identifier
                    "Linked_list_B_t"},
                declOrigin = NameOriginInSource,
                declAliases = [
                  Name "linked_list_B_t"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["recursive_struct.h"],
                    headerInclude =
                    "recursive_struct.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name
                      "@NsConstr"
                      "Linked_list_B_t"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "recursive_struct.h:10:7",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "linked_list_B_t_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "recursive_struct.h:11:20",
                        fieldName = NamePair {
                          nameC = Name "next",
                          nameHsIdent = Identifier
                            "linked_list_B_t_next"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeTypedef
                          (TypedefSquashed
                            (Name "linked_list_B_t")
                            (TypeStruct
                              NamePair {
                                nameC = Name "linked_list_B_t",
                                nameHsIdent = Identifier
                                  "Linked_list_B_t"}
                              NameOriginInSource))),
                      structFieldOffset = 64,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecModule = ModuleName
                    "Example",
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "linked_list_B_t",
              commentLocation = Just
                "recursive_struct.h:9:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["recursive_struct.h"],
                  headerInclude =
                  "recursive_struct.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 16,
          storableAlignment = 8,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Linked_list_B_t",
                  structConstr = Name
                    "@NsConstr"
                    "Linked_list_B_t",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "linked_list_B_t_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "recursive_struct.h:10:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "linked_list_B_t_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "recursive_struct.h:10:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["recursive_struct.h"],
                              headerInclude =
                              "recursive_struct.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "linked_list_B_t_next",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name
                            "@NsTypeConstr"
                            "Linked_list_B_t")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "recursive_struct.h:11:20",
                            fieldName = NamePair {
                              nameC = Name "next",
                              nameHsIdent = Identifier
                                "linked_list_B_t_next"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeTypedef
                              (TypedefSquashed
                                (Name "linked_list_B_t")
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "linked_list_B_t",
                                    nameHsIdent = Identifier
                                      "Linked_list_B_t"}
                                  NameOriginInSource))),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "next",
                          commentLocation = Just
                            "recursive_struct.h:11:20",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["recursive_struct.h"],
                              headerInclude =
                              "recursive_struct.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "recursive_struct.h:9:8",
                        declId = NamePair {
                          nameC = Name "linked_list_B_t",
                          nameHsIdent = Identifier
                            "Linked_list_B_t"},
                        declOrigin = NameOriginInSource,
                        declAliases = [
                          Name "linked_list_B_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["recursive_struct.h"],
                            headerInclude =
                            "recursive_struct.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Linked_list_B_t"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "recursive_struct.h:10:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "linked_list_B_t_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "recursive_struct.h:11:20",
                                fieldName = NamePair {
                                  nameC = Name "next",
                                  nameHsIdent = Identifier
                                    "linked_list_B_t_next"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeTypedef
                                  (TypedefSquashed
                                    (Name "linked_list_B_t")
                                    (TypeStruct
                                      NamePair {
                                        nameC = Name "linked_list_B_t",
                                        nameHsIdent = Identifier
                                          "Linked_list_B_t"}
                                      NameOriginInSource))),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = ModuleName
                            "Example",
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "linked_list_B_t",
                      commentLocation = Just
                        "recursive_struct.h:9:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["recursive_struct.h"],
                          headerInclude =
                          "recursive_struct.h"},
                      commentChildren = []}})
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
                  structName = Name
                    "@NsTypeConstr"
                    "Linked_list_B_t",
                  structConstr = Name
                    "@NsConstr"
                    "Linked_list_B_t",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "linked_list_B_t_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "recursive_struct.h:10:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "linked_list_B_t_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "recursive_struct.h:10:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["recursive_struct.h"],
                              headerInclude =
                              "recursive_struct.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "linked_list_B_t_next",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name
                            "@NsTypeConstr"
                            "Linked_list_B_t")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "recursive_struct.h:11:20",
                            fieldName = NamePair {
                              nameC = Name "next",
                              nameHsIdent = Identifier
                                "linked_list_B_t_next"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeTypedef
                              (TypedefSquashed
                                (Name "linked_list_B_t")
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "linked_list_B_t",
                                    nameHsIdent = Identifier
                                      "Linked_list_B_t"}
                                  NameOriginInSource))),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "next",
                          commentLocation = Just
                            "recursive_struct.h:11:20",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["recursive_struct.h"],
                              headerInclude =
                              "recursive_struct.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "recursive_struct.h:9:8",
                        declId = NamePair {
                          nameC = Name "linked_list_B_t",
                          nameHsIdent = Identifier
                            "Linked_list_B_t"},
                        declOrigin = NameOriginInSource,
                        declAliases = [
                          Name "linked_list_B_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["recursive_struct.h"],
                            headerInclude =
                            "recursive_struct.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Linked_list_B_t"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "recursive_struct.h:10:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "linked_list_B_t_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "recursive_struct.h:11:20",
                                fieldName = NamePair {
                                  nameC = Name "next",
                                  nameHsIdent = Identifier
                                    "linked_list_B_t_next"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeTypedef
                                  (TypedefSquashed
                                    (Name "linked_list_B_t")
                                    (TypeStruct
                                      NamePair {
                                        nameC = Name "linked_list_B_t",
                                        nameHsIdent = Identifier
                                          "Linked_list_B_t"}
                                      NameOriginInSource))),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = ModuleName
                            "Example",
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "linked_list_B_t",
                      commentLocation = Just
                        "recursive_struct.h:9:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["recursive_struct.h"],
                          headerInclude =
                          "recursive_struct.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      8
                      (Idx 1)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Linked_list_B_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Linked_list_B_t",
      deriveInstanceComment =
      Nothing}]
