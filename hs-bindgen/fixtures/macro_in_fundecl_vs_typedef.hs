[
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "MC",
      newtypeConstr = Name
        "@NsConstr"
        "MC",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_MC",
        fieldType = HsPrimType
          HsPrimCChar,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "macro_in_fundecl_vs_typedef.h:4:9",
          declId = NamePair {
            nameC = Name "MC",
            nameHsIdent = Identifier "MC"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "MC",
              newtypeField = Name
                "@NsVar"
                "un_MC"},
            macroType = TypePrim
              (PrimChar
                (PrimSignImplicit Nothing))},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
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
          commentOrigin = Just "MC",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:4:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MC",
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
        "MC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "MC",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "TC",
      newtypeConstr = Name
        "@NsConstr"
        "TC",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_TC",
        fieldType = HsPrimType
          HsPrimCChar,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "macro_in_fundecl_vs_typedef.h:5:14",
          declId = NamePair {
            nameC = Name "TC",
            nameHsIdent = Identifier "TC"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "TC",
              newtypeField = Name
                "@NsVar"
                "un_TC"},
            typedefType = TypePrim
              (PrimChar
                (PrimSignImplicit
                  (Just Signed)))},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
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
          commentOrigin = Just "TC",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:5:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "TC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "TC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "TC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "TC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "TC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "TC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "TC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "TC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "TC",
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
        "TC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "TC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "TC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "TC",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Struct1",
      structConstr = Name
        "@NsConstr"
        "Struct1",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "struct1_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "macro_in_fundecl_vs_typedef.h:18:30",
                fieldName = NamePair {
                  nameC = Name "a",
                  nameHsIdent = Identifier
                    "struct1_a"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "a",
              commentLocation = Just
                "macro_in_fundecl_vs_typedef.h:18:30",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "macro_in_fundecl_vs_typedef.h"],
                  headerInclude =
                  "macro_in_fundecl_vs_typedef.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "macro_in_fundecl_vs_typedef.h:18:16",
            declId = NamePair {
              nameC = Name "struct1",
              nameHsIdent = Identifier
                "Struct1"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  [
                    "macro_in_fundecl_vs_typedef.h"],
                headerInclude =
                "macro_in_fundecl_vs_typedef.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Struct1"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "macro_in_fundecl_vs_typedef.h:18:30",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "struct1_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecModule = Nothing,
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "struct1",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:18:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Struct1",
          structConstr = Name
            "@NsConstr"
            "Struct1",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "struct1_a",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "macro_in_fundecl_vs_typedef.h:18:30",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "struct1_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "a",
                  commentLocation = Just
                    "macro_in_fundecl_vs_typedef.h:18:30",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "macro_in_fundecl_vs_typedef.h"],
                      headerInclude =
                      "macro_in_fundecl_vs_typedef.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "macro_in_fundecl_vs_typedef.h:18:16",
                declId = NamePair {
                  nameC = Name "struct1",
                  nameHsIdent = Identifier
                    "Struct1"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      [
                        "macro_in_fundecl_vs_typedef.h"],
                    headerInclude =
                    "macro_in_fundecl_vs_typedef.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Struct1"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "macro_in_fundecl_vs_typedef.h:18:30",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = Identifier
                            "struct1_a"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecModule = Nothing,
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "struct1",
              commentLocation = Just
                "macro_in_fundecl_vs_typedef.h:18:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "macro_in_fundecl_vs_typedef.h"],
                  headerInclude =
                  "macro_in_fundecl_vs_typedef.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 4,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Struct1",
                  structConstr = Name
                    "@NsConstr"
                    "Struct1",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "struct1_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "macro_in_fundecl_vs_typedef.h:18:30",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "struct1_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "macro_in_fundecl_vs_typedef.h:18:30",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "macro_in_fundecl_vs_typedef.h"],
                              headerInclude =
                              "macro_in_fundecl_vs_typedef.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "macro_in_fundecl_vs_typedef.h:18:16",
                        declId = NamePair {
                          nameC = Name "struct1",
                          nameHsIdent = Identifier
                            "Struct1"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              [
                                "macro_in_fundecl_vs_typedef.h"],
                            headerInclude =
                            "macro_in_fundecl_vs_typedef.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Struct1"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "macro_in_fundecl_vs_typedef.h:18:30",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "struct1_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "struct1",
                      commentLocation = Just
                        "macro_in_fundecl_vs_typedef.h:18:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            [
                              "macro_in_fundecl_vs_typedef.h"],
                          headerInclude =
                          "macro_in_fundecl_vs_typedef.h"},
                      commentChildren = []}})
              [PeekByteOff (Idx 0) 0]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Struct1",
                  structConstr = Name
                    "@NsConstr"
                    "Struct1",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "struct1_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "macro_in_fundecl_vs_typedef.h:18:30",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "struct1_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "macro_in_fundecl_vs_typedef.h:18:30",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "macro_in_fundecl_vs_typedef.h"],
                              headerInclude =
                              "macro_in_fundecl_vs_typedef.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "macro_in_fundecl_vs_typedef.h:18:16",
                        declId = NamePair {
                          nameC = Name "struct1",
                          nameHsIdent = Identifier
                            "Struct1"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              [
                                "macro_in_fundecl_vs_typedef.h"],
                            headerInclude =
                            "macro_in_fundecl_vs_typedef.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Struct1"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "macro_in_fundecl_vs_typedef.h:18:30",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "struct1_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "struct1",
                      commentLocation = Just
                        "macro_in_fundecl_vs_typedef.h:18:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            [
                              "macro_in_fundecl_vs_typedef.h"],
                          headerInclude =
                          "macro_in_fundecl_vs_typedef.h"},
                      commentChildren = []}}
                (Add 1)
                (Seq
                  [
                    PokeByteOff
                      (Idx 2)
                      0
                      (Idx 0)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Struct1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Struct1",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Struct2",
      structConstr = Name
        "@NsConstr"
        "Struct2",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "struct2_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "macro_in_fundecl_vs_typedef.h:19:30",
                fieldName = NamePair {
                  nameC = Name "a",
                  nameHsIdent = Identifier
                    "struct2_a"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "a",
              commentLocation = Just
                "macro_in_fundecl_vs_typedef.h:19:30",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "macro_in_fundecl_vs_typedef.h"],
                  headerInclude =
                  "macro_in_fundecl_vs_typedef.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "macro_in_fundecl_vs_typedef.h:19:9",
            declId = NamePair {
              nameC = Name "struct2",
              nameHsIdent = Identifier
                "Struct2"},
            declOrigin = NameOriginGenerated
              (AnonId
                "macro_in_fundecl_vs_typedef.h:19:9"),
            declAliases = [Name "struct2"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  [
                    "macro_in_fundecl_vs_typedef.h"],
                headerInclude =
                "macro_in_fundecl_vs_typedef.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Struct2"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "macro_in_fundecl_vs_typedef.h:19:30",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "struct2_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecModule = Nothing,
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:19:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Struct2",
          structConstr = Name
            "@NsConstr"
            "Struct2",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "struct2_a",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "macro_in_fundecl_vs_typedef.h:19:30",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "struct2_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "a",
                  commentLocation = Just
                    "macro_in_fundecl_vs_typedef.h:19:30",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "macro_in_fundecl_vs_typedef.h"],
                      headerInclude =
                      "macro_in_fundecl_vs_typedef.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "macro_in_fundecl_vs_typedef.h:19:9",
                declId = NamePair {
                  nameC = Name "struct2",
                  nameHsIdent = Identifier
                    "Struct2"},
                declOrigin = NameOriginGenerated
                  (AnonId
                    "macro_in_fundecl_vs_typedef.h:19:9"),
                declAliases = [Name "struct2"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      [
                        "macro_in_fundecl_vs_typedef.h"],
                    headerInclude =
                    "macro_in_fundecl_vs_typedef.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Struct2"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "macro_in_fundecl_vs_typedef.h:19:30",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = Identifier
                            "struct2_a"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecModule = Nothing,
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Nothing,
              commentLocation = Just
                "macro_in_fundecl_vs_typedef.h:19:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "macro_in_fundecl_vs_typedef.h"],
                  headerInclude =
                  "macro_in_fundecl_vs_typedef.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 4,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Struct2",
                  structConstr = Name
                    "@NsConstr"
                    "Struct2",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "struct2_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "macro_in_fundecl_vs_typedef.h:19:30",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "struct2_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "macro_in_fundecl_vs_typedef.h:19:30",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "macro_in_fundecl_vs_typedef.h"],
                              headerInclude =
                              "macro_in_fundecl_vs_typedef.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "macro_in_fundecl_vs_typedef.h:19:9",
                        declId = NamePair {
                          nameC = Name "struct2",
                          nameHsIdent = Identifier
                            "Struct2"},
                        declOrigin = NameOriginGenerated
                          (AnonId
                            "macro_in_fundecl_vs_typedef.h:19:9"),
                        declAliases = [Name "struct2"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              [
                                "macro_in_fundecl_vs_typedef.h"],
                            headerInclude =
                            "macro_in_fundecl_vs_typedef.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Struct2"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "macro_in_fundecl_vs_typedef.h:19:30",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "struct2_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "macro_in_fundecl_vs_typedef.h:19:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            [
                              "macro_in_fundecl_vs_typedef.h"],
                          headerInclude =
                          "macro_in_fundecl_vs_typedef.h"},
                      commentChildren = []}})
              [PeekByteOff (Idx 0) 0]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Struct2",
                  structConstr = Name
                    "@NsConstr"
                    "Struct2",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "struct2_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "macro_in_fundecl_vs_typedef.h:19:30",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "struct2_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "macro_in_fundecl_vs_typedef.h:19:30",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "macro_in_fundecl_vs_typedef.h"],
                              headerInclude =
                              "macro_in_fundecl_vs_typedef.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "macro_in_fundecl_vs_typedef.h:19:9",
                        declId = NamePair {
                          nameC = Name "struct2",
                          nameHsIdent = Identifier
                            "Struct2"},
                        declOrigin = NameOriginGenerated
                          (AnonId
                            "macro_in_fundecl_vs_typedef.h:19:9"),
                        declAliases = [Name "struct2"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              [
                                "macro_in_fundecl_vs_typedef.h"],
                            headerInclude =
                            "macro_in_fundecl_vs_typedef.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Struct2"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "macro_in_fundecl_vs_typedef.h:19:30",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "struct2_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "macro_in_fundecl_vs_typedef.h:19:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            [
                              "macro_in_fundecl_vs_typedef.h"],
                          headerInclude =
                          "macro_in_fundecl_vs_typedef.h"},
                      commentChildren = []}}
                (Add 1)
                (Seq
                  [
                    PokeByteOff
                      (Idx 2)
                      0
                      (Idx 0)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Struct2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Struct2",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Struct3",
      structConstr = Name
        "@NsConstr"
        "Struct3",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "struct3_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "macro_in_fundecl_vs_typedef.h:20:30",
                fieldName = NamePair {
                  nameC = Name "a",
                  nameHsIdent = Identifier
                    "struct3_a"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "a",
              commentLocation = Just
                "macro_in_fundecl_vs_typedef.h:20:30",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "macro_in_fundecl_vs_typedef.h"],
                  headerInclude =
                  "macro_in_fundecl_vs_typedef.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "macro_in_fundecl_vs_typedef.h:20:16",
            declId = NamePair {
              nameC = Name "struct3",
              nameHsIdent = Identifier
                "Struct3"},
            declOrigin = NameOriginInSource,
            declAliases = [
              Name "struct3_t"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  [
                    "macro_in_fundecl_vs_typedef.h"],
                headerInclude =
                "macro_in_fundecl_vs_typedef.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Struct3"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "macro_in_fundecl_vs_typedef.h:20:30",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "struct3_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecModule = Nothing,
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "struct3",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:20:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Struct3",
          structConstr = Name
            "@NsConstr"
            "Struct3",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "struct3_a",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "macro_in_fundecl_vs_typedef.h:20:30",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "struct3_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "a",
                  commentLocation = Just
                    "macro_in_fundecl_vs_typedef.h:20:30",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "macro_in_fundecl_vs_typedef.h"],
                      headerInclude =
                      "macro_in_fundecl_vs_typedef.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "macro_in_fundecl_vs_typedef.h:20:16",
                declId = NamePair {
                  nameC = Name "struct3",
                  nameHsIdent = Identifier
                    "Struct3"},
                declOrigin = NameOriginInSource,
                declAliases = [
                  Name "struct3_t"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      [
                        "macro_in_fundecl_vs_typedef.h"],
                    headerInclude =
                    "macro_in_fundecl_vs_typedef.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Struct3"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "macro_in_fundecl_vs_typedef.h:20:30",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = Identifier
                            "struct3_a"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecModule = Nothing,
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "struct3",
              commentLocation = Just
                "macro_in_fundecl_vs_typedef.h:20:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "macro_in_fundecl_vs_typedef.h"],
                  headerInclude =
                  "macro_in_fundecl_vs_typedef.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 4,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Struct3",
                  structConstr = Name
                    "@NsConstr"
                    "Struct3",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "struct3_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "macro_in_fundecl_vs_typedef.h:20:30",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "struct3_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "macro_in_fundecl_vs_typedef.h:20:30",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "macro_in_fundecl_vs_typedef.h"],
                              headerInclude =
                              "macro_in_fundecl_vs_typedef.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "macro_in_fundecl_vs_typedef.h:20:16",
                        declId = NamePair {
                          nameC = Name "struct3",
                          nameHsIdent = Identifier
                            "Struct3"},
                        declOrigin = NameOriginInSource,
                        declAliases = [
                          Name "struct3_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              [
                                "macro_in_fundecl_vs_typedef.h"],
                            headerInclude =
                            "macro_in_fundecl_vs_typedef.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Struct3"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "macro_in_fundecl_vs_typedef.h:20:30",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "struct3_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "struct3",
                      commentLocation = Just
                        "macro_in_fundecl_vs_typedef.h:20:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            [
                              "macro_in_fundecl_vs_typedef.h"],
                          headerInclude =
                          "macro_in_fundecl_vs_typedef.h"},
                      commentChildren = []}})
              [PeekByteOff (Idx 0) 0]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Struct3",
                  structConstr = Name
                    "@NsConstr"
                    "Struct3",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "struct3_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "macro_in_fundecl_vs_typedef.h:20:30",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "struct3_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "macro_in_fundecl_vs_typedef.h:20:30",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "macro_in_fundecl_vs_typedef.h"],
                              headerInclude =
                              "macro_in_fundecl_vs_typedef.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "macro_in_fundecl_vs_typedef.h:20:16",
                        declId = NamePair {
                          nameC = Name "struct3",
                          nameHsIdent = Identifier
                            "Struct3"},
                        declOrigin = NameOriginInSource,
                        declAliases = [
                          Name "struct3_t"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              [
                                "macro_in_fundecl_vs_typedef.h"],
                            headerInclude =
                            "macro_in_fundecl_vs_typedef.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Struct3"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "macro_in_fundecl_vs_typedef.h:20:30",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "struct3_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "struct3",
                      commentLocation = Just
                        "macro_in_fundecl_vs_typedef.h:20:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            [
                              "macro_in_fundecl_vs_typedef.h"],
                          headerInclude =
                          "macro_in_fundecl_vs_typedef.h"},
                      commentChildren = []}}
                (Add 1)
                (Seq
                  [
                    PokeByteOff
                      (Idx 2)
                      0
                      (Idx 0)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Struct3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Struct3",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Struct3_t",
      newtypeConstr = Name
        "@NsConstr"
        "Struct3_t",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Struct3_t",
        fieldType = HsTypRef
          (Name
            "@NsTypeConstr"
            "Struct3"),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "macro_in_fundecl_vs_typedef.h:20:35",
          declId = NamePair {
            nameC = Name "struct3_t",
            nameHsIdent = Identifier
              "Struct3_t"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Struct3_t",
              newtypeField = Name
                "@NsVar"
                "un_Struct3_t"},
            typedefType = TypeStruct
              NamePair {
                nameC = Name "struct3",
                nameHsIdent = Identifier
                  "Struct3"}
              NameOriginInSource},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = Nothing,
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "struct3_t",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:20:35",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Struct3_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Struct3_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Struct3_t",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Struct4",
      structConstr = Name
        "@NsConstr"
        "Struct4",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "struct4_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "macro_in_fundecl_vs_typedef.h:21:30",
                fieldName = NamePair {
                  nameC = Name "a",
                  nameHsIdent = Identifier
                    "struct4_a"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "a",
              commentLocation = Just
                "macro_in_fundecl_vs_typedef.h:21:30",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "macro_in_fundecl_vs_typedef.h"],
                  headerInclude =
                  "macro_in_fundecl_vs_typedef.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "macro_in_fundecl_vs_typedef.h:21:16",
            declId = NamePair {
              nameC = Name "struct4",
              nameHsIdent = Identifier
                "Struct4"},
            declOrigin = NameOriginInSource,
            declAliases = [Name "struct4"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  [
                    "macro_in_fundecl_vs_typedef.h"],
                headerInclude =
                "macro_in_fundecl_vs_typedef.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Struct4"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "macro_in_fundecl_vs_typedef.h:21:30",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "struct4_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecModule = Nothing,
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "struct4",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:21:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Struct4",
          structConstr = Name
            "@NsConstr"
            "Struct4",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "struct4_a",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "macro_in_fundecl_vs_typedef.h:21:30",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = Identifier
                        "struct4_a"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "a",
                  commentLocation = Just
                    "macro_in_fundecl_vs_typedef.h:21:30",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "macro_in_fundecl_vs_typedef.h"],
                      headerInclude =
                      "macro_in_fundecl_vs_typedef.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "macro_in_fundecl_vs_typedef.h:21:16",
                declId = NamePair {
                  nameC = Name "struct4",
                  nameHsIdent = Identifier
                    "Struct4"},
                declOrigin = NameOriginInSource,
                declAliases = [Name "struct4"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      [
                        "macro_in_fundecl_vs_typedef.h"],
                    headerInclude =
                    "macro_in_fundecl_vs_typedef.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Struct4"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "macro_in_fundecl_vs_typedef.h:21:30",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = Identifier
                            "struct4_a"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecModule = Nothing,
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "struct4",
              commentLocation = Just
                "macro_in_fundecl_vs_typedef.h:21:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "macro_in_fundecl_vs_typedef.h"],
                  headerInclude =
                  "macro_in_fundecl_vs_typedef.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 4,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Struct4",
                  structConstr = Name
                    "@NsConstr"
                    "Struct4",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "struct4_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "macro_in_fundecl_vs_typedef.h:21:30",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "struct4_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "macro_in_fundecl_vs_typedef.h:21:30",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "macro_in_fundecl_vs_typedef.h"],
                              headerInclude =
                              "macro_in_fundecl_vs_typedef.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "macro_in_fundecl_vs_typedef.h:21:16",
                        declId = NamePair {
                          nameC = Name "struct4",
                          nameHsIdent = Identifier
                            "Struct4"},
                        declOrigin = NameOriginInSource,
                        declAliases = [Name "struct4"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              [
                                "macro_in_fundecl_vs_typedef.h"],
                            headerInclude =
                            "macro_in_fundecl_vs_typedef.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Struct4"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "macro_in_fundecl_vs_typedef.h:21:30",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "struct4_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "struct4",
                      commentLocation = Just
                        "macro_in_fundecl_vs_typedef.h:21:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            [
                              "macro_in_fundecl_vs_typedef.h"],
                          headerInclude =
                          "macro_in_fundecl_vs_typedef.h"},
                      commentChildren = []}})
              [PeekByteOff (Idx 0) 0]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Struct4",
                  structConstr = Name
                    "@NsConstr"
                    "Struct4",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "struct4_a",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "macro_in_fundecl_vs_typedef.h:21:30",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = Identifier
                                "struct4_a"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "a",
                          commentLocation = Just
                            "macro_in_fundecl_vs_typedef.h:21:30",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "macro_in_fundecl_vs_typedef.h"],
                              headerInclude =
                              "macro_in_fundecl_vs_typedef.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "macro_in_fundecl_vs_typedef.h:21:16",
                        declId = NamePair {
                          nameC = Name "struct4",
                          nameHsIdent = Identifier
                            "Struct4"},
                        declOrigin = NameOriginInSource,
                        declAliases = [Name "struct4"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              [
                                "macro_in_fundecl_vs_typedef.h"],
                            headerInclude =
                            "macro_in_fundecl_vs_typedef.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Struct4"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "macro_in_fundecl_vs_typedef.h:21:30",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = Identifier
                                    "struct4_a"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "struct4",
                      commentLocation = Just
                        "macro_in_fundecl_vs_typedef.h:21:16",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            [
                              "macro_in_fundecl_vs_typedef.h"],
                          headerInclude =
                          "macro_in_fundecl_vs_typedef.h"},
                      commentChildren = []}}
                (Add 1)
                (Seq
                  [
                    PokeByteOff
                      (Idx 2)
                      0
                      (Idx 0)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Struct4",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Struct4",
      deriveInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "quux1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "MC"),
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
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "TC"),
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
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_c7ba346f3006b36f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "char hs_bindgen_test_macro_in_fundecl_vs_typedef_c7ba346f3006b36f (\n",
              "  MC arg1,\n",
              "  TC arg2\n",
              ")\n",
              "{\n",
              "  return quux1(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
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
                  nameC = Name "MC",
                  nameHsIdent = Identifier "MC"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier "y"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "TC",
                    nameHsIdent = Identifier "TC"}
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimChar
              (PrimSignImplicit Nothing))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "quux1",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:8:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "quux2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "MC"),
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
        (HsIO
          (HsTypRef
            (Name "@NsTypeConstr" "TC"))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_db114519a8645d1f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "TC hs_bindgen_test_macro_in_fundecl_vs_typedef_db114519a8645d1f (\n",
              "  MC arg1,\n",
              "  char arg2\n",
              ")\n",
              "{\n",
              "  return quux2(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
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
                  nameC = Name "MC",
                  nameHsIdent = Identifier "MC"}
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
          functionRes = TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = Name "TC",
                nameHsIdent = Identifier "TC"}
              (TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed)))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "quux2",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:9:4",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "wam1",
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
              (Name "@NsTypeConstr" "TC")),
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
              (Name "@NsTypeConstr" "MC")))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_0a613fb26d413eaa",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "MC *hs_bindgen_test_macro_in_fundecl_vs_typedef_0a613fb26d413eaa (\n",
              "  float arg1,\n",
              "  TC *arg2\n",
              ")\n",
              "{\n",
              "  return wam1(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
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
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "TC",
                      nameHsIdent = Identifier "TC"}
                    (TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed)))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeMacroTypedef
              NamePair {
                nameC = Name "MC",
                nameHsIdent = Identifier "MC"}
              NameOriginInSource)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "wam1",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:10:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "wam2",
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
              (Name "@NsTypeConstr" "MC")),
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
              (Name "@NsTypeConstr" "TC")))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_279b15c6940eb4f8",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "TC *hs_bindgen_test_macro_in_fundecl_vs_typedef_279b15c6940eb4f8 (\n",
              "  float arg1,\n",
              "  MC *arg2\n",
              ")\n",
              "{\n",
              "  return wam2(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
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
                    nameC = Name "MC",
                    nameHsIdent = Identifier "MC"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "TC",
                  nameHsIdent = Identifier "TC"}
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "wam2",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:11:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "struct_typedef1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Struct2")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "s",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "MC"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_27a965a9bfd8c176",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_macro_in_fundecl_vs_typedef_27a965a9bfd8c176 (\n",
              "  struct2 *arg1,\n",
              "  MC arg2\n",
              ")\n",
              "{\n",
              "  struct_typedef1(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = Identifier "s"})
              (TypePointer
                (TypeTypedef
                  (TypedefSquashed
                    (Name "struct2")
                    (TypeStruct
                      NamePair {
                        nameC = Name "struct2",
                        nameHsIdent = Identifier
                          "Struct2"}
                      (NameOriginGenerated
                        (AnonId
                          "macro_in_fundecl_vs_typedef.h:19:9")))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = Identifier "MC"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "struct_typedef1",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:23:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "struct_typedef2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Struct3_t")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "s",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "MC"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_8e7c55302a7b5716",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_macro_in_fundecl_vs_typedef_8e7c55302a7b5716 (\n",
              "  struct3_t *arg1,\n",
              "  MC arg2\n",
              ")\n",
              "{\n",
              "  struct_typedef2(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = Identifier "s"})
              (TypePointer
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "struct3_t",
                      nameHsIdent = Identifier
                        "Struct3_t"}
                    (TypeStruct
                      NamePair {
                        nameC = Name "struct3",
                        nameHsIdent = Identifier
                          "Struct3"}
                      NameOriginInSource)))),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = Identifier "MC"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "struct_typedef2",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:24:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "struct_typedef3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Struct4")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "s",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "MC"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_fe83edb35a817050",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_macro_in_fundecl_vs_typedef_fe83edb35a817050 (\n",
              "  struct4 *arg1,\n",
              "  MC arg2\n",
              ")\n",
              "{\n",
              "  struct_typedef3(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = Identifier "s"})
              (TypePointer
                (TypeTypedef
                  (TypedefSquashed
                    (Name "struct4")
                    (TypeStruct
                      NamePair {
                        nameC = Name "struct4",
                        nameHsIdent = Identifier
                          "Struct4"}
                      NameOriginInSource)))),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = Identifier "MC"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "struct_typedef3",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:25:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "struct_name1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Struct1")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "s",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "MC"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_162df9fabcbef0c4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_macro_in_fundecl_vs_typedef_162df9fabcbef0c4 (\n",
              "  struct struct1 *arg1,\n",
              "  MC arg2\n",
              ")\n",
              "{\n",
              "  struct_name1(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = Identifier "s"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "struct1",
                    nameHsIdent = Identifier
                      "Struct1"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = Identifier "MC"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "struct_name1",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:27:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "struct_name2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Struct3")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "s",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "MC"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_a6b5f272333b19d4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_macro_in_fundecl_vs_typedef_a6b5f272333b19d4 (\n",
              "  struct struct3 *arg1,\n",
              "  MC arg2\n",
              ")\n",
              "{\n",
              "  struct_name2(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = Identifier "s"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "struct3",
                    nameHsIdent = Identifier
                      "Struct3"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = Identifier "MC"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "struct_name2",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:28:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "struct_name3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Struct4")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "s",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "MC"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_9eadc48880259e4a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_macro_in_fundecl_vs_typedef_9eadc48880259e4a (\n",
              "  struct struct4 *arg1,\n",
              "  MC arg2\n",
              ")\n",
              "{\n",
              "  struct_name3(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = Identifier "s"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "struct4",
                    nameHsIdent = Identifier
                      "Struct4"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = Identifier "MC"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "struct_name3",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:29:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "quux1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "MC"),
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
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "TC"),
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
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_d8e3530b80cd03ff",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "char hs_bindgen_test_macro_in_fundecl_vs_typedef_d8e3530b80cd03ff (\n",
              "  MC arg1,\n",
              "  TC arg2\n",
              ")\n",
              "{\n",
              "  return quux1(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
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
                  nameC = Name "MC",
                  nameHsIdent = Identifier "MC"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier "y"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "TC",
                    nameHsIdent = Identifier "TC"}
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimChar
              (PrimSignImplicit Nothing))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "quux1",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:8:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "quux2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "MC"),
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
        (HsIO
          (HsTypRef
            (Name "@NsTypeConstr" "TC"))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_857e3b8d0292d39d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "TC hs_bindgen_test_macro_in_fundecl_vs_typedef_857e3b8d0292d39d (\n",
              "  MC arg1,\n",
              "  char arg2\n",
              ")\n",
              "{\n",
              "  return quux2(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
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
                  nameC = Name "MC",
                  nameHsIdent = Identifier "MC"}
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
          functionRes = TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = Name "TC",
                nameHsIdent = Identifier "TC"}
              (TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed)))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "quux2",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:9:4",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "wam1",
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
              (Name "@NsTypeConstr" "TC")),
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
              (Name "@NsTypeConstr" "MC")))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_81d53aede4a7b424",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "MC *hs_bindgen_test_macro_in_fundecl_vs_typedef_81d53aede4a7b424 (\n",
              "  float arg1,\n",
              "  TC *arg2\n",
              ")\n",
              "{\n",
              "  return wam1(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
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
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "TC",
                      nameHsIdent = Identifier "TC"}
                    (TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed)))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeMacroTypedef
              NamePair {
                nameC = Name "MC",
                nameHsIdent = Identifier "MC"}
              NameOriginInSource)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "wam1",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:10:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "wam2",
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
              (Name "@NsTypeConstr" "MC")),
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
              (Name "@NsTypeConstr" "TC")))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_035abb3b83b92fea",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "TC *hs_bindgen_test_macro_in_fundecl_vs_typedef_035abb3b83b92fea (\n",
              "  float arg1,\n",
              "  MC *arg2\n",
              ")\n",
              "{\n",
              "  return wam2(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
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
                    nameC = Name "MC",
                    nameHsIdent = Identifier "MC"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "TC",
                  nameHsIdent = Identifier "TC"}
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "wam2",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:11:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "struct_typedef1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Struct2")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "s",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "MC"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_34300aead966e212",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_macro_in_fundecl_vs_typedef_34300aead966e212 (\n",
              "  struct2 *arg1,\n",
              "  MC arg2\n",
              ")\n",
              "{\n",
              "  struct_typedef1(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = Identifier "s"})
              (TypePointer
                (TypeTypedef
                  (TypedefSquashed
                    (Name "struct2")
                    (TypeStruct
                      NamePair {
                        nameC = Name "struct2",
                        nameHsIdent = Identifier
                          "Struct2"}
                      (NameOriginGenerated
                        (AnonId
                          "macro_in_fundecl_vs_typedef.h:19:9")))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = Identifier "MC"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "struct_typedef1",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:23:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "struct_typedef2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Struct3_t")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "s",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "MC"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_cc4abaeb55d1e034",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_macro_in_fundecl_vs_typedef_cc4abaeb55d1e034 (\n",
              "  struct3_t *arg1,\n",
              "  MC arg2\n",
              ")\n",
              "{\n",
              "  struct_typedef2(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = Identifier "s"})
              (TypePointer
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "struct3_t",
                      nameHsIdent = Identifier
                        "Struct3_t"}
                    (TypeStruct
                      NamePair {
                        nameC = Name "struct3",
                        nameHsIdent = Identifier
                          "Struct3"}
                      NameOriginInSource)))),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = Identifier "MC"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "struct_typedef2",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:24:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "struct_typedef3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Struct4")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "s",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "MC"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_d068247e5a3b03e6",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_macro_in_fundecl_vs_typedef_d068247e5a3b03e6 (\n",
              "  struct4 *arg1,\n",
              "  MC arg2\n",
              ")\n",
              "{\n",
              "  struct_typedef3(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = Identifier "s"})
              (TypePointer
                (TypeTypedef
                  (TypedefSquashed
                    (Name "struct4")
                    (TypeStruct
                      NamePair {
                        nameC = Name "struct4",
                        nameHsIdent = Identifier
                          "Struct4"}
                      NameOriginInSource)))),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = Identifier "MC"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "struct_typedef3",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:25:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "struct_name1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Struct1")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "s",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "MC"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_4107c49ec0f22d0c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_macro_in_fundecl_vs_typedef_4107c49ec0f22d0c (\n",
              "  struct struct1 *arg1,\n",
              "  MC arg2\n",
              ")\n",
              "{\n",
              "  struct_name1(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = Identifier "s"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "struct1",
                    nameHsIdent = Identifier
                      "Struct1"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = Identifier "MC"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "struct_name1",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:27:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "struct_name2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Struct3")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "s",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "MC"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_124c6e0ae4b86063",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_macro_in_fundecl_vs_typedef_124c6e0ae4b86063 (\n",
              "  struct struct3 *arg1,\n",
              "  MC arg2\n",
              ")\n",
              "{\n",
              "  struct_name2(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = Identifier "s"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "struct3",
                    nameHsIdent = Identifier
                      "Struct3"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = Identifier "MC"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "struct_name2",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:28:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "struct_name3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Struct4")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "s",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "MC"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_4241bc0cd1393d99",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_macro_in_fundecl_vs_typedef_4241bc0cd1393d99 (\n",
              "  struct struct4 *arg1,\n",
              "  MC arg2\n",
              ")\n",
              "{\n",
              "  struct_name3(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = Identifier "s"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "struct4",
                    nameHsIdent = Identifier
                      "Struct4"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = Identifier "MC"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "struct_name3",
          commentLocation = Just
            "macro_in_fundecl_vs_typedef.h:29:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "macro_in_fundecl_vs_typedef.h"],
              headerInclude =
              "macro_in_fundecl_vs_typedef.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_vs_typedef_7d7a63ab896ed293",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "MC"))
              (HsFun
                (HsTypRef
                  (Name "@NsTypeConstr" "TC"))
                (HsIO
                  (HsPrimType HsPrimCChar)))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_7d7a63ab896ed293",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_quux1_ptr */\n",
              "__attribute__ ((const))\n",
              "char (*hs_bindgen_test_macro_in_fundecl_vs_typedef_7d7a63ab896ed293 (void)) (\n",
              "  MC arg1,\n",
              "  TC arg2\n",
              ")\n",
              "{\n",
              "  return &quux1;\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "MC",
                nameHsIdent = Identifier "MC"}
              NameOriginInSource,
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "TC",
                  nameHsIdent = Identifier "TC"}
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))))]
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
        "hs_bindgen_test_macro_in_fundecl_vs_typedef_b64c564dd7071f5b",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "MC"))
              (HsFun
                (HsPrimType HsPrimCChar)
                (HsIO
                  (HsTypRef
                    (Name
                      "@NsTypeConstr"
                      "TC"))))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_b64c564dd7071f5b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_quux2_ptr */\n",
              "__attribute__ ((const))\n",
              "TC (*hs_bindgen_test_macro_in_fundecl_vs_typedef_b64c564dd7071f5b (void)) (\n",
              "  MC arg1,\n",
              "  char arg2\n",
              ")\n",
              "{\n",
              "  return &quux2;\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "MC",
                nameHsIdent = Identifier "MC"}
              NameOriginInSource,
            TypePrim
              (PrimChar
                (PrimSignImplicit Nothing))]
          (TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = Name "TC",
                nameHsIdent = Identifier "TC"}
              (TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_vs_typedef_aa26b3a0f4d0aefe",
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
                    (Name "@NsTypeConstr" "TC")))
                (HsIO
                  (HsPtr
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "MC")))))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_aa26b3a0f4d0aefe",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_wam1_ptr */\n",
              "__attribute__ ((const))\n",
              "MC *(*hs_bindgen_test_macro_in_fundecl_vs_typedef_aa26b3a0f4d0aefe (void)) (\n",
              "  float arg1,\n",
              "  TC *arg2\n",
              ")\n",
              "{\n",
              "  return &wam1;\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimFloating PrimFloat),
            TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "TC",
                    nameHsIdent = Identifier "TC"}
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))))))]
          (TypePointer
            (TypeMacroTypedef
              NamePair {
                nameC = Name "MC",
                nameHsIdent = Identifier "MC"}
              NameOriginInSource))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_vs_typedef_5cb5ead73c0a3d63",
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
                    (Name "@NsTypeConstr" "MC")))
                (HsIO
                  (HsPtr
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "TC")))))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_5cb5ead73c0a3d63",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_wam2_ptr */\n",
              "__attribute__ ((const))\n",
              "TC *(*hs_bindgen_test_macro_in_fundecl_vs_typedef_5cb5ead73c0a3d63 (void)) (\n",
              "  float arg1,\n",
              "  MC *arg2\n",
              ")\n",
              "{\n",
              "  return &wam2;\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimFloating PrimFloat),
            TypePointer
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = Identifier "MC"}
                NameOriginInSource)]
          (TypePointer
            (TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "TC",
                  nameHsIdent = Identifier "TC"}
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_vs_typedef_a1aadeb6878a5152",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Struct2")))
              (HsFun
                (HsTypRef
                  (Name "@NsTypeConstr" "MC"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_a1aadeb6878a5152",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_struct_typedef1_ptr */\n",
              "__attribute__ ((const))\n",
              "void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_a1aadeb6878a5152 (void)) (\n",
              "  struct2 *arg1,\n",
              "  MC arg2\n",
              ")\n",
              "{\n",
              "  return &struct_typedef1;\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeTypedef
                (TypedefSquashed
                  (Name "struct2")
                  (TypeStruct
                    NamePair {
                      nameC = Name "struct2",
                      nameHsIdent = Identifier
                        "Struct2"}
                    (NameOriginGenerated
                      (AnonId
                        "macro_in_fundecl_vs_typedef.h:19:9"))))),
            TypeMacroTypedef
              NamePair {
                nameC = Name "MC",
                nameHsIdent = Identifier "MC"}
              NameOriginInSource]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_vs_typedef_e1dac8a006e6b043",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Struct3_t")))
              (HsFun
                (HsTypRef
                  (Name "@NsTypeConstr" "MC"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_e1dac8a006e6b043",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_struct_typedef2_ptr */\n",
              "__attribute__ ((const))\n",
              "void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_e1dac8a006e6b043 (void)) (\n",
              "  struct3_t *arg1,\n",
              "  MC arg2\n",
              ")\n",
              "{\n",
              "  return &struct_typedef2;\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "struct3_t",
                    nameHsIdent = Identifier
                      "Struct3_t"}
                  (TypeStruct
                    NamePair {
                      nameC = Name "struct3",
                      nameHsIdent = Identifier
                        "Struct3"}
                    NameOriginInSource))),
            TypeMacroTypedef
              NamePair {
                nameC = Name "MC",
                nameHsIdent = Identifier "MC"}
              NameOriginInSource]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_vs_typedef_078075d0a80d4368",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Struct4")))
              (HsFun
                (HsTypRef
                  (Name "@NsTypeConstr" "MC"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_078075d0a80d4368",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_struct_typedef3_ptr */\n",
              "__attribute__ ((const))\n",
              "void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_078075d0a80d4368 (void)) (\n",
              "  struct4 *arg1,\n",
              "  MC arg2\n",
              ")\n",
              "{\n",
              "  return &struct_typedef3;\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeTypedef
                (TypedefSquashed
                  (Name "struct4")
                  (TypeStruct
                    NamePair {
                      nameC = Name "struct4",
                      nameHsIdent = Identifier
                        "Struct4"}
                    NameOriginInSource))),
            TypeMacroTypedef
              NamePair {
                nameC = Name "MC",
                nameHsIdent = Identifier "MC"}
              NameOriginInSource]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_vs_typedef_7574edf86480f042",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Struct1")))
              (HsFun
                (HsTypRef
                  (Name "@NsTypeConstr" "MC"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_7574edf86480f042",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_struct_name1_ptr */\n",
              "__attribute__ ((const))\n",
              "void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_7574edf86480f042 (void)) (\n",
              "  struct struct1 *arg1,\n",
              "  MC arg2\n",
              ")\n",
              "{\n",
              "  return &struct_name1;\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name "struct1",
                  nameHsIdent = Identifier
                    "Struct1"}
                NameOriginInSource),
            TypeMacroTypedef
              NamePair {
                nameC = Name "MC",
                nameHsIdent = Identifier "MC"}
              NameOriginInSource]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_vs_typedef_e7a8c1f45f8b20c2",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Struct3")))
              (HsFun
                (HsTypRef
                  (Name "@NsTypeConstr" "MC"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_e7a8c1f45f8b20c2",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_struct_name2_ptr */\n",
              "__attribute__ ((const))\n",
              "void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_e7a8c1f45f8b20c2 (void)) (\n",
              "  struct struct3 *arg1,\n",
              "  MC arg2\n",
              ")\n",
              "{\n",
              "  return &struct_name2;\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name "struct3",
                  nameHsIdent = Identifier
                    "Struct3"}
                NameOriginInSource),
            TypeMacroTypedef
              NamePair {
                nameC = Name "MC",
                nameHsIdent = Identifier "MC"}
              NameOriginInSource]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_vs_typedef_d52310663e8daa5c",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr
                (HsTypRef
                  (Name
                    "@NsTypeConstr"
                    "Struct4")))
              (HsFun
                (HsTypRef
                  (Name "@NsTypeConstr" "MC"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_d52310663e8daa5c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_struct_name3_ptr */\n",
              "__attribute__ ((const))\n",
              "void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_d52310663e8daa5c (void)) (\n",
              "  struct struct4 *arg1,\n",
              "  MC arg2\n",
              ")\n",
              "{\n",
              "  return &struct_name3;\n",
              "}"],
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name "struct4",
                  nameHsIdent = Identifier
                    "Struct4"}
                NameOriginInSource),
            TypeMacroTypedef
              NamePair {
                nameC = Name "MC",
                nameHsIdent = Identifier "MC"}
              NameOriginInSource]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
