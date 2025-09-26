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
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "macro_in_fundecl_vs_typedef.h:4:9",
          declId = NamePair {
            nameC = Name "MC",
            nameHsIdent = HsIdentifier
              "MC"},
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MC",
      deriveInstanceComment =
      Nothing},
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
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "macro_in_fundecl_vs_typedef.h:5:14",
          declId = NamePair {
            nameC = Name "TC",
            nameHsIdent = HsIdentifier
              "TC"},
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "TC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "TC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "TC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "TC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "TC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "TC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "TC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "TC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "TC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "TC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "TC",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "TC",
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
      structFields = [
        Field {
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
              nameHsIdent = HsIdentifier
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
                (HsName "@NsConstr" "Struct1"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "macro_in_fundecl_vs_typedef.h:18:30",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = HsIdentifier
                        "struct1_a"},
                    fieldComment = Nothing},
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
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "macro_in_fundecl_vs_typedef.h:18:30",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = HsIdentifier
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
                  nameHsIdent = HsIdentifier
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
                    (HsName "@NsConstr" "Struct1"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "macro_in_fundecl_vs_typedef.h:18:30",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = HsIdentifier
                            "struct1_a"},
                        fieldComment = Nothing},
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
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "macro_in_fundecl_vs_typedef.h:18:30",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
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
                            (HsName "@NsConstr" "Struct1"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "macro_in_fundecl_vs_typedef.h:18:30",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
                                    "struct1_a"},
                                fieldComment = Nothing},
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
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "macro_in_fundecl_vs_typedef.h:18:30",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
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
                            (HsName "@NsConstr" "Struct1"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "macro_in_fundecl_vs_typedef.h:18:30",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
                                    "struct1_a"},
                                fieldComment = Nothing},
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
              structFieldInfo = FieldInfo {
                fieldLoc =
                "macro_in_fundecl_vs_typedef.h:19:30",
                fieldName = NamePair {
                  nameC = Name "a",
                  nameHsIdent = HsIdentifier
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
              nameHsIdent = HsIdentifier
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
                (HsName "@NsConstr" "Struct2"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "macro_in_fundecl_vs_typedef.h:19:30",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = HsIdentifier
                        "struct2_a"},
                    fieldComment = Nothing},
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
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "struct2",
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
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "macro_in_fundecl_vs_typedef.h:19:30",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = HsIdentifier
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
                  nameHsIdent = HsIdentifier
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
                    (HsName "@NsConstr" "Struct2"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "macro_in_fundecl_vs_typedef.h:19:30",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = HsIdentifier
                            "struct2_a"},
                        fieldComment = Nothing},
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
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "struct2",
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
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "macro_in_fundecl_vs_typedef.h:19:30",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
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
                            (HsName "@NsConstr" "Struct2"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "macro_in_fundecl_vs_typedef.h:19:30",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
                                    "struct2_a"},
                                fieldComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "struct2",
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
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "macro_in_fundecl_vs_typedef.h:19:30",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
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
                            (HsName "@NsConstr" "Struct2"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "macro_in_fundecl_vs_typedef.h:19:30",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
                                    "struct2_a"},
                                fieldComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "struct2",
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Struct2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Struct2",
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
      structFields = [
        Field {
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
              nameHsIdent = HsIdentifier
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
                (HsName "@NsConstr" "Struct3"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "macro_in_fundecl_vs_typedef.h:20:30",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = HsIdentifier
                        "struct3_a"},
                    fieldComment = Nothing},
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
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "macro_in_fundecl_vs_typedef.h:20:30",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = HsIdentifier
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
                  nameHsIdent = HsIdentifier
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
                    (HsName "@NsConstr" "Struct3"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "macro_in_fundecl_vs_typedef.h:20:30",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = HsIdentifier
                            "struct3_a"},
                        fieldComment = Nothing},
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
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "macro_in_fundecl_vs_typedef.h:20:30",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
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
                            (HsName "@NsConstr" "Struct3"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "macro_in_fundecl_vs_typedef.h:20:30",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
                                    "struct3_a"},
                                fieldComment = Nothing},
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
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "macro_in_fundecl_vs_typedef.h:20:30",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
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
                            (HsName "@NsConstr" "Struct3"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "macro_in_fundecl_vs_typedef.h:20:30",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
                                    "struct3_a"},
                                fieldComment = Nothing},
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
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "macro_in_fundecl_vs_typedef.h:20:35",
          declId = NamePair {
            nameC = Name "struct3_t",
            nameHsIdent = HsIdentifier
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
              newtypeConstr = HsName
                "@NsConstr"
                "Struct3_t",
              newtypeField = HsName
                "@NsVar"
                "un_Struct3_t"},
            typedefType = TypeStruct
              NamePair {
                nameC = Name "struct3",
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Struct3_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Struct3_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Struct3_t",
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
      structFields = [
        Field {
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
              nameHsIdent = HsIdentifier
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
                (HsName "@NsConstr" "Struct4"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "macro_in_fundecl_vs_typedef.h:21:30",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = HsIdentifier
                        "struct4_a"},
                    fieldComment = Nothing},
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
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "macro_in_fundecl_vs_typedef.h:21:30",
                    fieldName = NamePair {
                      nameC = Name "a",
                      nameHsIdent = HsIdentifier
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
                  nameHsIdent = HsIdentifier
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
                    (HsName "@NsConstr" "Struct4"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "macro_in_fundecl_vs_typedef.h:21:30",
                        fieldName = NamePair {
                          nameC = Name "a",
                          nameHsIdent = HsIdentifier
                            "struct4_a"},
                        fieldComment = Nothing},
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
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "macro_in_fundecl_vs_typedef.h:21:30",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
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
                            (HsName "@NsConstr" "Struct4"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "macro_in_fundecl_vs_typedef.h:21:30",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
                                    "struct4_a"},
                                fieldComment = Nothing},
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
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "macro_in_fundecl_vs_typedef.h:21:30",
                            fieldName = NamePair {
                              nameC = Name "a",
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
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
                            (HsName "@NsConstr" "Struct4"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "macro_in_fundecl_vs_typedef.h:21:30",
                                fieldName = NamePair {
                                  nameC = Name "a",
                                  nameHsIdent = HsIdentifier
                                    "struct4_a"},
                                fieldComment = Nothing},
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
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "quux1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "MC"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "y"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "TC"),
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
          "char hs_bindgen_test_macro_in_fundecl_vs_typedef_c7ba346f3006b36f (MC arg1, TC arg2) { return quux1(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
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
                  nameC = Name "MC",
                  nameHsIdent = HsIdentifier "MC"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier "y"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "TC",
                    nameHsIdent = HsIdentifier
                      "TC"}))],
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
      foreignImportName = HsName
        "@NsVar"
        "quux2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "MC"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName "@NsTypeConstr" "TC"))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_db114519a8645d1f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "TC hs_bindgen_test_macro_in_fundecl_vs_typedef_db114519a8645d1f (MC arg1, char arg2) { return quux2(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
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
                  nameC = Name "MC",
                  nameHsIdent = HsIdentifier "MC"}
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
          functionRes = TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = Name "TC",
                nameHsIdent = HsIdentifier
                  "TC"})},
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
      foreignImportName = HsName
        "@NsVar"
        "wam1",
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
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "y"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "TC")),
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
              (HsName
                "@NsTypeConstr"
                "MC")))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_0a613fb26d413eaa",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "MC *hs_bindgen_test_macro_in_fundecl_vs_typedef_0a613fb26d413eaa (float arg1, TC *arg2) { return wam1(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
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
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "TC",
                      nameHsIdent = HsIdentifier
                        "TC"})))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeMacroTypedef
              NamePair {
                nameC = Name "MC",
                nameHsIdent = HsIdentifier "MC"}
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
      foreignImportName = HsName
        "@NsVar"
        "wam2",
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
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "y"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "MC")),
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
              (HsName
                "@NsTypeConstr"
                "TC")))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_279b15c6940eb4f8",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "TC *hs_bindgen_test_macro_in_fundecl_vs_typedef_279b15c6940eb4f8 (float arg1, MC *arg2) { return wam2(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
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
                    nameC = Name "MC",
                    nameHsIdent = HsIdentifier "MC"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "TC",
                  nameHsIdent = HsIdentifier
                    "TC"}))},
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
      foreignImportName = HsName
        "@NsVar"
        "struct_typedef1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
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
            (HsName "@NsVar" "x"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "MC"),
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
          "void hs_bindgen_test_macro_in_fundecl_vs_typedef_27a965a9bfd8c176 (struct2 *arg1, MC arg2) { struct_typedef1(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = HsIdentifier "s"})
              (TypePointer
                (TypeTypedef
                  (TypedefSquashed
                    (Name "struct2")
                    (TypeStruct
                      NamePair {
                        nameC = Name "struct2",
                        nameHsIdent = HsIdentifier
                          "Struct2"}
                      (NameOriginGenerated
                        (AnonId
                          "macro_in_fundecl_vs_typedef.h:19:9")))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = HsIdentifier "MC"}
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
      foreignImportName = HsName
        "@NsVar"
        "struct_typedef2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
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
            (HsName "@NsVar" "x"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "MC"),
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
          "void hs_bindgen_test_macro_in_fundecl_vs_typedef_8e7c55302a7b5716 (struct3_t *arg1, MC arg2) { struct_typedef2(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = HsIdentifier "s"})
              (TypePointer
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "struct3_t",
                      nameHsIdent = HsIdentifier
                        "Struct3_t"}))),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = HsIdentifier "MC"}
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
      foreignImportName = HsName
        "@NsVar"
        "struct_typedef3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
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
            (HsName "@NsVar" "x"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "MC"),
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
          "void hs_bindgen_test_macro_in_fundecl_vs_typedef_fe83edb35a817050 (struct4 *arg1, MC arg2) { struct_typedef3(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = HsIdentifier "s"})
              (TypePointer
                (TypeTypedef
                  (TypedefSquashed
                    (Name "struct4")
                    (TypeStruct
                      NamePair {
                        nameC = Name "struct4",
                        nameHsIdent = HsIdentifier
                          "Struct4"}
                      NameOriginInSource)))),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = HsIdentifier "MC"}
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
      foreignImportName = HsName
        "@NsVar"
        "struct_name1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
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
            (HsName "@NsVar" "x"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "MC"),
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
          "void hs_bindgen_test_macro_in_fundecl_vs_typedef_162df9fabcbef0c4 (struct struct1 *arg1, MC arg2) { struct_name1(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = HsIdentifier "s"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "struct1",
                    nameHsIdent = HsIdentifier
                      "Struct1"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = HsIdentifier "MC"}
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
      foreignImportName = HsName
        "@NsVar"
        "struct_name2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
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
            (HsName "@NsVar" "x"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "MC"),
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
          "void hs_bindgen_test_macro_in_fundecl_vs_typedef_a6b5f272333b19d4 (struct struct3 *arg1, MC arg2) { struct_name2(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = HsIdentifier "s"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "struct3",
                    nameHsIdent = HsIdentifier
                      "Struct3"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = HsIdentifier "MC"}
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
      foreignImportName = HsName
        "@NsVar"
        "struct_name3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
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
            (HsName "@NsVar" "x"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "MC"),
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
          "void hs_bindgen_test_macro_in_fundecl_vs_typedef_9eadc48880259e4a (struct struct4 *arg1, MC arg2) { struct_name3(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = HsIdentifier "s"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "struct4",
                    nameHsIdent = HsIdentifier
                      "Struct4"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = HsIdentifier "MC"}
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
      foreignImportName = HsName
        "@NsVar"
        "quux1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "MC"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "y"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "TC"),
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
          "char hs_bindgen_test_macro_in_fundecl_vs_typedef_d8e3530b80cd03ff (MC arg1, TC arg2) { return quux1(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
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
                  nameC = Name "MC",
                  nameHsIdent = HsIdentifier "MC"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier "y"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "TC",
                    nameHsIdent = HsIdentifier
                      "TC"}))],
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
      foreignImportName = HsName
        "@NsVar"
        "quux2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "MC"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName "@NsTypeConstr" "TC"))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_857e3b8d0292d39d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "TC hs_bindgen_test_macro_in_fundecl_vs_typedef_857e3b8d0292d39d (MC arg1, char arg2) { return quux2(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
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
                  nameC = Name "MC",
                  nameHsIdent = HsIdentifier "MC"}
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
          functionRes = TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = Name "TC",
                nameHsIdent = HsIdentifier
                  "TC"})},
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
      foreignImportName = HsName
        "@NsVar"
        "wam1",
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
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "y"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "TC")),
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
              (HsName
                "@NsTypeConstr"
                "MC")))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_81d53aede4a7b424",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "MC *hs_bindgen_test_macro_in_fundecl_vs_typedef_81d53aede4a7b424 (float arg1, TC *arg2) { return wam1(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
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
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "TC",
                      nameHsIdent = HsIdentifier
                        "TC"})))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeMacroTypedef
              NamePair {
                nameC = Name "MC",
                nameHsIdent = HsIdentifier "MC"}
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
      foreignImportName = HsName
        "@NsVar"
        "wam2",
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
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "y"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "MC")),
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
              (HsName
                "@NsTypeConstr"
                "TC")))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_035abb3b83b92fea",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "TC *hs_bindgen_test_macro_in_fundecl_vs_typedef_035abb3b83b92fea (float arg1, MC *arg2) { return wam2(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
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
                    nameC = Name "MC",
                    nameHsIdent = HsIdentifier "MC"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "TC",
                  nameHsIdent = HsIdentifier
                    "TC"}))},
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
      foreignImportName = HsName
        "@NsVar"
        "struct_typedef1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
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
            (HsName "@NsVar" "x"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "MC"),
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
          "void hs_bindgen_test_macro_in_fundecl_vs_typedef_34300aead966e212 (struct2 *arg1, MC arg2) { struct_typedef1(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = HsIdentifier "s"})
              (TypePointer
                (TypeTypedef
                  (TypedefSquashed
                    (Name "struct2")
                    (TypeStruct
                      NamePair {
                        nameC = Name "struct2",
                        nameHsIdent = HsIdentifier
                          "Struct2"}
                      (NameOriginGenerated
                        (AnonId
                          "macro_in_fundecl_vs_typedef.h:19:9")))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = HsIdentifier "MC"}
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
      foreignImportName = HsName
        "@NsVar"
        "struct_typedef2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
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
            (HsName "@NsVar" "x"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "MC"),
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
          "void hs_bindgen_test_macro_in_fundecl_vs_typedef_cc4abaeb55d1e034 (struct3_t *arg1, MC arg2) { struct_typedef2(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = HsIdentifier "s"})
              (TypePointer
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "struct3_t",
                      nameHsIdent = HsIdentifier
                        "Struct3_t"}))),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = HsIdentifier "MC"}
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
      foreignImportName = HsName
        "@NsVar"
        "struct_typedef3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
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
            (HsName "@NsVar" "x"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "MC"),
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
          "void hs_bindgen_test_macro_in_fundecl_vs_typedef_d068247e5a3b03e6 (struct4 *arg1, MC arg2) { struct_typedef3(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = HsIdentifier "s"})
              (TypePointer
                (TypeTypedef
                  (TypedefSquashed
                    (Name "struct4")
                    (TypeStruct
                      NamePair {
                        nameC = Name "struct4",
                        nameHsIdent = HsIdentifier
                          "Struct4"}
                      NameOriginInSource)))),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = HsIdentifier "MC"}
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
      foreignImportName = HsName
        "@NsVar"
        "struct_name1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
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
            (HsName "@NsVar" "x"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "MC"),
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
          "void hs_bindgen_test_macro_in_fundecl_vs_typedef_4107c49ec0f22d0c (struct struct1 *arg1, MC arg2) { struct_name1(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = HsIdentifier "s"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "struct1",
                    nameHsIdent = HsIdentifier
                      "Struct1"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = HsIdentifier "MC"}
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
      foreignImportName = HsName
        "@NsVar"
        "struct_name2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
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
            (HsName "@NsVar" "x"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "MC"),
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
          "void hs_bindgen_test_macro_in_fundecl_vs_typedef_124c6e0ae4b86063 (struct struct3 *arg1, MC arg2) { struct_name2(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = HsIdentifier "s"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "struct3",
                    nameHsIdent = HsIdentifier
                      "Struct3"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = HsIdentifier "MC"}
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
      foreignImportName = HsName
        "@NsVar"
        "struct_name3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "s"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
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
            (HsName "@NsVar" "x"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "MC"),
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
          "void hs_bindgen_test_macro_in_fundecl_vs_typedef_4241bc0cd1393d99 (struct struct4 *arg1, MC arg2) { struct_name3(arg1, arg2); }",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "s",
                  nameHsIdent = HsIdentifier "s"})
              (TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "struct4",
                    nameHsIdent = HsIdentifier
                      "Struct4"}
                  NameOriginInSource)),
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "MC",
                  nameHsIdent = HsIdentifier "MC"}
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
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_vs_typedef_7d7a63ab896ed293",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "MC"))
              (HsFun
                (HsTypRef
                  (HsName "@NsTypeConstr" "TC"))
                (HsIO
                  (HsPrimType HsPrimCChar)))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_7d7a63ab896ed293",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_quux1_ptr */ __attribute__ ((const)) char (*hs_bindgen_test_macro_in_fundecl_vs_typedef_7d7a63ab896ed293 (void)) (MC arg1, TC arg2) { return &quux1; } ",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "MC",
                nameHsIdent = HsIdentifier "MC"}
              NameOriginInSource,
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "TC",
                  nameHsIdent = HsIdentifier
                    "TC"})]
          (TypePrim
            (PrimChar
              (PrimSignImplicit Nothing)))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_macro_in_fundecl_vs_typedef_b64c564dd7071f5b",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "MC"))
              (HsFun
                (HsPrimType HsPrimCChar)
                (HsIO
                  (HsTypRef
                    (HsName
                      "@NsTypeConstr"
                      "TC"))))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_b64c564dd7071f5b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_quux2_ptr */ __attribute__ ((const)) TC (*hs_bindgen_test_macro_in_fundecl_vs_typedef_b64c564dd7071f5b (void)) (MC arg1, char arg2) { return &quux2; } ",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "MC",
                nameHsIdent = HsIdentifier "MC"}
              NameOriginInSource,
            TypePrim
              (PrimChar
                (PrimSignImplicit Nothing))]
          (TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = Name "TC",
                nameHsIdent = HsIdentifier
                  "TC"}))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
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
                    (HsName "@NsTypeConstr" "TC")))
                (HsIO
                  (HsPtr
                    (HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "MC")))))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_aa26b3a0f4d0aefe",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_wam1_ptr */ __attribute__ ((const)) MC *(*hs_bindgen_test_macro_in_fundecl_vs_typedef_aa26b3a0f4d0aefe (void)) (float arg1, TC *arg2) { return &wam1; } ",
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
                    nameHsIdent = HsIdentifier
                      "TC"}))]
          (TypePointer
            (TypeMacroTypedef
              NamePair {
                nameC = Name "MC",
                nameHsIdent = HsIdentifier "MC"}
              NameOriginInSource))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
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
                    (HsName "@NsTypeConstr" "MC")))
                (HsIO
                  (HsPtr
                    (HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "TC")))))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_5cb5ead73c0a3d63",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_wam2_ptr */ __attribute__ ((const)) TC *(*hs_bindgen_test_macro_in_fundecl_vs_typedef_5cb5ead73c0a3d63 (void)) (float arg1, MC *arg2) { return &wam2; } ",
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
                  nameHsIdent = HsIdentifier "MC"}
                NameOriginInSource)]
          (TypePointer
            (TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "TC",
                  nameHsIdent = HsIdentifier
                    "TC"})))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
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
                  (HsName
                    "@NsTypeConstr"
                    "Struct2")))
              (HsFun
                (HsTypRef
                  (HsName "@NsTypeConstr" "MC"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_a1aadeb6878a5152",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_struct_typedef1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_a1aadeb6878a5152 (void)) (struct2 *arg1, MC arg2) { return &struct_typedef1; } ",
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
                      nameHsIdent = HsIdentifier
                        "Struct2"}
                    (NameOriginGenerated
                      (AnonId
                        "macro_in_fundecl_vs_typedef.h:19:9"))))),
            TypeMacroTypedef
              NamePair {
                nameC = Name "MC",
                nameHsIdent = HsIdentifier "MC"}
              NameOriginInSource]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
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
                  (HsName
                    "@NsTypeConstr"
                    "Struct3_t")))
              (HsFun
                (HsTypRef
                  (HsName "@NsTypeConstr" "MC"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_e1dac8a006e6b043",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_struct_typedef2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_e1dac8a006e6b043 (void)) (struct3_t *arg1, MC arg2) { return &struct_typedef2; } ",
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
                    nameHsIdent = HsIdentifier
                      "Struct3_t"})),
            TypeMacroTypedef
              NamePair {
                nameC = Name "MC",
                nameHsIdent = HsIdentifier "MC"}
              NameOriginInSource]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
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
                  (HsName
                    "@NsTypeConstr"
                    "Struct4")))
              (HsFun
                (HsTypRef
                  (HsName "@NsTypeConstr" "MC"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_078075d0a80d4368",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_struct_typedef3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_078075d0a80d4368 (void)) (struct4 *arg1, MC arg2) { return &struct_typedef3; } ",
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
                      nameHsIdent = HsIdentifier
                        "Struct4"}
                    NameOriginInSource))),
            TypeMacroTypedef
              NamePair {
                nameC = Name "MC",
                nameHsIdent = HsIdentifier "MC"}
              NameOriginInSource]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
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
                  (HsName
                    "@NsTypeConstr"
                    "Struct1")))
              (HsFun
                (HsTypRef
                  (HsName "@NsTypeConstr" "MC"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_7574edf86480f042",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_struct_name1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_7574edf86480f042 (void)) (struct struct1 *arg1, MC arg2) { return &struct_name1; } ",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name "struct1",
                  nameHsIdent = HsIdentifier
                    "Struct1"}
                NameOriginInSource),
            TypeMacroTypedef
              NamePair {
                nameC = Name "MC",
                nameHsIdent = HsIdentifier "MC"}
              NameOriginInSource]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
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
                  (HsName
                    "@NsTypeConstr"
                    "Struct3")))
              (HsFun
                (HsTypRef
                  (HsName "@NsTypeConstr" "MC"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_e7a8c1f45f8b20c2",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_struct_name2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_e7a8c1f45f8b20c2 (void)) (struct struct3 *arg1, MC arg2) { return &struct_name2; } ",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name "struct3",
                  nameHsIdent = HsIdentifier
                    "Struct3"}
                NameOriginInSource),
            TypeMacroTypedef
              NamePair {
                nameC = Name "MC",
                nameHsIdent = HsIdentifier "MC"}
              NameOriginInSource]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
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
                  (HsName
                    "@NsTypeConstr"
                    "Struct4")))
              (HsFun
                (HsTypRef
                  (HsName "@NsTypeConstr" "MC"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_d52310663e8daa5c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_struct_name3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_d52310663e8daa5c (void)) (struct struct4 *arg1, MC arg2) { return &struct_name3; } ",
          capiWrapperImport =
          "macro_in_fundecl_vs_typedef.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name "struct4",
                  nameHsIdent = HsIdentifier
                    "Struct4"}
                NameOriginInSource),
            TypeMacroTypedef
              NamePair {
                nameC = Name "MC",
                nameHsIdent = HsIdentifier "MC"}
              NameOriginInSource]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
