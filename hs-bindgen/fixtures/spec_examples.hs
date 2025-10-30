[
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Int16_T",
      newtypeConstr = Name
        "@NsConstr"
        "Int16_T",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Int16_T",
        fieldType = HsPrimType
          HsPrimCShort,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "spec_examples.h:10:15",
          declId = NamePair {
            nameC = Name "int16_T",
            nameHsIdent = Identifier
              "Int16_T"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["spec_examples.h"],
              headerInclude =
              "spec_examples.h"},
          declComment = Just
            (Comment
              [
                Paragraph
                  [
                    TextContent
                      "Examples from the initial specification"]])},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Int16_T",
              newtypeField = Name
                "@NsVar"
                "un_Int16_T"},
            typedefType = TypePrim
              (PrimIntegral
                PrimShort
                Signed)},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = ModuleName
              "Example",
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
          commentTitle = Just
            [
              TextContent
                "Examples from the initial specification"],
          commentOrigin = Just "int16_T",
          commentLocation = Just
            "spec_examples.h:10:15",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["spec_examples.h"],
              headerInclude =
              "spec_examples.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int16_T",
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
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Int32_T",
      newtypeConstr = Name
        "@NsConstr"
        "Int32_T",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Int32_T",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "spec_examples.h:11:13",
          declId = NamePair {
            nameC = Name "int32_T",
            nameHsIdent = Identifier
              "Int32_T"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["spec_examples.h"],
              headerInclude =
              "spec_examples.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Int32_T",
              newtypeField = Name
                "@NsVar"
                "un_Int32_T"},
            typedefType = TypePrim
              (PrimIntegral PrimInt Signed)},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = ModuleName
              "Example",
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
          commentOrigin = Just "int32_T",
          commentLocation = Just
            "spec_examples.h:11:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["spec_examples.h"],
              headerInclude =
              "spec_examples.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int32_T",
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
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Int64_T",
      newtypeConstr = Name
        "@NsConstr"
        "Int64_T",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_Int64_T",
        fieldType = HsPrimType
          HsPrimCLLong,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "spec_examples.h:12:19",
          declId = NamePair {
            nameC = Name "int64_T",
            nameHsIdent = Identifier
              "Int64_T"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["spec_examples.h"],
              headerInclude =
              "spec_examples.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "Int64_T",
              newtypeField = Name
                "@NsVar"
                "un_Int64_T"},
            typedefType = TypePrim
              (PrimIntegral
                PrimLongLong
                Signed)},
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = ModuleName
              "Example",
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
          commentOrigin = Just "int64_T",
          commentLocation = Just
            "spec_examples.h:12:19",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["spec_examples.h"],
              headerInclude =
              "spec_examples.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int64_T",
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
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Cint16_T",
      structConstr = Name
        "@NsConstr"
        "Cint16_T",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "cint16_T_re",
          fieldType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Int16_T"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "spec_examples.h:15:11",
                fieldName = NamePair {
                  nameC = Name "re",
                  nameHsIdent = Identifier
                    "cint16_T_re"},
                fieldComment = Nothing},
              structFieldType = TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "int16_T",
                    nameHsIdent = Identifier
                      "Int16_T"}
                  (TypePrim
                    (PrimIntegral
                      PrimShort
                      Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "re",
              commentLocation = Just
                "spec_examples.h:15:11",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["spec_examples.h"],
                  headerInclude =
                  "spec_examples.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "cint16_T_im",
          fieldType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Int16_T"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "spec_examples.h:16:11",
                fieldName = NamePair {
                  nameC = Name "im",
                  nameHsIdent = Identifier
                    "cint16_T_im"},
                fieldComment = Nothing},
              structFieldType = TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "int16_T",
                    nameHsIdent = Identifier
                      "Int16_T"}
                  (TypePrim
                    (PrimIntegral
                      PrimShort
                      Signed))),
              structFieldOffset = 16,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "im",
              commentLocation = Just
                "spec_examples.h:16:11",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["spec_examples.h"],
                  headerInclude =
                  "spec_examples.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "spec_examples.h:14:9",
            declId = NamePair {
              nameC = Name "cint16_T",
              nameHsIdent = Identifier
                "Cint16_T"},
            declOrigin = NameOriginGenerated
              (AnonId "spec_examples.h:14:9"),
            declAliases = [Name "cint16_T"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["spec_examples.h"],
                headerInclude =
                "spec_examples.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Cint16_T"),
              structSizeof = 4,
              structAlignment = 2,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "spec_examples.h:15:11",
                    fieldName = NamePair {
                      nameC = Name "re",
                      nameHsIdent = Identifier
                        "cint16_T_re"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "int16_T",
                        nameHsIdent = Identifier
                          "Int16_T"}
                      (TypePrim
                        (PrimIntegral
                          PrimShort
                          Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "spec_examples.h:16:11",
                    fieldName = NamePair {
                      nameC = Name "im",
                      nameHsIdent = Identifier
                        "cint16_T_im"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "int16_T",
                        nameHsIdent = Identifier
                          "Int16_T"}
                      (TypePrim
                        (PrimIntegral
                          PrimShort
                          Signed))),
                  structFieldOffset = 16,
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
          commentOrigin = Nothing,
          commentLocation = Just
            "spec_examples.h:14:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["spec_examples.h"],
              headerInclude =
              "spec_examples.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Cint16_T",
          structConstr = Name
            "@NsConstr"
            "Cint16_T",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "cint16_T_re",
              fieldType = HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Int16_T"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "spec_examples.h:15:11",
                    fieldName = NamePair {
                      nameC = Name "re",
                      nameHsIdent = Identifier
                        "cint16_T_re"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "int16_T",
                        nameHsIdent = Identifier
                          "Int16_T"}
                      (TypePrim
                        (PrimIntegral
                          PrimShort
                          Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "re",
                  commentLocation = Just
                    "spec_examples.h:15:11",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["spec_examples.h"],
                      headerInclude =
                      "spec_examples.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "cint16_T_im",
              fieldType = HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Int16_T"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "spec_examples.h:16:11",
                    fieldName = NamePair {
                      nameC = Name "im",
                      nameHsIdent = Identifier
                        "cint16_T_im"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "int16_T",
                        nameHsIdent = Identifier
                          "Int16_T"}
                      (TypePrim
                        (PrimIntegral
                          PrimShort
                          Signed))),
                  structFieldOffset = 16,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "im",
                  commentLocation = Just
                    "spec_examples.h:16:11",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["spec_examples.h"],
                      headerInclude =
                      "spec_examples.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "spec_examples.h:14:9",
                declId = NamePair {
                  nameC = Name "cint16_T",
                  nameHsIdent = Identifier
                    "Cint16_T"},
                declOrigin = NameOriginGenerated
                  (AnonId "spec_examples.h:14:9"),
                declAliases = [Name "cint16_T"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["spec_examples.h"],
                    headerInclude =
                    "spec_examples.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Cint16_T"),
                  structSizeof = 4,
                  structAlignment = 2,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "spec_examples.h:15:11",
                        fieldName = NamePair {
                          nameC = Name "re",
                          nameHsIdent = Identifier
                            "cint16_T_re"},
                        fieldComment = Nothing},
                      structFieldType = TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "int16_T",
                            nameHsIdent = Identifier
                              "Int16_T"}
                          (TypePrim
                            (PrimIntegral
                              PrimShort
                              Signed))),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "spec_examples.h:16:11",
                        fieldName = NamePair {
                          nameC = Name "im",
                          nameHsIdent = Identifier
                            "cint16_T_im"},
                        fieldComment = Nothing},
                      structFieldType = TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "int16_T",
                            nameHsIdent = Identifier
                              "Int16_T"}
                          (TypePrim
                            (PrimIntegral
                              PrimShort
                              Signed))),
                      structFieldOffset = 16,
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
              commentOrigin = Nothing,
              commentLocation = Just
                "spec_examples.h:14:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["spec_examples.h"],
                  headerInclude =
                  "spec_examples.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 4,
          storableAlignment = 2,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Cint16_T",
                  structConstr = Name
                    "@NsConstr"
                    "Cint16_T",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "cint16_T_re",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Int16_T"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:15:11",
                            fieldName = NamePair {
                              nameC = Name "re",
                              nameHsIdent = Identifier
                                "cint16_T_re"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefRegular
                              NamePair {
                                nameC = Name "int16_T",
                                nameHsIdent = Identifier
                                  "Int16_T"}
                              (TypePrim
                                (PrimIntegral
                                  PrimShort
                                  Signed))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "re",
                          commentLocation = Just
                            "spec_examples.h:15:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["spec_examples.h"],
                              headerInclude =
                              "spec_examples.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "cint16_T_im",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Int16_T"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:16:11",
                            fieldName = NamePair {
                              nameC = Name "im",
                              nameHsIdent = Identifier
                                "cint16_T_im"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefRegular
                              NamePair {
                                nameC = Name "int16_T",
                                nameHsIdent = Identifier
                                  "Int16_T"}
                              (TypePrim
                                (PrimIntegral
                                  PrimShort
                                  Signed))),
                          structFieldOffset = 16,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "im",
                          commentLocation = Just
                            "spec_examples.h:16:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["spec_examples.h"],
                              headerInclude =
                              "spec_examples.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "spec_examples.h:14:9",
                        declId = NamePair {
                          nameC = Name "cint16_T",
                          nameHsIdent = Identifier
                            "Cint16_T"},
                        declOrigin = NameOriginGenerated
                          (AnonId "spec_examples.h:14:9"),
                        declAliases = [Name "cint16_T"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["spec_examples.h"],
                            headerInclude =
                            "spec_examples.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Cint16_T"),
                          structSizeof = 4,
                          structAlignment = 2,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "spec_examples.h:15:11",
                                fieldName = NamePair {
                                  nameC = Name "re",
                                  nameHsIdent = Identifier
                                    "cint16_T_re"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefRegular
                                  NamePair {
                                    nameC = Name "int16_T",
                                    nameHsIdent = Identifier
                                      "Int16_T"}
                                  (TypePrim
                                    (PrimIntegral
                                      PrimShort
                                      Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "spec_examples.h:16:11",
                                fieldName = NamePair {
                                  nameC = Name "im",
                                  nameHsIdent = Identifier
                                    "cint16_T_im"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefRegular
                                  NamePair {
                                    nameC = Name "int16_T",
                                    nameHsIdent = Identifier
                                      "Int16_T"}
                                  (TypePrim
                                    (PrimIntegral
                                      PrimShort
                                      Signed))),
                              structFieldOffset = 16,
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
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "spec_examples.h:14:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["spec_examples.h"],
                          headerInclude =
                          "spec_examples.h"},
                      commentChildren = []}})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 2]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Cint16_T",
                  structConstr = Name
                    "@NsConstr"
                    "Cint16_T",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "cint16_T_re",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Int16_T"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:15:11",
                            fieldName = NamePair {
                              nameC = Name "re",
                              nameHsIdent = Identifier
                                "cint16_T_re"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefRegular
                              NamePair {
                                nameC = Name "int16_T",
                                nameHsIdent = Identifier
                                  "Int16_T"}
                              (TypePrim
                                (PrimIntegral
                                  PrimShort
                                  Signed))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "re",
                          commentLocation = Just
                            "spec_examples.h:15:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["spec_examples.h"],
                              headerInclude =
                              "spec_examples.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "cint16_T_im",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "Int16_T"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:16:11",
                            fieldName = NamePair {
                              nameC = Name "im",
                              nameHsIdent = Identifier
                                "cint16_T_im"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefRegular
                              NamePair {
                                nameC = Name "int16_T",
                                nameHsIdent = Identifier
                                  "Int16_T"}
                              (TypePrim
                                (PrimIntegral
                                  PrimShort
                                  Signed))),
                          structFieldOffset = 16,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "im",
                          commentLocation = Just
                            "spec_examples.h:16:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["spec_examples.h"],
                              headerInclude =
                              "spec_examples.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "spec_examples.h:14:9",
                        declId = NamePair {
                          nameC = Name "cint16_T",
                          nameHsIdent = Identifier
                            "Cint16_T"},
                        declOrigin = NameOriginGenerated
                          (AnonId "spec_examples.h:14:9"),
                        declAliases = [Name "cint16_T"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["spec_examples.h"],
                            headerInclude =
                            "spec_examples.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Cint16_T"),
                          structSizeof = 4,
                          structAlignment = 2,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "spec_examples.h:15:11",
                                fieldName = NamePair {
                                  nameC = Name "re",
                                  nameHsIdent = Identifier
                                    "cint16_T_re"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefRegular
                                  NamePair {
                                    nameC = Name "int16_T",
                                    nameHsIdent = Identifier
                                      "Int16_T"}
                                  (TypePrim
                                    (PrimIntegral
                                      PrimShort
                                      Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "spec_examples.h:16:11",
                                fieldName = NamePair {
                                  nameC = Name "im",
                                  nameHsIdent = Identifier
                                    "cint16_T_im"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefRegular
                                  NamePair {
                                    nameC = Name "int16_T",
                                    nameHsIdent = Identifier
                                      "Int16_T"}
                                  (TypePrim
                                    (PrimIntegral
                                      PrimShort
                                      Signed))),
                              structFieldOffset = 16,
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
                      commentOrigin = Nothing,
                      commentLocation = Just
                        "spec_examples.h:14:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["spec_examples.h"],
                          headerInclude =
                          "spec_examples.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      2
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
        "Cint16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Cint16_T",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "B",
      structConstr = Name
        "@NsConstr"
        "B",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "spec_examples.h:19:8",
            declId = NamePair {
              nameC = Name "B",
              nameHsIdent = Identifier "B"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["spec_examples.h"],
                headerInclude =
                "spec_examples.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "B"),
              structSizeof = 0,
              structAlignment = 1,
              structFields = [],
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
          commentOrigin = Just "B",
          commentLocation = Just
            "spec_examples.h:19:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["spec_examples.h"],
              headerInclude =
              "spec_examples.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "B",
          structConstr = Name
            "@NsConstr"
            "B",
          structFields = [],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "spec_examples.h:19:8",
                declId = NamePair {
                  nameC = Name "B",
                  nameHsIdent = Identifier "B"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["spec_examples.h"],
                    headerInclude =
                    "spec_examples.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "B"),
                  structSizeof = 0,
                  structAlignment = 1,
                  structFields = [],
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
              commentOrigin = Just "B",
              commentLocation = Just
                "spec_examples.h:19:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["spec_examples.h"],
                  headerInclude =
                  "spec_examples.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 0,
          storableAlignment = 1,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "B",
                  structConstr = Name
                    "@NsConstr"
                    "B",
                  structFields = [],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "spec_examples.h:19:8",
                        declId = NamePair {
                          nameC = Name "B",
                          nameHsIdent = Identifier "B"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["spec_examples.h"],
                            headerInclude =
                            "spec_examples.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "B"),
                          structSizeof = 0,
                          structAlignment = 1,
                          structFields = [],
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
                      commentOrigin = Just "B",
                      commentLocation = Just
                        "spec_examples.h:19:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["spec_examples.h"],
                          headerInclude =
                          "spec_examples.h"},
                      commentChildren = []}})
              []),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "B",
                  structConstr = Name
                    "@NsConstr"
                    "B",
                  structFields = [],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "spec_examples.h:19:8",
                        declId = NamePair {
                          nameC = Name "B",
                          nameHsIdent = Identifier "B"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["spec_examples.h"],
                            headerInclude =
                            "spec_examples.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "B"),
                          structSizeof = 0,
                          structAlignment = 1,
                          structFields = [],
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
                      commentOrigin = Just "B",
                      commentLocation = Just
                        "spec_examples.h:19:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["spec_examples.h"],
                          headerInclude =
                          "spec_examples.h"},
                      commentChildren = []}}
                (Add 0)
                (Seq [])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "B",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "B",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "A",
      structConstr = Name
        "@NsConstr"
        "A",
      structFields = [
        Field {
          fieldName = Name "@NsVar" "a_x",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "spec_examples.h:24:10",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "a_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Just
                "spec_examples.h:24:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["spec_examples.h"],
                  headerInclude =
                  "spec_examples.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "a_label",
          fieldType = HsPtr
            (HsPrimType HsPrimCChar),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "spec_examples.h:25:9",
                fieldName = NamePair {
                  nameC = Name "label",
                  nameHsIdent = Identifier
                    "a_label"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "label",
              commentLocation = Just
                "spec_examples.h:25:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["spec_examples.h"],
                  headerInclude =
                  "spec_examples.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "a_samples",
          fieldType = HsConstArray
            128
            (HsPrimType HsPrimCChar),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "spec_examples.h:26:8",
                fieldName = NamePair {
                  nameC = Name "samples",
                  nameHsIdent = Identifier
                    "a_samples"},
                fieldComment = Nothing},
              structFieldType = TypeConstArray
                128
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))),
              structFieldOffset = 128,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "samples",
              commentLocation = Just
                "spec_examples.h:26:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["spec_examples.h"],
                  headerInclude =
                  "spec_examples.h"},
              commentChildren = []}},
        Field {
          fieldName = Name "@NsVar" "a_b",
          fieldType = HsTypRef
            (Name "@NsTypeConstr" "B"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "spec_examples.h:27:12",
                fieldName = NamePair {
                  nameC = Name "b",
                  nameHsIdent = Identifier "a_b"},
                fieldComment = Nothing},
              structFieldType = TypeStruct
                NamePair {
                  nameC = Name "B",
                  nameHsIdent = Identifier "B"}
                NameOriginInSource,
              structFieldOffset = 1152,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "b",
              commentLocation = Just
                "spec_examples.h:27:12",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["spec_examples.h"],
                  headerInclude =
                  "spec_examples.h"},
              commentChildren = []}},
        Field {
          fieldName = Name "@NsVar" "a_c",
          fieldType = HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "C")),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "spec_examples.h:28:13",
                fieldName = NamePair {
                  nameC = Name "c",
                  nameHsIdent = Identifier "a_c"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "C",
                    nameHsIdent = Identifier "C"}
                  NameOriginInSource),
              structFieldOffset = 1152,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "c",
              commentLocation = Just
                "spec_examples.h:28:13",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["spec_examples.h"],
                  headerInclude =
                  "spec_examples.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "spec_examples.h:23:8",
            declId = NamePair {
              nameC = Name "A",
              nameHsIdent = Identifier "A"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["spec_examples.h"],
                headerInclude =
                "spec_examples.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "A"),
              structSizeof = 152,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "spec_examples.h:24:10",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier "a_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "spec_examples.h:25:9",
                    fieldName = NamePair {
                      nameC = Name "label",
                      nameHsIdent = Identifier
                        "a_label"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed)))),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "spec_examples.h:26:8",
                    fieldName = NamePair {
                      nameC = Name "samples",
                      nameHsIdent = Identifier
                        "a_samples"},
                    fieldComment = Nothing},
                  structFieldType = TypeConstArray
                    128
                    (TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed)))),
                  structFieldOffset = 128,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "spec_examples.h:27:12",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier "a_b"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "B",
                      nameHsIdent = Identifier "B"}
                    NameOriginInSource,
                  structFieldOffset = 1152,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "spec_examples.h:28:13",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = Identifier "a_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "C",
                        nameHsIdent = Identifier "C"}
                      NameOriginInSource),
                  structFieldOffset = 1152,
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
          commentOrigin = Just "A",
          commentLocation = Just
            "spec_examples.h:23:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["spec_examples.h"],
              headerInclude =
              "spec_examples.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "A",
          structConstr = Name
            "@NsConstr"
            "A",
          structFields = [
            Field {
              fieldName = Name "@NsVar" "a_x",
              fieldType = HsPrimType
                HsPrimCDouble,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "spec_examples.h:24:10",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier "a_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "x",
                  commentLocation = Just
                    "spec_examples.h:24:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["spec_examples.h"],
                      headerInclude =
                      "spec_examples.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "a_label",
              fieldType = HsPtr
                (HsPrimType HsPrimCChar),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "spec_examples.h:25:9",
                    fieldName = NamePair {
                      nameC = Name "label",
                      nameHsIdent = Identifier
                        "a_label"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed)))),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "label",
                  commentLocation = Just
                    "spec_examples.h:25:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["spec_examples.h"],
                      headerInclude =
                      "spec_examples.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "a_samples",
              fieldType = HsConstArray
                128
                (HsPrimType HsPrimCChar),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "spec_examples.h:26:8",
                    fieldName = NamePair {
                      nameC = Name "samples",
                      nameHsIdent = Identifier
                        "a_samples"},
                    fieldComment = Nothing},
                  structFieldType = TypeConstArray
                    128
                    (TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed)))),
                  structFieldOffset = 128,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "samples",
                  commentLocation = Just
                    "spec_examples.h:26:8",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["spec_examples.h"],
                      headerInclude =
                      "spec_examples.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name "@NsVar" "a_b",
              fieldType = HsTypRef
                (Name "@NsTypeConstr" "B"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "spec_examples.h:27:12",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = Identifier "a_b"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "B",
                      nameHsIdent = Identifier "B"}
                    NameOriginInSource,
                  structFieldOffset = 1152,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "b",
                  commentLocation = Just
                    "spec_examples.h:27:12",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["spec_examples.h"],
                      headerInclude =
                      "spec_examples.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name "@NsVar" "a_c",
              fieldType = HsPtr
                (HsTypRef
                  (Name "@NsTypeConstr" "C")),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "spec_examples.h:28:13",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = Identifier "a_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "C",
                        nameHsIdent = Identifier "C"}
                      NameOriginInSource),
                  structFieldOffset = 1152,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "c",
                  commentLocation = Just
                    "spec_examples.h:28:13",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["spec_examples.h"],
                      headerInclude =
                      "spec_examples.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "spec_examples.h:23:8",
                declId = NamePair {
                  nameC = Name "A",
                  nameHsIdent = Identifier "A"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["spec_examples.h"],
                    headerInclude =
                    "spec_examples.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "A"),
                  structSizeof = 152,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "spec_examples.h:24:10",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier "a_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimFloating PrimDouble),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "spec_examples.h:25:9",
                        fieldName = NamePair {
                          nameC = Name "label",
                          nameHsIdent = Identifier
                            "a_label"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed)))),
                      structFieldOffset = 64,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "spec_examples.h:26:8",
                        fieldName = NamePair {
                          nameC = Name "samples",
                          nameHsIdent = Identifier
                            "a_samples"},
                        fieldComment = Nothing},
                      structFieldType = TypeConstArray
                        128
                        (TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed)))),
                      structFieldOffset = 128,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "spec_examples.h:27:12",
                        fieldName = NamePair {
                          nameC = Name "b",
                          nameHsIdent = Identifier "a_b"},
                        fieldComment = Nothing},
                      structFieldType = TypeStruct
                        NamePair {
                          nameC = Name "B",
                          nameHsIdent = Identifier "B"}
                        NameOriginInSource,
                      structFieldOffset = 1152,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "spec_examples.h:28:13",
                        fieldName = NamePair {
                          nameC = Name "c",
                          nameHsIdent = Identifier "a_c"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeStruct
                          NamePair {
                            nameC = Name "C",
                            nameHsIdent = Identifier "C"}
                          NameOriginInSource),
                      structFieldOffset = 1152,
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
              commentOrigin = Just "A",
              commentLocation = Just
                "spec_examples.h:23:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["spec_examples.h"],
                  headerInclude =
                  "spec_examples.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 152,
          storableAlignment = 8,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "A",
                  structConstr = Name
                    "@NsConstr"
                    "A",
                  structFields = [
                    Field {
                      fieldName = Name "@NsVar" "a_x",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:24:10",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier "a_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "spec_examples.h:24:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["spec_examples.h"],
                              headerInclude =
                              "spec_examples.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_label",
                      fieldType = HsPtr
                        (HsPrimType HsPrimCChar),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:25:9",
                            fieldName = NamePair {
                              nameC = Name "label",
                              nameHsIdent = Identifier
                                "a_label"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed)))),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "label",
                          commentLocation = Just
                            "spec_examples.h:25:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["spec_examples.h"],
                              headerInclude =
                              "spec_examples.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_samples",
                      fieldType = HsConstArray
                        128
                        (HsPrimType HsPrimCChar),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:26:8",
                            fieldName = NamePair {
                              nameC = Name "samples",
                              nameHsIdent = Identifier
                                "a_samples"},
                            fieldComment = Nothing},
                          structFieldType = TypeConstArray
                            128
                            (TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed)))),
                          structFieldOffset = 128,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "samples",
                          commentLocation = Just
                            "spec_examples.h:26:8",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["spec_examples.h"],
                              headerInclude =
                              "spec_examples.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name "@NsVar" "a_b",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "B"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:27:12",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier "a_b"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "B",
                              nameHsIdent = Identifier "B"}
                            NameOriginInSource,
                          structFieldOffset = 1152,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "b",
                          commentLocation = Just
                            "spec_examples.h:27:12",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["spec_examples.h"],
                              headerInclude =
                              "spec_examples.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name "@NsVar" "a_c",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name "@NsTypeConstr" "C")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:28:13",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = Identifier "a_c"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "C",
                                nameHsIdent = Identifier "C"}
                              NameOriginInSource),
                          structFieldOffset = 1152,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "spec_examples.h:28:13",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["spec_examples.h"],
                              headerInclude =
                              "spec_examples.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "spec_examples.h:23:8",
                        declId = NamePair {
                          nameC = Name "A",
                          nameHsIdent = Identifier "A"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["spec_examples.h"],
                            headerInclude =
                            "spec_examples.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "A"),
                          structSizeof = 152,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "spec_examples.h:24:10",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier "a_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "spec_examples.h:25:9",
                                fieldName = NamePair {
                                  nameC = Name "label",
                                  nameHsIdent = Identifier
                                    "a_label"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypePrim
                                  (PrimChar
                                    (PrimSignImplicit
                                      (Just Signed)))),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "spec_examples.h:26:8",
                                fieldName = NamePair {
                                  nameC = Name "samples",
                                  nameHsIdent = Identifier
                                    "a_samples"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                128
                                (TypePrim
                                  (PrimChar
                                    (PrimSignImplicit
                                      (Just Signed)))),
                              structFieldOffset = 128,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "spec_examples.h:27:12",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier "a_b"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "B",
                                  nameHsIdent = Identifier "B"}
                                NameOriginInSource,
                              structFieldOffset = 1152,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "spec_examples.h:28:13",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = Identifier "a_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "C",
                                    nameHsIdent = Identifier "C"}
                                  NameOriginInSource),
                              structFieldOffset = 1152,
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
                      commentOrigin = Just "A",
                      commentLocation = Just
                        "spec_examples.h:23:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["spec_examples.h"],
                          headerInclude =
                          "spec_examples.h"},
                      commentChildren = []}})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 8,
                PeekByteOff (Idx 0) 16,
                PeekByteOff (Idx 0) 144,
                PeekByteOff (Idx 0) 144]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "A",
                  structConstr = Name
                    "@NsConstr"
                    "A",
                  structFields = [
                    Field {
                      fieldName = Name "@NsVar" "a_x",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:24:10",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier "a_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "spec_examples.h:24:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["spec_examples.h"],
                              headerInclude =
                              "spec_examples.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_label",
                      fieldType = HsPtr
                        (HsPrimType HsPrimCChar),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:25:9",
                            fieldName = NamePair {
                              nameC = Name "label",
                              nameHsIdent = Identifier
                                "a_label"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed)))),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "label",
                          commentLocation = Just
                            "spec_examples.h:25:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["spec_examples.h"],
                              headerInclude =
                              "spec_examples.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "a_samples",
                      fieldType = HsConstArray
                        128
                        (HsPrimType HsPrimCChar),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:26:8",
                            fieldName = NamePair {
                              nameC = Name "samples",
                              nameHsIdent = Identifier
                                "a_samples"},
                            fieldComment = Nothing},
                          structFieldType = TypeConstArray
                            128
                            (TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed)))),
                          structFieldOffset = 128,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "samples",
                          commentLocation = Just
                            "spec_examples.h:26:8",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["spec_examples.h"],
                              headerInclude =
                              "spec_examples.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name "@NsVar" "a_b",
                      fieldType = HsTypRef
                        (Name "@NsTypeConstr" "B"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:27:12",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = Identifier "a_b"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "B",
                              nameHsIdent = Identifier "B"}
                            NameOriginInSource,
                          structFieldOffset = 1152,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "b",
                          commentLocation = Just
                            "spec_examples.h:27:12",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["spec_examples.h"],
                              headerInclude =
                              "spec_examples.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name "@NsVar" "a_c",
                      fieldType = HsPtr
                        (HsTypRef
                          (Name "@NsTypeConstr" "C")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:28:13",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = Identifier "a_c"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "C",
                                nameHsIdent = Identifier "C"}
                              NameOriginInSource),
                          structFieldOffset = 1152,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "spec_examples.h:28:13",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["spec_examples.h"],
                              headerInclude =
                              "spec_examples.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "spec_examples.h:23:8",
                        declId = NamePair {
                          nameC = Name "A",
                          nameHsIdent = Identifier "A"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["spec_examples.h"],
                            headerInclude =
                            "spec_examples.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "A"),
                          structSizeof = 152,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "spec_examples.h:24:10",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier "a_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "spec_examples.h:25:9",
                                fieldName = NamePair {
                                  nameC = Name "label",
                                  nameHsIdent = Identifier
                                    "a_label"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypePrim
                                  (PrimChar
                                    (PrimSignImplicit
                                      (Just Signed)))),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "spec_examples.h:26:8",
                                fieldName = NamePair {
                                  nameC = Name "samples",
                                  nameHsIdent = Identifier
                                    "a_samples"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                128
                                (TypePrim
                                  (PrimChar
                                    (PrimSignImplicit
                                      (Just Signed)))),
                              structFieldOffset = 128,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "spec_examples.h:27:12",
                                fieldName = NamePair {
                                  nameC = Name "b",
                                  nameHsIdent = Identifier "a_b"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "B",
                                  nameHsIdent = Identifier "B"}
                                NameOriginInSource,
                              structFieldOffset = 1152,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "spec_examples.h:28:13",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = Identifier "a_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "C",
                                    nameHsIdent = Identifier "C"}
                                  NameOriginInSource),
                              structFieldOffset = 1152,
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
                      commentOrigin = Just "A",
                      commentLocation = Just
                        "spec_examples.h:23:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["spec_examples.h"],
                          headerInclude =
                          "spec_examples.h"},
                      commentChildren = []}}
                (Add 5)
                (Seq
                  [
                    PokeByteOff (Idx 6) 0 (Idx 0),
                    PokeByteOff (Idx 6) 8 (Idx 1),
                    PokeByteOff (Idx 6) 16 (Idx 2),
                    PokeByteOff (Idx 6) 144 (Idx 3),
                    PokeByteOff
                      (Idx 6)
                      144
                      (Idx 4)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "A",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "A",
      deriveInstanceComment =
      Nothing},
  DeclEmpty
    EmptyData {
      emptyDataName = Name
        "@NsTypeConstr"
        "C",
      emptyDataOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "spec_examples.h:28:10",
          declId = NamePair {
            nameC = Name "C",
            nameHsIdent = Identifier "C"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["spec_examples.h"],
              headerInclude =
              "spec_examples.h"},
          declComment = Nothing},
        declKind = Opaque
          (NameKindTagged TagKindStruct),
        declSpec = DeclSpec
          CTypeSpec {
            cTypeSpecModule = ModuleName
              "Example",
            cTypeSpecIdentifier = Nothing,
            cTypeSpecInstances =
            Map.fromList []}},
      emptyDataComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "C",
          commentLocation = Just
            "spec_examples.h:28:10",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["spec_examples.h"],
              headerInclude =
              "spec_examples.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "resample",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name
              "@NsVar"
              "res_m_num_valid_samples"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Int32_T")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "res_m_num_valid_samples",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "res_m_iq_int"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Cint16_T")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "res_m_iq_int",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name
              "@NsVar"
              "res_m_old_rate"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Int64_T"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "res_m_old_rate",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name
              "@NsVar"
              "res_m_new_rate"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Int64_T"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "res_m_new_rate",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name
              "@NsVar"
              "res_m_iq_resampled_int"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Cint16_T")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "res_m_iq_resampled_int",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_spec_examples_7d4128962cfce15d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_spec_examples_7d4128962cfce15d (int32_T *arg1, cint16_T *arg2, int64_T arg3, int64_T arg4, cint16_T *arg5) { resample(arg1, arg2, arg3, arg4, arg5); }",
          capiWrapperImport =
          "spec_examples.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name
                    "res_m_num_valid_samples",
                  nameHsIdent = Identifier
                    "res_m_num_valid_samples"})
              (TypePointer
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "int32_T",
                      nameHsIdent = Identifier
                        "Int32_T"}
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "res_m_iq_int",
                  nameHsIdent = Identifier
                    "res_m_iq_int"})
              (TypeConstArray
                30720000
                (TypeTypedef
                  (TypedefSquashed
                    (Name "cint16_T")
                    (TypeStruct
                      NamePair {
                        nameC = Name "cint16_T",
                        nameHsIdent = Identifier
                          "Cint16_T"}
                      (NameOriginGenerated
                        (AnonId
                          "spec_examples.h:14:9")))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "res_m_old_rate",
                  nameHsIdent = Identifier
                    "res_m_old_rate"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "int64_T",
                    nameHsIdent = Identifier
                      "Int64_T"}
                  (TypePrim
                    (PrimIntegral
                      PrimLongLong
                      Signed)))),
            _×_
              (Just
                NamePair {
                  nameC = Name "res_m_new_rate",
                  nameHsIdent = Identifier
                    "res_m_new_rate"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "int64_T",
                    nameHsIdent = Identifier
                      "Int64_T"}
                  (TypePrim
                    (PrimIntegral
                      PrimLongLong
                      Signed)))),
            _×_
              (Just
                NamePair {
                  nameC = Name
                    "res_m_iq_resampled_int",
                  nameHsIdent = Identifier
                    "res_m_iq_resampled_int"})
              (TypeConstArray
                30720000
                (TypeTypedef
                  (TypedefSquashed
                    (Name "cint16_T")
                    (TypeStruct
                      NamePair {
                        nameC = Name "cint16_T",
                        nameHsIdent = Identifier
                          "Cint16_T"}
                      (NameOriginGenerated
                        (AnonId
                          "spec_examples.h:14:9"))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "resample",
          commentLocation = Just
            "spec_examples.h:31:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["spec_examples.h"],
              headerInclude =
              "spec_examples.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "resample",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name
              "@NsVar"
              "res_m_num_valid_samples"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Int32_T")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "res_m_num_valid_samples",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "res_m_iq_int"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Cint16_T")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "res_m_iq_int",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name
              "@NsVar"
              "res_m_old_rate"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Int64_T"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "res_m_old_rate",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name
              "@NsVar"
              "res_m_new_rate"),
          functionParameterType = HsTypRef
            (Name
              "@NsTypeConstr"
              "Int64_T"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "res_m_new_rate",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name
              "@NsVar"
              "res_m_iq_resampled_int"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name
                "@NsTypeConstr"
                "Cint16_T")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "res_m_iq_resampled_int",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_spec_examples_f31d4400b3244637",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_spec_examples_f31d4400b3244637 (int32_T *arg1, cint16_T *arg2, int64_T arg3, int64_T arg4, cint16_T *arg5) { resample(arg1, arg2, arg3, arg4, arg5); }",
          capiWrapperImport =
          "spec_examples.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name
                    "res_m_num_valid_samples",
                  nameHsIdent = Identifier
                    "res_m_num_valid_samples"})
              (TypePointer
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "int32_T",
                      nameHsIdent = Identifier
                        "Int32_T"}
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "res_m_iq_int",
                  nameHsIdent = Identifier
                    "res_m_iq_int"})
              (TypeConstArray
                30720000
                (TypeTypedef
                  (TypedefSquashed
                    (Name "cint16_T")
                    (TypeStruct
                      NamePair {
                        nameC = Name "cint16_T",
                        nameHsIdent = Identifier
                          "Cint16_T"}
                      (NameOriginGenerated
                        (AnonId
                          "spec_examples.h:14:9")))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "res_m_old_rate",
                  nameHsIdent = Identifier
                    "res_m_old_rate"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "int64_T",
                    nameHsIdent = Identifier
                      "Int64_T"}
                  (TypePrim
                    (PrimIntegral
                      PrimLongLong
                      Signed)))),
            _×_
              (Just
                NamePair {
                  nameC = Name "res_m_new_rate",
                  nameHsIdent = Identifier
                    "res_m_new_rate"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "int64_T",
                    nameHsIdent = Identifier
                      "Int64_T"}
                  (TypePrim
                    (PrimIntegral
                      PrimLongLong
                      Signed)))),
            _×_
              (Just
                NamePair {
                  nameC = Name
                    "res_m_iq_resampled_int",
                  nameHsIdent = Identifier
                    "res_m_iq_resampled_int"})
              (TypeConstArray
                30720000
                (TypeTypedef
                  (TypedefSquashed
                    (Name "cint16_T")
                    (TypeStruct
                      NamePair {
                        nameC = Name "cint16_T",
                        nameHsIdent = Identifier
                          "Cint16_T"}
                      (NameOriginGenerated
                        (AnonId
                          "spec_examples.h:14:9"))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "resample",
          commentLocation = Just
            "spec_examples.h:31:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["spec_examples.h"],
              headerInclude =
              "spec_examples.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_spec_examples_46b04422dcd0bbd5",
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
                    "Int32_T")))
              (HsFun
                (HsConstArray
                  30720000
                  (HsTypRef
                    (Name
                      "@NsTypeConstr"
                      "Cint16_T")))
                (HsFun
                  (HsTypRef
                    (Name
                      "@NsTypeConstr"
                      "Int64_T"))
                  (HsFun
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "Int64_T"))
                    (HsFun
                      (HsConstArray
                        30720000
                        (HsTypRef
                          (Name
                            "@NsTypeConstr"
                            "Cint16_T")))
                      (HsIO
                        (HsPrimType HsPrimUnit))))))))),
      foreignImportOrigName =
      "hs_bindgen_test_spec_examples_46b04422dcd0bbd5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_resample_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_spec_examples_46b04422dcd0bbd5 (void)) (int32_T *arg1, cint16_T arg2[30720000], int64_T arg3, int64_T arg4, cint16_T arg5[30720000]) { return &resample; } ",
          capiWrapperImport =
          "spec_examples.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "int32_T",
                    nameHsIdent = Identifier
                      "Int32_T"}
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))),
            TypeConstArray
              30720000
              (TypeTypedef
                (TypedefSquashed
                  (Name "cint16_T")
                  (TypeStruct
                    NamePair {
                      nameC = Name "cint16_T",
                      nameHsIdent = Identifier
                        "Cint16_T"}
                    (NameOriginGenerated
                      (AnonId
                        "spec_examples.h:14:9"))))),
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "int64_T",
                  nameHsIdent = Identifier
                    "Int64_T"}
                (TypePrim
                  (PrimIntegral
                    PrimLongLong
                    Signed))),
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "int64_T",
                  nameHsIdent = Identifier
                    "Int64_T"}
                (TypePrim
                  (PrimIntegral
                    PrimLongLong
                    Signed))),
            TypeConstArray
              30720000
              (TypeTypedef
                (TypedefSquashed
                  (Name "cint16_T")
                  (TypeStruct
                    NamePair {
                      nameC = Name "cint16_T",
                      nameHsIdent = Identifier
                        "Cint16_T"}
                    (NameOriginGenerated
                      (AnonId
                        "spec_examples.h:14:9")))))]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
