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
                ["edge-cases/spec_examples.h"],
              headerInclude =
              "edge-cases/spec_examples.h"},
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
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
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
                ["edge-cases/spec_examples.h"],
              headerInclude =
              "edge-cases/spec_examples.h"},
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
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Int16_T"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_Int16_T",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCShort,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Int16_T"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_Int16_T",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCShort,
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
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
                ["edge-cases/spec_examples.h"],
              headerInclude =
              "edge-cases/spec_examples.h"},
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
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
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
                ["edge-cases/spec_examples.h"],
              headerInclude =
              "edge-cases/spec_examples.h"},
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
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Int32_T"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_Int32_T",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Int32_T"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_Int32_T",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCInt,
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
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
                ["edge-cases/spec_examples.h"],
              headerInclude =
              "edge-cases/spec_examples.h"},
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
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
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
                ["edge-cases/spec_examples.h"],
              headerInclude =
              "edge-cases/spec_examples.h"},
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
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Int64_T"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_Int64_T",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCLLong,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Int64_T"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_Int64_T",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCLLong,
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
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
                    ["edge-cases/spec_examples.h"],
                  headerInclude =
                  "edge-cases/spec_examples.h"},
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
                    ["edge-cases/spec_examples.h"],
                  headerInclude =
                  "edge-cases/spec_examples.h"},
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
                  ["edge-cases/spec_examples.h"],
                headerInclude =
                "edge-cases/spec_examples.h"},
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
          declSpec = DeclSpec {
            declSpecC = Nothing,
            declSpecHs = Nothing}},
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
                ["edge-cases/spec_examples.h"],
              headerInclude =
              "edge-cases/spec_examples.h"},
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
                        ["edge-cases/spec_examples.h"],
                      headerInclude =
                      "edge-cases/spec_examples.h"},
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
                        ["edge-cases/spec_examples.h"],
                      headerInclude =
                      "edge-cases/spec_examples.h"},
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
                      ["edge-cases/spec_examples.h"],
                    headerInclude =
                    "edge-cases/spec_examples.h"},
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
              declSpec = DeclSpec {
                declSpecC = Nothing,
                declSpecHs = Nothing}},
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
                    ["edge-cases/spec_examples.h"],
                  headerInclude =
                  "edge-cases/spec_examples.h"},
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
                                ["edge-cases/spec_examples.h"],
                              headerInclude =
                              "edge-cases/spec_examples.h"},
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
                                ["edge-cases/spec_examples.h"],
                              headerInclude =
                              "edge-cases/spec_examples.h"},
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
                              ["edge-cases/spec_examples.h"],
                            headerInclude =
                            "edge-cases/spec_examples.h"},
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
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
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
                            ["edge-cases/spec_examples.h"],
                          headerInclude =
                          "edge-cases/spec_examples.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "cint16_T_re")
                  (Idx 0),
                PeekCField
                  (HsStrLit "cint16_T_im")
                  (Idx 0)]),
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
                                ["edge-cases/spec_examples.h"],
                              headerInclude =
                              "edge-cases/spec_examples.h"},
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
                                ["edge-cases/spec_examples.h"],
                              headerInclude =
                              "edge-cases/spec_examples.h"},
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
                              ["edge-cases/spec_examples.h"],
                            headerInclude =
                            "edge-cases/spec_examples.h"},
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
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
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
                            ["edge-cases/spec_examples.h"],
                          headerInclude =
                          "edge-cases/spec_examples.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "cint16_T_re")
                      (Idx 3)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "cint16_T_im")
                      (Idx 3)
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
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Cint16_T"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "cint16_T_re",
          hasCFieldInstanceCFieldType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Int16_T"),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Cint16_T"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "cint16_T_re",
          hasFieldInstanceFieldType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Int16_T"),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Cint16_T"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "cint16_T_im",
          hasCFieldInstanceCFieldType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Int16_T"),
          hasCFieldInstanceFieldOffset =
          2},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Cint16_T"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "cint16_T_im",
          hasFieldInstanceFieldType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Int16_T"),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
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
                  ["edge-cases/spec_examples.h"],
                headerInclude =
                "edge-cases/spec_examples.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "B"),
              structSizeof = 0,
              structAlignment = 1,
              structFields = [],
              structFlam = Nothing},
          declSpec = DeclSpec {
            declSpecC = Nothing,
            declSpecHs = Nothing}},
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
                ["edge-cases/spec_examples.h"],
              headerInclude =
              "edge-cases/spec_examples.h"},
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
                      ["edge-cases/spec_examples.h"],
                    headerInclude =
                    "edge-cases/spec_examples.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "B"),
                  structSizeof = 0,
                  structAlignment = 1,
                  structFields = [],
                  structFlam = Nothing},
              declSpec = DeclSpec {
                declSpecC = Nothing,
                declSpecHs = Nothing}},
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
                    ["edge-cases/spec_examples.h"],
                  headerInclude =
                  "edge-cases/spec_examples.h"},
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
                              ["edge-cases/spec_examples.h"],
                            headerInclude =
                            "edge-cases/spec_examples.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "B"),
                          structSizeof = 0,
                          structAlignment = 1,
                          structFields = [],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
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
                            ["edge-cases/spec_examples.h"],
                          headerInclude =
                          "edge-cases/spec_examples.h"},
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
                              ["edge-cases/spec_examples.h"],
                            headerInclude =
                            "edge-cases/spec_examples.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "B"),
                          structSizeof = 0,
                          structAlignment = 1,
                          structFields = [],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
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
                            ["edge-cases/spec_examples.h"],
                          headerInclude =
                          "edge-cases/spec_examples.h"},
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
                    ["edge-cases/spec_examples.h"],
                  headerInclude =
                  "edge-cases/spec_examples.h"},
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
                    ["edge-cases/spec_examples.h"],
                  headerInclude =
                  "edge-cases/spec_examples.h"},
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
                    ["edge-cases/spec_examples.h"],
                  headerInclude =
                  "edge-cases/spec_examples.h"},
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
                    ["edge-cases/spec_examples.h"],
                  headerInclude =
                  "edge-cases/spec_examples.h"},
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
                    ["edge-cases/spec_examples.h"],
                  headerInclude =
                  "edge-cases/spec_examples.h"},
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
                  ["edge-cases/spec_examples.h"],
                headerInclude =
                "edge-cases/spec_examples.h"},
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
          declSpec = DeclSpec {
            declSpecC = Nothing,
            declSpecHs = Nothing}},
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
                ["edge-cases/spec_examples.h"],
              headerInclude =
              "edge-cases/spec_examples.h"},
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
                        ["edge-cases/spec_examples.h"],
                      headerInclude =
                      "edge-cases/spec_examples.h"},
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
                        ["edge-cases/spec_examples.h"],
                      headerInclude =
                      "edge-cases/spec_examples.h"},
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
                        ["edge-cases/spec_examples.h"],
                      headerInclude =
                      "edge-cases/spec_examples.h"},
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
                        ["edge-cases/spec_examples.h"],
                      headerInclude =
                      "edge-cases/spec_examples.h"},
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
                        ["edge-cases/spec_examples.h"],
                      headerInclude =
                      "edge-cases/spec_examples.h"},
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
                      ["edge-cases/spec_examples.h"],
                    headerInclude =
                    "edge-cases/spec_examples.h"},
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
              declSpec = DeclSpec {
                declSpecC = Nothing,
                declSpecHs = Nothing}},
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
                    ["edge-cases/spec_examples.h"],
                  headerInclude =
                  "edge-cases/spec_examples.h"},
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
                                ["edge-cases/spec_examples.h"],
                              headerInclude =
                              "edge-cases/spec_examples.h"},
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
                                ["edge-cases/spec_examples.h"],
                              headerInclude =
                              "edge-cases/spec_examples.h"},
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
                                ["edge-cases/spec_examples.h"],
                              headerInclude =
                              "edge-cases/spec_examples.h"},
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
                                ["edge-cases/spec_examples.h"],
                              headerInclude =
                              "edge-cases/spec_examples.h"},
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
                                ["edge-cases/spec_examples.h"],
                              headerInclude =
                              "edge-cases/spec_examples.h"},
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
                              ["edge-cases/spec_examples.h"],
                            headerInclude =
                            "edge-cases/spec_examples.h"},
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
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
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
                            ["edge-cases/spec_examples.h"],
                          headerInclude =
                          "edge-cases/spec_examples.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "a_x")
                  (Idx 0),
                PeekCField
                  (HsStrLit "a_label")
                  (Idx 0),
                PeekCField
                  (HsStrLit "a_samples")
                  (Idx 0),
                PeekCField
                  (HsStrLit "a_b")
                  (Idx 0),
                PeekCField
                  (HsStrLit "a_c")
                  (Idx 0)]),
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
                                ["edge-cases/spec_examples.h"],
                              headerInclude =
                              "edge-cases/spec_examples.h"},
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
                                ["edge-cases/spec_examples.h"],
                              headerInclude =
                              "edge-cases/spec_examples.h"},
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
                                ["edge-cases/spec_examples.h"],
                              headerInclude =
                              "edge-cases/spec_examples.h"},
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
                                ["edge-cases/spec_examples.h"],
                              headerInclude =
                              "edge-cases/spec_examples.h"},
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
                                ["edge-cases/spec_examples.h"],
                              headerInclude =
                              "edge-cases/spec_examples.h"},
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
                              ["edge-cases/spec_examples.h"],
                            headerInclude =
                            "edge-cases/spec_examples.h"},
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
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
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
                            ["edge-cases/spec_examples.h"],
                          headerInclude =
                          "edge-cases/spec_examples.h"},
                      commentChildren = []}}
                (Add 5)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "a_x")
                      (Idx 6)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "a_label")
                      (Idx 6)
                      (Idx 1),
                    PokeCField
                      (HsStrLit "a_samples")
                      (Idx 6)
                      (Idx 2),
                    PokeCField
                      (HsStrLit "a_b")
                      (Idx 6)
                      (Idx 3),
                    PokeCField
                      (HsStrLit "a_c")
                      (Idx 6)
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
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "A"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "a_x",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCDouble,
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "A"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "a_x",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCDouble,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "A"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "a_label",
          hasCFieldInstanceCFieldType =
          HsPtr (HsPrimType HsPrimCChar),
          hasCFieldInstanceFieldOffset =
          8},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "A"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "a_label",
          hasFieldInstanceFieldType =
          HsPtr (HsPrimType HsPrimCChar),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "A"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "a_samples",
          hasCFieldInstanceCFieldType =
          HsConstArray
            128
            (HsPrimType HsPrimCChar),
          hasCFieldInstanceFieldOffset =
          16},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "A"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "a_samples",
          hasFieldInstanceFieldType =
          HsConstArray
            128
            (HsPrimType HsPrimCChar),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "A"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "a_b",
          hasCFieldInstanceCFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "B"),
          hasCFieldInstanceFieldOffset =
          144},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "A"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "a_b",
          hasFieldInstanceFieldType =
          HsTypRef
            (Name "@NsTypeConstr" "B"),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "A"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "a_c",
          hasCFieldInstanceCFieldType =
          HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "C")),
          hasCFieldInstanceFieldOffset =
          144},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "A"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "a_c",
          hasFieldInstanceFieldType =
          HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "C")),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
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
                ["edge-cases/spec_examples.h"],
              headerInclude =
              "edge-cases/spec_examples.h"},
          declComment = Nothing},
        declKind = Opaque
          (NameKindTagged TagKindStruct),
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
      emptyDataComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "C",
          commentLocation = Just
            "spec_examples.h:28:10",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["edge-cases/spec_examples.h"],
              headerInclude =
              "edge-cases/spec_examples.h"},
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
      "hs_bindgen_test_edgecasesspec_examples_7d4128962cfce15d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_edgecasesspec_examples_7d4128962cfce15d (\n",
              "  int32_T *arg1,\n",
              "  cint16_T *arg2,\n",
              "  int64_T arg3,\n",
              "  int64_T arg4,\n",
              "  cint16_T *arg5\n",
              ")\n",
              "{\n",
              "  resample(arg1, arg2, arg3, arg4, arg5);\n",
              "}"],
          capiWrapperImport =
          "edge-cases/spec_examples.h"},
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
                ["edge-cases/spec_examples.h"],
              headerInclude =
              "edge-cases/spec_examples.h"},
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
      "hs_bindgen_test_edgecasesspec_examples_f31d4400b3244637",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "void hs_bindgen_test_edgecasesspec_examples_f31d4400b3244637 (\n",
              "  int32_T *arg1,\n",
              "  cint16_T *arg2,\n",
              "  int64_T arg3,\n",
              "  int64_T arg4,\n",
              "  cint16_T *arg5\n",
              ")\n",
              "{\n",
              "  resample(arg1, arg2, arg3, arg4, arg5);\n",
              "}"],
          capiWrapperImport =
          "edge-cases/spec_examples.h"},
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
                ["edge-cases/spec_examples.h"],
              headerInclude =
              "edge-cases/spec_examples.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_edgecasesspec_examples_46b04422dcd0bbd5",
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
      "hs_bindgen_test_edgecasesspec_examples_46b04422dcd0bbd5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_resample_ptr */\n",
              "__attribute__ ((const))\n",
              "void (*hs_bindgen_test_edgecasesspec_examples_46b04422dcd0bbd5 (void)) (\n",
              "  int32_T *arg1,\n",
              "  cint16_T arg2[30720000],\n",
              "  int64_T arg3,\n",
              "  int64_T arg4,\n",
              "  cint16_T arg5[30720000]\n",
              ")\n",
              "{\n",
              "  return &resample;\n",
              "}"],
          capiWrapperImport =
          "edge-cases/spec_examples.h"},
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
