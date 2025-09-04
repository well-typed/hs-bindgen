[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Int16_T",
      newtypeConstr = HsName
        "@NsConstr"
        "Int16_T",
      newtypeField = Field {
        fieldName = HsName
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
            nameHsIdent = HsIdentifier
              "Int16_T"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "spec_examples.h",
          declComment = Just
            [
              Paragraph
                [
                  TextContent
                    "Examples from the initial specification"]]},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Int16_T",
              newtypeField = HsName
                "@NsVar"
                "un_Int16_T"},
            typedefType = TypePrim
              (PrimIntegral
                PrimShort
                Signed)},
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
        (Comment
          (Just
            [
              TextContent
                "Examples from the initial specification"])
          (Just "spec_examples.h:10:15")
          (Just "spec_examples.h")
          [])},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Int32_T",
      newtypeConstr = HsName
        "@NsConstr"
        "Int32_T",
      newtypeField = Field {
        fieldName = HsName
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
            nameHsIdent = HsIdentifier
              "Int32_T"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "spec_examples.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Int32_T",
              newtypeField = HsName
                "@NsVar"
                "un_Int32_T"},
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
          Storable],
      newtypeComment = Just
        (Comment
          Nothing
          (Just "spec_examples.h:11:13")
          (Just "spec_examples.h")
          [])},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int32_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Int64_T",
      newtypeConstr = HsName
        "@NsConstr"
        "Int64_T",
      newtypeField = Field {
        fieldName = HsName
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
            nameHsIdent = HsIdentifier
              "Int64_T"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "spec_examples.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Int64_T",
              newtypeField = HsName
                "@NsVar"
                "un_Int64_T"},
            typedefType = TypePrim
              (PrimIntegral
                PrimLongLong
                Signed)},
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
        (Comment
          Nothing
          (Just "spec_examples.h:12:19")
          (Just "spec_examples.h")
          [])},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Int64_T",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Cint16_T",
      structConstr = HsName
        "@NsConstr"
        "Cint16_T",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "cint16_T_re",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Int16_T"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "spec_examples.h:15:11",
                fieldName = NamePair {
                  nameC = Name "re",
                  nameHsIdent = HsIdentifier
                    "cint16_T_re"},
                fieldComment = Nothing},
              structFieldType = TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "int16_T",
                    nameHsIdent = HsIdentifier
                      "Int16_T"}),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "spec_examples.h:15:11")
              (Just "spec_examples.h")
              [])},
        Field {
          fieldName = HsName
            "@NsVar"
            "cint16_T_im",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Int16_T"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "spec_examples.h:16:11",
                fieldName = NamePair {
                  nameC = Name "im",
                  nameHsIdent = HsIdentifier
                    "cint16_T_im"},
                fieldComment = Nothing},
              structFieldType = TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "int16_T",
                    nameHsIdent = HsIdentifier
                      "Int16_T"}),
              structFieldOffset = 16,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "spec_examples.h:16:11")
              (Just "spec_examples.h")
              [])}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "spec_examples.h:14:9",
            declId = NamePair {
              nameC = Name "cint16_T",
              nameHsIdent = HsIdentifier
                "Cint16_T"},
            declOrigin = NameOriginGenerated
              (AnonId "spec_examples.h:14:9"),
            declAliases = [Name "cint16_T"],
            declHeader = "spec_examples.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Cint16_T"),
              structSizeof = 4,
              structAlignment = 2,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "spec_examples.h:15:11",
                    fieldName = NamePair {
                      nameC = Name "re",
                      nameHsIdent = HsIdentifier
                        "cint16_T_re"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "int16_T",
                        nameHsIdent = HsIdentifier
                          "Int16_T"}),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "spec_examples.h:16:11",
                    fieldName = NamePair {
                      nameC = Name "im",
                      nameHsIdent = HsIdentifier
                        "cint16_T_im"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "int16_T",
                        nameHsIdent = HsIdentifier
                          "Int16_T"}),
                  structFieldOffset = 16,
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
        (Comment
          Nothing
          (Just "spec_examples.h:14:9")
          (Just "spec_examples.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Cint16_T",
          structConstr = HsName
            "@NsConstr"
            "Cint16_T",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "cint16_T_re",
              fieldType = HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Int16_T"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "spec_examples.h:15:11",
                    fieldName = NamePair {
                      nameC = Name "re",
                      nameHsIdent = HsIdentifier
                        "cint16_T_re"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "int16_T",
                        nameHsIdent = HsIdentifier
                          "Int16_T"}),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "spec_examples.h:15:11")
                  (Just "spec_examples.h")
                  [])},
            Field {
              fieldName = HsName
                "@NsVar"
                "cint16_T_im",
              fieldType = HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Int16_T"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "spec_examples.h:16:11",
                    fieldName = NamePair {
                      nameC = Name "im",
                      nameHsIdent = HsIdentifier
                        "cint16_T_im"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefRegular
                      NamePair {
                        nameC = Name "int16_T",
                        nameHsIdent = HsIdentifier
                          "Int16_T"}),
                  structFieldOffset = 16,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "spec_examples.h:16:11")
                  (Just "spec_examples.h")
                  [])}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "spec_examples.h:14:9",
                declId = NamePair {
                  nameC = Name "cint16_T",
                  nameHsIdent = HsIdentifier
                    "Cint16_T"},
                declOrigin = NameOriginGenerated
                  (AnonId "spec_examples.h:14:9"),
                declAliases = [Name "cint16_T"],
                declHeader = "spec_examples.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Cint16_T"),
                  structSizeof = 4,
                  structAlignment = 2,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "spec_examples.h:15:11",
                        fieldName = NamePair {
                          nameC = Name "re",
                          nameHsIdent = HsIdentifier
                            "cint16_T_re"},
                        fieldComment = Nothing},
                      structFieldType = TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "int16_T",
                            nameHsIdent = HsIdentifier
                              "Int16_T"}),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "spec_examples.h:16:11",
                        fieldName = NamePair {
                          nameC = Name "im",
                          nameHsIdent = HsIdentifier
                            "cint16_T_im"},
                        fieldComment = Nothing},
                      structFieldType = TypeTypedef
                        (TypedefRegular
                          NamePair {
                            nameC = Name "int16_T",
                            nameHsIdent = HsIdentifier
                              "Int16_T"}),
                      structFieldOffset = 16,
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
            (Comment
              Nothing
              (Just "spec_examples.h:14:9")
              (Just "spec_examples.h")
              [])}
        StorableInstance {
          storableSizeOf = 4,
          storableAlignment = 2,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Cint16_T",
                  structConstr = HsName
                    "@NsConstr"
                    "Cint16_T",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "cint16_T_re",
                      fieldType = HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Int16_T"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:15:11",
                            fieldName = NamePair {
                              nameC = Name "re",
                              nameHsIdent = HsIdentifier
                                "cint16_T_re"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefRegular
                              NamePair {
                                nameC = Name "int16_T",
                                nameHsIdent = HsIdentifier
                                  "Int16_T"}),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "spec_examples.h:15:11")
                          (Just "spec_examples.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "cint16_T_im",
                      fieldType = HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Int16_T"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:16:11",
                            fieldName = NamePair {
                              nameC = Name "im",
                              nameHsIdent = HsIdentifier
                                "cint16_T_im"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefRegular
                              NamePair {
                                nameC = Name "int16_T",
                                nameHsIdent = HsIdentifier
                                  "Int16_T"}),
                          structFieldOffset = 16,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "spec_examples.h:16:11")
                          (Just "spec_examples.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "spec_examples.h:14:9",
                        declId = NamePair {
                          nameC = Name "cint16_T",
                          nameHsIdent = HsIdentifier
                            "Cint16_T"},
                        declOrigin = NameOriginGenerated
                          (AnonId "spec_examples.h:14:9"),
                        declAliases = [Name "cint16_T"],
                        declHeader = "spec_examples.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Cint16_T"),
                          structSizeof = 4,
                          structAlignment = 2,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "spec_examples.h:15:11",
                                fieldName = NamePair {
                                  nameC = Name "re",
                                  nameHsIdent = HsIdentifier
                                    "cint16_T_re"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefRegular
                                  NamePair {
                                    nameC = Name "int16_T",
                                    nameHsIdent = HsIdentifier
                                      "Int16_T"}),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "spec_examples.h:16:11",
                                fieldName = NamePair {
                                  nameC = Name "im",
                                  nameHsIdent = HsIdentifier
                                    "cint16_T_im"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefRegular
                                  NamePair {
                                    nameC = Name "int16_T",
                                    nameHsIdent = HsIdentifier
                                      "Int16_T"}),
                              structFieldOffset = 16,
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
                    (Comment
                      Nothing
                      (Just "spec_examples.h:14:9")
                      (Just "spec_examples.h")
                      [])})
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
                  structName = HsName
                    "@NsTypeConstr"
                    "Cint16_T",
                  structConstr = HsName
                    "@NsConstr"
                    "Cint16_T",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "cint16_T_re",
                      fieldType = HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Int16_T"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:15:11",
                            fieldName = NamePair {
                              nameC = Name "re",
                              nameHsIdent = HsIdentifier
                                "cint16_T_re"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefRegular
                              NamePair {
                                nameC = Name "int16_T",
                                nameHsIdent = HsIdentifier
                                  "Int16_T"}),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "spec_examples.h:15:11")
                          (Just "spec_examples.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "cint16_T_im",
                      fieldType = HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Int16_T"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:16:11",
                            fieldName = NamePair {
                              nameC = Name "im",
                              nameHsIdent = HsIdentifier
                                "cint16_T_im"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefRegular
                              NamePair {
                                nameC = Name "int16_T",
                                nameHsIdent = HsIdentifier
                                  "Int16_T"}),
                          structFieldOffset = 16,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "spec_examples.h:16:11")
                          (Just "spec_examples.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "spec_examples.h:14:9",
                        declId = NamePair {
                          nameC = Name "cint16_T",
                          nameHsIdent = HsIdentifier
                            "Cint16_T"},
                        declOrigin = NameOriginGenerated
                          (AnonId "spec_examples.h:14:9"),
                        declAliases = [Name "cint16_T"],
                        declHeader = "spec_examples.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Cint16_T"),
                          structSizeof = 4,
                          structAlignment = 2,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "spec_examples.h:15:11",
                                fieldName = NamePair {
                                  nameC = Name "re",
                                  nameHsIdent = HsIdentifier
                                    "cint16_T_re"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefRegular
                                  NamePair {
                                    nameC = Name "int16_T",
                                    nameHsIdent = HsIdentifier
                                      "Int16_T"}),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "spec_examples.h:16:11",
                                fieldName = NamePair {
                                  nameC = Name "im",
                                  nameHsIdent = HsIdentifier
                                    "cint16_T_im"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefRegular
                                  NamePair {
                                    nameC = Name "int16_T",
                                    nameHsIdent = HsIdentifier
                                      "Int16_T"}),
                              structFieldOffset = 16,
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
                    (Comment
                      Nothing
                      (Just "spec_examples.h:14:9")
                      (Just "spec_examples.h")
                      [])}
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Cint16_T",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Cint16_T",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "B",
      structConstr = HsName
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
              nameHsIdent = HsIdentifier "B"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "spec_examples.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "B"),
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
          (Just "spec_examples.h:19:8")
          (Just "spec_examples.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "B",
          structConstr = HsName
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
                  nameHsIdent = HsIdentifier "B"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "spec_examples.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "B"),
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
              (Just "spec_examples.h:19:8")
              (Just "spec_examples.h")
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
                    "B",
                  structConstr = HsName
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
                          nameHsIdent = HsIdentifier "B"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "spec_examples.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "B"),
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
                      (Just "spec_examples.h:19:8")
                      (Just "spec_examples.h")
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
                    "B",
                  structConstr = HsName
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
                          nameHsIdent = HsIdentifier "B"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "spec_examples.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "B"),
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
                      (Just "spec_examples.h:19:8")
                      (Just "spec_examples.h")
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
        "B",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "B",
      deriveInstanceComment =
      Nothing},
  DeclEmpty
    EmptyData {
      emptyDataName = HsName
        "@NsTypeConstr"
        "C",
      emptyDataOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "spec_examples.h:28:10",
          declId = NamePair {
            nameC = Name "C",
            nameHsIdent = HsIdentifier "C"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "spec_examples.h",
          declComment = Nothing},
        declKind = OpaqueStruct,
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      emptyDataComment = Just
        (Comment
          Nothing
          (Just "spec_examples.h:28:10")
          (Just "spec_examples.h")
          [])},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "A",
      structConstr = HsName
        "@NsConstr"
        "A",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "a_x",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "spec_examples.h:24:10",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier
                    "a_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "spec_examples.h:24:10")
              (Just "spec_examples.h")
              [])},
        Field {
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
            (Comment
              Nothing
              (Just "spec_examples.h:25:9")
              (Just "spec_examples.h")
              [])},
        Field {
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
            (Comment
              Nothing
              (Just "spec_examples.h:26:8")
              (Just "spec_examples.h")
              [])},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_b",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "B"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "spec_examples.h:27:12",
                fieldName = NamePair {
                  nameC = Name "b",
                  nameHsIdent = HsIdentifier
                    "a_b"},
                fieldComment = Nothing},
              structFieldType = TypeStruct
                NamePair {
                  nameC = Name "B",
                  nameHsIdent = HsIdentifier "B"}
                NameOriginInSource,
              structFieldOffset = 1152,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "spec_examples.h:27:12")
              (Just "spec_examples.h")
              [])},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_c",
          fieldType = HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "C")),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "spec_examples.h:28:13",
                fieldName = NamePair {
                  nameC = Name "c",
                  nameHsIdent = HsIdentifier
                    "a_c"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeStruct
                  NamePair {
                    nameC = Name "C",
                    nameHsIdent = HsIdentifier "C"}
                  NameOriginInSource),
              structFieldOffset = 1152,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "spec_examples.h:28:13")
              (Just "spec_examples.h")
              [])}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "spec_examples.h:23:8",
            declId = NamePair {
              nameC = Name "A",
              nameHsIdent = HsIdentifier "A"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "spec_examples.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "A"),
              structSizeof = 152,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "spec_examples.h:24:10",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "a_x"},
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
                      nameHsIdent = HsIdentifier
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
                      nameHsIdent = HsIdentifier
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
                      nameHsIdent = HsIdentifier
                        "a_b"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "B",
                      nameHsIdent = HsIdentifier "B"}
                    NameOriginInSource,
                  structFieldOffset = 1152,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "spec_examples.h:28:13",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = HsIdentifier
                        "a_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "C",
                        nameHsIdent = HsIdentifier "C"}
                      NameOriginInSource),
                  structFieldOffset = 1152,
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
        (Comment
          Nothing
          (Just "spec_examples.h:23:8")
          (Just "spec_examples.h")
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
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "a_x",
              fieldType = HsPrimType
                HsPrimCDouble,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "spec_examples.h:24:10",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "a_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "spec_examples.h:24:10")
                  (Just "spec_examples.h")
                  [])},
            Field {
              fieldName = HsName
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
                      nameHsIdent = HsIdentifier
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
                (Comment
                  Nothing
                  (Just "spec_examples.h:25:9")
                  (Just "spec_examples.h")
                  [])},
            Field {
              fieldName = HsName
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
                      nameHsIdent = HsIdentifier
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
                (Comment
                  Nothing
                  (Just "spec_examples.h:26:8")
                  (Just "spec_examples.h")
                  [])},
            Field {
              fieldName = HsName
                "@NsVar"
                "a_b",
              fieldType = HsTypRef
                (HsName "@NsTypeConstr" "B"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "spec_examples.h:27:12",
                    fieldName = NamePair {
                      nameC = Name "b",
                      nameHsIdent = HsIdentifier
                        "a_b"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name "B",
                      nameHsIdent = HsIdentifier "B"}
                    NameOriginInSource,
                  structFieldOffset = 1152,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "spec_examples.h:27:12")
                  (Just "spec_examples.h")
                  [])},
            Field {
              fieldName = HsName
                "@NsVar"
                "a_c",
              fieldType = HsPtr
                (HsTypRef
                  (HsName "@NsTypeConstr" "C")),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "spec_examples.h:28:13",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = HsIdentifier
                        "a_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = Name "C",
                        nameHsIdent = HsIdentifier "C"}
                      NameOriginInSource),
                  structFieldOffset = 1152,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "spec_examples.h:28:13")
                  (Just "spec_examples.h")
                  [])}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "spec_examples.h:23:8",
                declId = NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "spec_examples.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "A"),
                  structSizeof = 152,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "spec_examples.h:24:10",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "a_x"},
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
                          nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
                            "a_b"},
                        fieldComment = Nothing},
                      structFieldType = TypeStruct
                        NamePair {
                          nameC = Name "B",
                          nameHsIdent = HsIdentifier "B"}
                        NameOriginInSource,
                      structFieldOffset = 1152,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "spec_examples.h:28:13",
                        fieldName = NamePair {
                          nameC = Name "c",
                          nameHsIdent = HsIdentifier
                            "a_c"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeStruct
                          NamePair {
                            nameC = Name "C",
                            nameHsIdent = HsIdentifier "C"}
                          NameOriginInSource),
                      structFieldOffset = 1152,
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
            (Comment
              Nothing
              (Just "spec_examples.h:23:8")
              (Just "spec_examples.h")
              [])}
        StorableInstance {
          storableSizeOf = 152,
          storableAlignment = 8,
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
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "a_x",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:24:10",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "a_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "spec_examples.h:24:10")
                          (Just "spec_examples.h")
                          [])},
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                        (Comment
                          Nothing
                          (Just "spec_examples.h:25:9")
                          (Just "spec_examples.h")
                          [])},
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                        (Comment
                          Nothing
                          (Just "spec_examples.h:26:8")
                          (Just "spec_examples.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "a_b",
                      fieldType = HsTypRef
                        (HsName "@NsTypeConstr" "B"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:27:12",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = HsIdentifier
                                "a_b"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "B",
                              nameHsIdent = HsIdentifier "B"}
                            NameOriginInSource,
                          structFieldOffset = 1152,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "spec_examples.h:27:12")
                          (Just "spec_examples.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "a_c",
                      fieldType = HsPtr
                        (HsTypRef
                          (HsName "@NsTypeConstr" "C")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:28:13",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = HsIdentifier
                                "a_c"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "C",
                                nameHsIdent = HsIdentifier "C"}
                              NameOriginInSource),
                          structFieldOffset = 1152,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "spec_examples.h:28:13")
                          (Just "spec_examples.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "spec_examples.h:23:8",
                        declId = NamePair {
                          nameC = Name "A",
                          nameHsIdent = HsIdentifier "A"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "spec_examples.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "A"),
                          structSizeof = 152,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "spec_examples.h:24:10",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "a_x"},
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
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
                                    "a_b"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "B",
                                  nameHsIdent = HsIdentifier "B"}
                                NameOriginInSource,
                              structFieldOffset = 1152,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "spec_examples.h:28:13",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = HsIdentifier
                                    "a_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "C",
                                    nameHsIdent = HsIdentifier "C"}
                                  NameOriginInSource),
                              structFieldOffset = 1152,
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
                    (Comment
                      Nothing
                      (Just "spec_examples.h:23:8")
                      (Just "spec_examples.h")
                      [])})
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
                  structName = HsName
                    "@NsTypeConstr"
                    "A",
                  structConstr = HsName
                    "@NsConstr"
                    "A",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "a_x",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:24:10",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "a_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "spec_examples.h:24:10")
                          (Just "spec_examples.h")
                          [])},
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                        (Comment
                          Nothing
                          (Just "spec_examples.h:25:9")
                          (Just "spec_examples.h")
                          [])},
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                        (Comment
                          Nothing
                          (Just "spec_examples.h:26:8")
                          (Just "spec_examples.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "a_b",
                      fieldType = HsTypRef
                        (HsName "@NsTypeConstr" "B"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:27:12",
                            fieldName = NamePair {
                              nameC = Name "b",
                              nameHsIdent = HsIdentifier
                                "a_b"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name "B",
                              nameHsIdent = HsIdentifier "B"}
                            NameOriginInSource,
                          structFieldOffset = 1152,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "spec_examples.h:27:12")
                          (Just "spec_examples.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "a_c",
                      fieldType = HsPtr
                        (HsTypRef
                          (HsName "@NsTypeConstr" "C")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "spec_examples.h:28:13",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = HsIdentifier
                                "a_c"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = Name "C",
                                nameHsIdent = HsIdentifier "C"}
                              NameOriginInSource),
                          structFieldOffset = 1152,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "spec_examples.h:28:13")
                          (Just "spec_examples.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "spec_examples.h:23:8",
                        declId = NamePair {
                          nameC = Name "A",
                          nameHsIdent = HsIdentifier "A"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "spec_examples.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "A"),
                          structSizeof = 152,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "spec_examples.h:24:10",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "a_x"},
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
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
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
                                  nameHsIdent = HsIdentifier
                                    "a_b"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name "B",
                                  nameHsIdent = HsIdentifier "B"}
                                NameOriginInSource,
                              structFieldOffset = 1152,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "spec_examples.h:28:13",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = HsIdentifier
                                    "a_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = Name "C",
                                    nameHsIdent = HsIdentifier "C"}
                                  NameOriginInSource),
                              structFieldOffset = 1152,
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
                    (Comment
                      Nothing
                      (Just "spec_examples.h:23:8")
                      (Just "spec_examples.h")
                      [])}
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
  DeclInlineCInclude
    "spec_examples.h",
  DeclInlineC
    "void hs_bindgen_test_spec_examples_bab0544b0c2274da (int32_T *arg1, cint16_T *arg2, int64_T arg3, int64_T arg4, cint16_T *arg5) { resample(arg1, arg2, arg3, arg4, arg5); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "resample_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName
              "@NsVar"
              "res_m_num_valid_samples"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Int32_T")),
          functionParameterComment = Just
            (Comment
              Nothing
              Nothing
              Nothing
              [])},
        FunctionParameter {
          functionParameterName = Just
            (HsName
              "@NsVar"
              "res_m_iq_int"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Cint16_T")),
          functionParameterComment = Just
            (Comment
              Nothing
              Nothing
              Nothing
              [])},
        FunctionParameter {
          functionParameterName = Just
            (HsName
              "@NsVar"
              "res_m_old_rate"),
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Int64_T"),
          functionParameterComment = Just
            (Comment
              Nothing
              Nothing
              Nothing
              [])},
        FunctionParameter {
          functionParameterName = Just
            (HsName
              "@NsVar"
              "res_m_new_rate"),
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Int64_T"),
          functionParameterComment = Just
            (Comment
              Nothing
              Nothing
              Nothing
              [])},
        FunctionParameter {
          functionParameterName = Just
            (HsName
              "@NsVar"
              "res_m_iq_resampled_int"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Cint16_T")),
          functionParameterComment = Just
            (Comment
              Nothing
              Nothing
              Nothing
              [])}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_spec_examples_bab0544b0c2274da",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name
                    "res_m_num_valid_samples",
                  nameHsIdent = HsIdentifier
                    "res_m_num_valid_samples"})
              (TypePointer
                (TypeTypedef
                  (TypedefRegular
                    NamePair {
                      nameC = Name "int32_T",
                      nameHsIdent = HsIdentifier
                        "Int32_T"}))),
            _×_
              (Just
                NamePair {
                  nameC = Name "res_m_iq_int",
                  nameHsIdent = HsIdentifier
                    "res_m_iq_int"})
              (TypeConstArray
                30720000
                (TypeTypedef
                  (TypedefSquashed
                    (Name "cint16_T")
                    (TypeStruct
                      NamePair {
                        nameC = Name "cint16_T",
                        nameHsIdent = HsIdentifier
                          "Cint16_T"}
                      (NameOriginGenerated
                        (AnonId
                          "spec_examples.h:14:9")))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "res_m_old_rate",
                  nameHsIdent = HsIdentifier
                    "res_m_old_rate"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "int64_T",
                    nameHsIdent = HsIdentifier
                      "Int64_T"})),
            _×_
              (Just
                NamePair {
                  nameC = Name "res_m_new_rate",
                  nameHsIdent = HsIdentifier
                    "res_m_new_rate"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "int64_T",
                    nameHsIdent = HsIdentifier
                      "Int64_T"})),
            _×_
              (Just
                NamePair {
                  nameC = Name
                    "res_m_iq_resampled_int",
                  nameHsIdent = HsIdentifier
                    "res_m_iq_resampled_int"})
              (TypeConstArray
                30720000
                (TypeTypedef
                  (TypedefSquashed
                    (Name "cint16_T")
                    (TypeStruct
                      NamePair {
                        nameC = Name "cint16_T",
                        nameHsIdent = HsIdentifier
                          "Cint16_T"}
                      (NameOriginGenerated
                        (AnonId
                          "spec_examples.h:14:9"))))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        (Comment
          Nothing
          (Just "spec_examples.h:31:6")
          (Just "spec_examples.h")
          []),
      foreignImportSafety = Safe},
  DeclSimple,
  DeclInlineCInclude
    "spec_examples.h",
  DeclInlineC
    "/* get_resample_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_spec_examples_215c42c65ae193a6 (void)) (int32_T *arg1, cint16_T arg2[30720000], int64_T arg3, int64_T arg4, cint16_T arg5[30720000]) { return &resample; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_spec_examples_215c42c65ae193a6",
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
                    "Int32_T")))
              (HsFun
                (HsConstArray
                  30720000
                  (HsTypRef
                    (HsName
                      "@NsTypeConstr"
                      "Cint16_T")))
                (HsFun
                  (HsTypRef
                    (HsName
                      "@NsTypeConstr"
                      "Int64_T"))
                  (HsFun
                    (HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Int64_T"))
                    (HsFun
                      (HsConstArray
                        30720000
                        (HsTypRef
                          (HsName
                            "@NsTypeConstr"
                            "Cint16_T")))
                      (HsIO
                        (HsPrimType HsPrimUnit))))))))),
      foreignImportOrigName =
      "hs_bindgen_test_spec_examples_215c42c65ae193a6",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "int32_T",
                    nameHsIdent = HsIdentifier
                      "Int32_T"})),
            TypeConstArray
              30720000
              (TypeTypedef
                (TypedefSquashed
                  (Name "cint16_T")
                  (TypeStruct
                    NamePair {
                      nameC = Name "cint16_T",
                      nameHsIdent = HsIdentifier
                        "Cint16_T"}
                    (NameOriginGenerated
                      (AnonId
                        "spec_examples.h:14:9"))))),
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "int64_T",
                  nameHsIdent = HsIdentifier
                    "Int64_T"}),
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "int64_T",
                  nameHsIdent = HsIdentifier
                    "Int64_T"}),
            TypeConstArray
              30720000
              (TypeTypedef
                (TypedefSquashed
                  (Name "cint16_T")
                  (TypeStruct
                    NamePair {
                      nameC = Name "cint16_T",
                      nameHsIdent = HsIdentifier
                        "Cint16_T"}
                    (NameOriginGenerated
                      (AnonId
                        "spec_examples.h:14:9")))))]
          TypeVoid),
      foreignImportComment = Just
        (Comment
          Nothing
          (Just "spec_examples.h:31:6")
          (Just "spec_examples.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
