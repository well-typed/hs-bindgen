[
  DeclEmpty
    EmptyData {
      emptyDataName = HsName
        "@NsTypeConstr"
        "Opaque",
      emptyDataOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "decls_in_signature.h:2:8",
          declId = NamePair {
            nameC = Name "opaque",
            nameHsIdent = HsIdentifier
              "Opaque"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "decls_in_signature.h",
          declComment = Nothing},
        declKind = OpaqueStruct,
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      emptyDataComment = Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Outside",
      structConstr = HsName
        "@NsConstr"
        "Outside",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "outside_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "decls_in_signature.h:4:7",
              structFieldName = NamePair {
                nameC = Name "x",
                nameHsIdent = HsIdentifier
                  "outside_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing},
        Field {
          fieldName = HsName
            "@NsVar"
            "outside_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "decls_in_signature.h:5:7",
              structFieldName = NamePair {
                nameC = Name "y",
                nameHsIdent = HsIdentifier
                  "outside_y"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "decls_in_signature.h:3:8",
            declId = NamePair {
              nameC = Name "outside",
              nameHsIdent = HsIdentifier
                "Outside"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "decls_in_signature.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Outside"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "decls_in_signature.h:4:7",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "outside_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
                StructField {
                  structFieldLoc =
                  "decls_in_signature.h:5:7",
                  structFieldName = NamePair {
                    nameC = Name "y",
                    nameHsIdent = HsIdentifier
                      "outside_y"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            TypeSpec {
              typeSpecModule = Nothing,
              typeSpecIdentifier = Nothing,
              typeSpecInstances = Map.fromList
                []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Outside",
          structConstr = HsName
            "@NsConstr"
            "Outside",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "outside_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc =
                  "decls_in_signature.h:4:7",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "outside_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing},
            Field {
              fieldName = HsName
                "@NsVar"
                "outside_y",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc =
                  "decls_in_signature.h:5:7",
                  structFieldName = NamePair {
                    nameC = Name "y",
                    nameHsIdent = HsIdentifier
                      "outside_y"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "decls_in_signature.h:3:8",
                declId = NamePair {
                  nameC = Name "outside",
                  nameHsIdent = HsIdentifier
                    "Outside"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader =
                "decls_in_signature.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Outside"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldLoc =
                      "decls_in_signature.h:4:7",
                      structFieldName = NamePair {
                        nameC = Name "x",
                        nameHsIdent = HsIdentifier
                          "outside_x"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing},
                    StructField {
                      structFieldLoc =
                      "decls_in_signature.h:5:7",
                      structFieldName = NamePair {
                        nameC = Name "y",
                        nameHsIdent = HsIdentifier
                          "outside_y"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                TypeSpec {
                  typeSpecModule = Nothing,
                  typeSpecIdentifier = Nothing,
                  typeSpecInstances = Map.fromList
                    []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Nothing}
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
                    "Outside",
                  structConstr = HsName
                    "@NsConstr"
                    "Outside",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "outside_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "decls_in_signature.h:4:7",
                          structFieldName = NamePair {
                            nameC = Name "x",
                            nameHsIdent = HsIdentifier
                              "outside_x"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "outside_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "decls_in_signature.h:5:7",
                          structFieldName = NamePair {
                            nameC = Name "y",
                            nameHsIdent = HsIdentifier
                              "outside_y"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "decls_in_signature.h:3:8",
                        declId = NamePair {
                          nameC = Name "outside",
                          nameHsIdent = HsIdentifier
                            "Outside"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "decls_in_signature.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Outside"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "decls_in_signature.h:4:7",
                              structFieldName = NamePair {
                                nameC = Name "x",
                                nameHsIdent = HsIdentifier
                                  "outside_x"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc =
                              "decls_in_signature.h:5:7",
                              structFieldName = NamePair {
                                nameC = Name "y",
                                nameHsIdent = HsIdentifier
                                  "outside_y"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Nothing})
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
                    "Outside",
                  structConstr = HsName
                    "@NsConstr"
                    "Outside",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "outside_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "decls_in_signature.h:4:7",
                          structFieldName = NamePair {
                            nameC = Name "x",
                            nameHsIdent = HsIdentifier
                              "outside_x"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "outside_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "decls_in_signature.h:5:7",
                          structFieldName = NamePair {
                            nameC = Name "y",
                            nameHsIdent = HsIdentifier
                              "outside_y"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "decls_in_signature.h:3:8",
                        declId = NamePair {
                          nameC = Name "outside",
                          nameHsIdent = HsIdentifier
                            "Outside"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "decls_in_signature.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Outside"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "decls_in_signature.h:4:7",
                              structFieldName = NamePair {
                                nameC = Name "x",
                                nameHsIdent = HsIdentifier
                                  "outside_x"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc =
                              "decls_in_signature.h:5:7",
                              structFieldName = NamePair {
                                nameC = Name "y",
                                nameHsIdent = HsIdentifier
                                  "outside_y"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Nothing}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      4
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
        "Outside",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Outside",
      deriveInstanceComment =
      Nothing},
  DeclInlineCInclude
    "decls_in_signature.h",
  DeclInlineC
    "void testmodule_normal (struct opaque *arg1, struct outside *arg2, struct outside *arg3) { normal(arg1, arg2, *arg3); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "normal_wrapper",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Opaque")))
        (HsFun
          (HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Outside")))
          (HsFun
            (HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Outside")))
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "testmodule_normal",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name "opaque",
                  nameHsIdent = HsIdentifier
                    "Opaque"}
                NameOriginInSource),
            TypePointer
              (TypeStruct
                NamePair {
                  nameC = Name "outside",
                  nameHsIdent = HsIdentifier
                    "Outside"}
                NameOriginInSource),
            TypeStruct
              NamePair {
                nameC = Name "outside",
                nameHsIdent = HsIdentifier
                  "Outside"}
              NameOriginInSource],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Nothing},
  DeclSimple,
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Named_struct",
      structConstr = HsName
        "@NsConstr"
        "Named_struct",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "named_struct_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "decls_in_signature.h:17:35",
              structFieldName = NamePair {
                nameC = Name "x",
                nameHsIdent = HsIdentifier
                  "named_struct_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing},
        Field {
          fieldName = HsName
            "@NsVar"
            "named_struct_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "decls_in_signature.h:17:42",
              structFieldName = NamePair {
                nameC = Name "y",
                nameHsIdent = HsIdentifier
                  "named_struct_y"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
      structOrigin =
      Just
        Decl {
          declInfo =
          DeclInfo {
            declLoc =
            "decls_in_signature.h:17:16",
            declId = NamePair {
              nameC = Name "named_struct",
              nameHsIdent = HsIdentifier
                "Named_struct"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "decls_in_signature.h",
            declComment =
            Just
              [
                Paragraph
                  [TextContent "Error cases"],
                Paragraph
                  [
                    TextContent
                      "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                    TextContent
                      "and the edge cases below)."]]},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Named_struct"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "decls_in_signature.h:17:35",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "named_struct_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
                StructField {
                  structFieldLoc =
                  "decls_in_signature.h:17:42",
                  structFieldName = NamePair {
                    nameC = Name "y",
                    nameHsIdent = HsIdentifier
                      "named_struct_y"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            TypeSpec {
              typeSpecModule = Nothing,
              typeSpecIdentifier = Nothing,
              typeSpecInstances = Map.fromList
                []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment =
      Just
        Comment {
          commentTitle = Just
            [TextContent "Error cases"],
          commentOrigin = Just
            "named_struct",
          commentChildren =
          [
            Paragraph
              [
                TextContent
                  "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                TextContent
                  "and the edge cases below)."]]}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Named_struct",
          structConstr = HsName
            "@NsConstr"
            "Named_struct",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "named_struct_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc =
                  "decls_in_signature.h:17:35",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "named_struct_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing},
            Field {
              fieldName = HsName
                "@NsVar"
                "named_struct_y",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc =
                  "decls_in_signature.h:17:42",
                  structFieldName = NamePair {
                    nameC = Name "y",
                    nameHsIdent = HsIdentifier
                      "named_struct_y"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin =
          Just
            Decl {
              declInfo =
              DeclInfo {
                declLoc =
                "decls_in_signature.h:17:16",
                declId = NamePair {
                  nameC = Name "named_struct",
                  nameHsIdent = HsIdentifier
                    "Named_struct"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader =
                "decls_in_signature.h",
                declComment =
                Just
                  [
                    Paragraph
                      [TextContent "Error cases"],
                    Paragraph
                      [
                        TextContent
                          "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                        TextContent
                          "and the edge cases below)."]]},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName
                      "@NsConstr"
                      "Named_struct"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldLoc =
                      "decls_in_signature.h:17:35",
                      structFieldName = NamePair {
                        nameC = Name "x",
                        nameHsIdent = HsIdentifier
                          "named_struct_x"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing},
                    StructField {
                      structFieldLoc =
                      "decls_in_signature.h:17:42",
                      structFieldName = NamePair {
                        nameC = Name "y",
                        nameHsIdent = HsIdentifier
                          "named_struct_y"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                TypeSpec {
                  typeSpecModule = Nothing,
                  typeSpecIdentifier = Nothing,
                  typeSpecInstances = Map.fromList
                    []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment =
          Just
            Comment {
              commentTitle = Just
                [TextContent "Error cases"],
              commentOrigin = Just
                "named_struct",
              commentChildren =
              [
                Paragraph
                  [
                    TextContent
                      "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                    TextContent
                      "and the edge cases below)."]]}}
        StorableInstance {
          storableSizeOf = 8,
          storableAlignment = 4,
          storablePeek =
          Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Named_struct",
                  structConstr = HsName
                    "@NsConstr"
                    "Named_struct",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "named_struct_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "decls_in_signature.h:17:35",
                          structFieldName = NamePair {
                            nameC = Name "x",
                            nameHsIdent = HsIdentifier
                              "named_struct_x"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "named_struct_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "decls_in_signature.h:17:42",
                          structFieldName = NamePair {
                            nameC = Name "y",
                            nameHsIdent = HsIdentifier
                              "named_struct_y"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin =
                  Just
                    Decl {
                      declInfo =
                      DeclInfo {
                        declLoc =
                        "decls_in_signature.h:17:16",
                        declId = NamePair {
                          nameC = Name "named_struct",
                          nameHsIdent = HsIdentifier
                            "Named_struct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "decls_in_signature.h",
                        declComment =
                        Just
                          [
                            Paragraph
                              [TextContent "Error cases"],
                            Paragraph
                              [
                                TextContent
                                  "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                                TextContent
                                  "and the edge cases below)."]]},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Named_struct"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "decls_in_signature.h:17:35",
                              structFieldName = NamePair {
                                nameC = Name "x",
                                nameHsIdent = HsIdentifier
                                  "named_struct_x"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc =
                              "decls_in_signature.h:17:42",
                              structFieldName = NamePair {
                                nameC = Name "y",
                                nameHsIdent = HsIdentifier
                                  "named_struct_y"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment =
                  Just
                    Comment {
                      commentTitle = Just
                        [TextContent "Error cases"],
                      commentOrigin = Just
                        "named_struct",
                      commentChildren =
                      [
                        Paragraph
                          [
                            TextContent
                              "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                            TextContent
                              "and the edge cases below)."]]}})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 4]),
          storablePoke =
          Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Named_struct",
                  structConstr = HsName
                    "@NsConstr"
                    "Named_struct",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "named_struct_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "decls_in_signature.h:17:35",
                          structFieldName = NamePair {
                            nameC = Name "x",
                            nameHsIdent = HsIdentifier
                              "named_struct_x"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "named_struct_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "decls_in_signature.h:17:42",
                          structFieldName = NamePair {
                            nameC = Name "y",
                            nameHsIdent = HsIdentifier
                              "named_struct_y"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin =
                  Just
                    Decl {
                      declInfo =
                      DeclInfo {
                        declLoc =
                        "decls_in_signature.h:17:16",
                        declId = NamePair {
                          nameC = Name "named_struct",
                          nameHsIdent = HsIdentifier
                            "Named_struct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "decls_in_signature.h",
                        declComment =
                        Just
                          [
                            Paragraph
                              [TextContent "Error cases"],
                            Paragraph
                              [
                                TextContent
                                  "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                                TextContent
                                  "and the edge cases below)."]]},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Named_struct"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "decls_in_signature.h:17:35",
                              structFieldName = NamePair {
                                nameC = Name "x",
                                nameHsIdent = HsIdentifier
                                  "named_struct_x"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc =
                              "decls_in_signature.h:17:42",
                              structFieldName = NamePair {
                                nameC = Name "y",
                                nameHsIdent = HsIdentifier
                                  "named_struct_y"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment =
                  Just
                    Comment {
                      commentTitle = Just
                        [TextContent "Error cases"],
                      commentOrigin = Just
                        "named_struct",
                      commentChildren =
                      [
                        Paragraph
                          [
                            TextContent
                              "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                            TextContent
                              "and the edge cases below)."]]}}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      4
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
        "Named_struct",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Named_struct",
      deriveInstanceComment =
      Nothing},
  DeclInlineCInclude
    "decls_in_signature.h",
  DeclInlineC
    "void testmodule_f1 (struct named_struct *arg1) { f1(*arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "f1_wrapper",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Named_struct")))
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "testmodule_f1",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypeStruct
              NamePair {
                nameC = Name "named_struct",
                nameHsIdent = HsIdentifier
                  "Named_struct"}
              NameOriginInSource],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment =
      Just
        Comment {
          commentTitle = Just
            [TextContent "Error cases"],
          commentOrigin = Just
            "f1(struct named_struct)",
          commentChildren =
          [
            Paragraph
              [
                TextContent
                  "See 'UnexpectedAnonInSignature' for discussion (of both these error cases",
                TextContent
                  "and the edge cases below)."]]}},
  DeclSimple,
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Named_union",
      newtypeConstr = HsName
        "@NsConstr"
        "Named_union",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Named_union",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "decls_in_signature.h:20:15",
          declId = NamePair {
            nameC = Name "named_union",
            nameHsIdent = HsIdentifier
              "Named_union"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "decls_in_signature.h",
          declComment = Nothing},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Named_union",
              newtypeField = HsName
                "@NsVar"
                "un_Named_union"},
            unionSizeof = 4,
            unionAlignment = 4,
            unionFields = [
              UnionField {
                unionFieldLoc =
                "decls_in_signature.h:20:33",
                unionFieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier
                    "named_union_x"},
                unionFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                unionFieldComment = Nothing},
              UnionField {
                unionFieldLoc =
                "decls_in_signature.h:20:41",
                unionFieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier
                    "named_union_y"},
                unionFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                unionFieldComment = Nothing}]},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Storable],
      newtypeComment = Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveVia
        (HsSizedByteArray 4 4),
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Named_union",
      deriveInstanceComment =
      Nothing},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = HsName
        "@NsVar"
        "get_named_union_x",
      unionGetterType = HsPrimType
        HsPrimCInt,
      unionGetterConstr = HsName
        "@NsTypeConstr"
        "Named_union",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_named_union_x"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = HsName
        "@NsVar"
        "set_named_union_x",
      unionSetterType = HsPrimType
        HsPrimCInt,
      unionSetterConstr = HsName
        "@NsTypeConstr"
        "Named_union",
      unionSetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "get_named_union_x"]]}},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = HsName
        "@NsVar"
        "get_named_union_y",
      unionGetterType = HsPrimType
        HsPrimCChar,
      unionGetterConstr = HsName
        "@NsTypeConstr"
        "Named_union",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_named_union_y"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = HsName
        "@NsVar"
        "set_named_union_y",
      unionSetterType = HsPrimType
        HsPrimCChar,
      unionSetterConstr = HsName
        "@NsTypeConstr"
        "Named_union",
      unionSetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "get_named_union_y"]]}},
  DeclInlineCInclude
    "decls_in_signature.h",
  DeclInlineC
    "void testmodule_f2 (union named_union *arg1) { f2(*arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "f2_wrapper",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Named_union")))
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "testmodule_f2",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypeUnion
              NamePair {
                nameC = Name "named_union",
                nameHsIdent = HsIdentifier
                  "Named_union"}
              NameOriginInSource],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Nothing},
  DeclSimple]
