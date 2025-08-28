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
          declHeader =
          "macro_in_fundecl_vs_typedef.h",
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
      newtypeComment = Nothing},
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
          declHeader =
          "macro_in_fundecl_vs_typedef.h",
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
      newtypeComment = Nothing},
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
  DeclInlineCInclude
    "macro_in_fundecl_vs_typedef.h",
  DeclInlineC
    "char hs_bindgen_test_macro_in_fundecl_vs_typedef_07fab5dfa3fd2fad (MC arg1, TC arg2) { return quux1(arg1, arg2); }",
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
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCChar)),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_07fab5dfa3fd2fad",
      foreignImportCallConv =
      CallConvUserlandCAPI,
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
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "macro_in_fundecl_vs_typedef.h",
  DeclInlineC
    "TC hs_bindgen_test_macro_in_fundecl_vs_typedef_63e619d3916718c2 (MC arg1, char arg2) { return quux2(arg1, arg2); }",
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
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName "@NsTypeConstr" "TC"))),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_63e619d3916718c2",
      foreignImportCallConv =
      CallConvUserlandCAPI,
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
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "macro_in_fundecl_vs_typedef.h",
  DeclInlineC
    "MC *hs_bindgen_test_macro_in_fundecl_vs_typedef_cf2edbc5f779e4a0 (float arg1, TC *arg2) { return wam1(arg1, arg2); }",
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
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_cf2edbc5f779e4a0",
      foreignImportCallConv =
      CallConvUserlandCAPI,
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
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "macro_in_fundecl_vs_typedef.h",
  DeclInlineC
    "TC *hs_bindgen_test_macro_in_fundecl_vs_typedef_261e915bc628d210 (float arg1, MC *arg2) { return wam2(arg1, arg2); }",
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
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_261e915bc628d210",
      foreignImportCallConv =
      CallConvUserlandCAPI,
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
          commentChildren = []},
      foreignImportSafety = Safe},
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
              structFieldLoc =
              "macro_in_fundecl_vs_typedef.h:18:30",
              structFieldName = NamePair {
                nameC = Name "a",
                nameHsIdent = HsIdentifier
                  "struct1_a"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
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
            declHeader =
            "macro_in_fundecl_vs_typedef.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Struct1"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "macro_in_fundecl_vs_typedef.h:18:30",
                  structFieldName = NamePair {
                    nameC = Name "a",
                    nameHsIdent = HsIdentifier
                      "struct1_a"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
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
                  structFieldLoc =
                  "macro_in_fundecl_vs_typedef.h:18:30",
                  structFieldName = NamePair {
                    nameC = Name "a",
                    nameHsIdent = HsIdentifier
                      "struct1_a"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
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
                declHeader =
                "macro_in_fundecl_vs_typedef.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Struct1"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldLoc =
                      "macro_in_fundecl_vs_typedef.h:18:30",
                      structFieldName = NamePair {
                        nameC = Name "a",
                        nameHsIdent = HsIdentifier
                          "struct1_a"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
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
                          structFieldLoc =
                          "macro_in_fundecl_vs_typedef.h:18:30",
                          structFieldName = NamePair {
                            nameC = Name "a",
                            nameHsIdent = HsIdentifier
                              "struct1_a"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
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
                        declHeader =
                        "macro_in_fundecl_vs_typedef.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Struct1"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "macro_in_fundecl_vs_typedef.h:18:30",
                              structFieldName = NamePair {
                                nameC = Name "a",
                                nameHsIdent = HsIdentifier
                                  "struct1_a"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
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
                          structFieldLoc =
                          "macro_in_fundecl_vs_typedef.h:18:30",
                          structFieldName = NamePair {
                            nameC = Name "a",
                            nameHsIdent = HsIdentifier
                              "struct1_a"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
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
                        declHeader =
                        "macro_in_fundecl_vs_typedef.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Struct1"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "macro_in_fundecl_vs_typedef.h:18:30",
                              structFieldName = NamePair {
                                nameC = Name "a",
                                nameHsIdent = HsIdentifier
                                  "struct1_a"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
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
              structFieldLoc =
              "macro_in_fundecl_vs_typedef.h:19:30",
              structFieldName = NamePair {
                nameC = Name "a",
                nameHsIdent = HsIdentifier
                  "struct2_a"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
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
            declHeader =
            "macro_in_fundecl_vs_typedef.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Struct2"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "macro_in_fundecl_vs_typedef.h:19:30",
                  structFieldName = NamePair {
                    nameC = Name "a",
                    nameHsIdent = HsIdentifier
                      "struct2_a"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
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
                  structFieldLoc =
                  "macro_in_fundecl_vs_typedef.h:19:30",
                  structFieldName = NamePair {
                    nameC = Name "a",
                    nameHsIdent = HsIdentifier
                      "struct2_a"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
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
                declHeader =
                "macro_in_fundecl_vs_typedef.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Struct2"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldLoc =
                      "macro_in_fundecl_vs_typedef.h:19:30",
                      structFieldName = NamePair {
                        nameC = Name "a",
                        nameHsIdent = HsIdentifier
                          "struct2_a"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
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
                          structFieldLoc =
                          "macro_in_fundecl_vs_typedef.h:19:30",
                          structFieldName = NamePair {
                            nameC = Name "a",
                            nameHsIdent = HsIdentifier
                              "struct2_a"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
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
                        declHeader =
                        "macro_in_fundecl_vs_typedef.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Struct2"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "macro_in_fundecl_vs_typedef.h:19:30",
                              structFieldName = NamePair {
                                nameC = Name "a",
                                nameHsIdent = HsIdentifier
                                  "struct2_a"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
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
                          structFieldLoc =
                          "macro_in_fundecl_vs_typedef.h:19:30",
                          structFieldName = NamePair {
                            nameC = Name "a",
                            nameHsIdent = HsIdentifier
                              "struct2_a"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
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
                        declHeader =
                        "macro_in_fundecl_vs_typedef.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Struct2"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "macro_in_fundecl_vs_typedef.h:19:30",
                              structFieldName = NamePair {
                                nameC = Name "a",
                                nameHsIdent = HsIdentifier
                                  "struct2_a"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
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
              structFieldLoc =
              "macro_in_fundecl_vs_typedef.h:20:30",
              structFieldName = NamePair {
                nameC = Name "a",
                nameHsIdent = HsIdentifier
                  "struct3_a"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
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
            declHeader =
            "macro_in_fundecl_vs_typedef.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Struct3"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "macro_in_fundecl_vs_typedef.h:20:30",
                  structFieldName = NamePair {
                    nameC = Name "a",
                    nameHsIdent = HsIdentifier
                      "struct3_a"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
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
                  structFieldLoc =
                  "macro_in_fundecl_vs_typedef.h:20:30",
                  structFieldName = NamePair {
                    nameC = Name "a",
                    nameHsIdent = HsIdentifier
                      "struct3_a"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
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
                declHeader =
                "macro_in_fundecl_vs_typedef.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Struct3"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldLoc =
                      "macro_in_fundecl_vs_typedef.h:20:30",
                      structFieldName = NamePair {
                        nameC = Name "a",
                        nameHsIdent = HsIdentifier
                          "struct3_a"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
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
                          structFieldLoc =
                          "macro_in_fundecl_vs_typedef.h:20:30",
                          structFieldName = NamePair {
                            nameC = Name "a",
                            nameHsIdent = HsIdentifier
                              "struct3_a"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
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
                        declHeader =
                        "macro_in_fundecl_vs_typedef.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Struct3"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "macro_in_fundecl_vs_typedef.h:20:30",
                              structFieldName = NamePair {
                                nameC = Name "a",
                                nameHsIdent = HsIdentifier
                                  "struct3_a"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
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
                          structFieldLoc =
                          "macro_in_fundecl_vs_typedef.h:20:30",
                          structFieldName = NamePair {
                            nameC = Name "a",
                            nameHsIdent = HsIdentifier
                              "struct3_a"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
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
                        declHeader =
                        "macro_in_fundecl_vs_typedef.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Struct3"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "macro_in_fundecl_vs_typedef.h:20:30",
                              structFieldName = NamePair {
                                nameC = Name "a",
                                nameHsIdent = HsIdentifier
                                  "struct3_a"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
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
          declHeader =
          "macro_in_fundecl_vs_typedef.h",
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
      newtypeComment = Nothing},
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
              structFieldLoc =
              "macro_in_fundecl_vs_typedef.h:21:30",
              structFieldName = NamePair {
                nameC = Name "a",
                nameHsIdent = HsIdentifier
                  "struct4_a"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
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
            declHeader =
            "macro_in_fundecl_vs_typedef.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Struct4"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "macro_in_fundecl_vs_typedef.h:21:30",
                  structFieldName = NamePair {
                    nameC = Name "a",
                    nameHsIdent = HsIdentifier
                      "struct4_a"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
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
                  structFieldLoc =
                  "macro_in_fundecl_vs_typedef.h:21:30",
                  structFieldName = NamePair {
                    nameC = Name "a",
                    nameHsIdent = HsIdentifier
                      "struct4_a"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
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
                declHeader =
                "macro_in_fundecl_vs_typedef.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Struct4"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldLoc =
                      "macro_in_fundecl_vs_typedef.h:21:30",
                      structFieldName = NamePair {
                        nameC = Name "a",
                        nameHsIdent = HsIdentifier
                          "struct4_a"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
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
                          structFieldLoc =
                          "macro_in_fundecl_vs_typedef.h:21:30",
                          structFieldName = NamePair {
                            nameC = Name "a",
                            nameHsIdent = HsIdentifier
                              "struct4_a"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
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
                        declHeader =
                        "macro_in_fundecl_vs_typedef.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Struct4"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "macro_in_fundecl_vs_typedef.h:21:30",
                              structFieldName = NamePair {
                                nameC = Name "a",
                                nameHsIdent = HsIdentifier
                                  "struct4_a"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
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
                          structFieldLoc =
                          "macro_in_fundecl_vs_typedef.h:21:30",
                          structFieldName = NamePair {
                            nameC = Name "a",
                            nameHsIdent = HsIdentifier
                              "struct4_a"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
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
                        declHeader =
                        "macro_in_fundecl_vs_typedef.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Struct4"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "macro_in_fundecl_vs_typedef.h:21:30",
                              structFieldName = NamePair {
                                nameC = Name "a",
                                nameHsIdent = HsIdentifier
                                  "struct4_a"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
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
  DeclInlineCInclude
    "macro_in_fundecl_vs_typedef.h",
  DeclInlineC
    "void hs_bindgen_test_macro_in_fundecl_vs_typedef_0411223e6a6740c0 (struct2 *arg1, MC arg2) { struct_typedef1(arg1, arg2); }",
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
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_0411223e6a6740c0",
      foreignImportCallConv =
      CallConvUserlandCAPI,
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
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "macro_in_fundecl_vs_typedef.h",
  DeclInlineC
    "void hs_bindgen_test_macro_in_fundecl_vs_typedef_46539ee6ebd5a75d (struct3_t *arg1, MC arg2) { struct_typedef2(arg1, arg2); }",
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
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_46539ee6ebd5a75d",
      foreignImportCallConv =
      CallConvUserlandCAPI,
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
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "macro_in_fundecl_vs_typedef.h",
  DeclInlineC
    "void hs_bindgen_test_macro_in_fundecl_vs_typedef_bac0c4d09acb0d94 (struct4 *arg1, MC arg2) { struct_typedef3(arg1, arg2); }",
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
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_bac0c4d09acb0d94",
      foreignImportCallConv =
      CallConvUserlandCAPI,
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
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "macro_in_fundecl_vs_typedef.h",
  DeclInlineC
    "void hs_bindgen_test_macro_in_fundecl_vs_typedef_8026baca65480b26 (struct struct1 *arg1, MC arg2) { struct_name1(arg1, arg2); }",
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
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_8026baca65480b26",
      foreignImportCallConv =
      CallConvUserlandCAPI,
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
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "macro_in_fundecl_vs_typedef.h",
  DeclInlineC
    "void hs_bindgen_test_macro_in_fundecl_vs_typedef_4923fa8dff338449 (struct struct3 *arg1, MC arg2) { struct_name2(arg1, arg2); }",
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
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_4923fa8dff338449",
      foreignImportCallConv =
      CallConvUserlandCAPI,
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
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "macro_in_fundecl_vs_typedef.h",
  DeclInlineC
    "void hs_bindgen_test_macro_in_fundecl_vs_typedef_cbb77211881a7cdf (struct struct4 *arg1, MC arg2) { struct_name3(arg1, arg2); }",
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
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_macro_in_fundecl_vs_typedef_cbb77211881a7cdf",
      foreignImportCallConv =
      CallConvUserlandCAPI,
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
          commentChildren = []},
      foreignImportSafety = Safe}]
